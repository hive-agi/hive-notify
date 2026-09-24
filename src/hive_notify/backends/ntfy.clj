(ns hive-notify.backends.ntfy
  "NtfyBackend — an ntfy server as a two-way surface, e.g. a phone.
   `ask!` publishes the question to :ask-topic with one http action per choice;
   each action POSTs its own random token to :reply-topic, which is polled for
   the tokens this ask issued. The HTTP call, clock and sleep are injected."
  (:require [hive-spi.notify :as notify]
            [hive-notify.ask :as ask]
            [hive-dsl.result :refer [rescue]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net URI URLEncoder]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.security SecureRandom]
           [java.time Duration]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(def max-choices
  "ntfy shows at most three action buttons."
  3)

(def default-poll-ms 2000)

(defonce ^:private rng (SecureRandom.))

(defn new-token
  "32 hex chars of SecureRandom."
  []
  (let [b (byte-array 16)]
    (.nextBytes ^SecureRandom rng b)
    (apply str (map #(format "%02x" (bit-and % 0xff)) b))))

(defn header-safe
  "`s` on one line of printable ASCII, as HTTP header values must be."
  [s]
  (-> (str s) (str/replace #"[\r\n\t]+" " ") (str/replace #"[^\x20-\x7E]" "?") str/trim))

(defn- label-safe [s]
  (-> (header-safe s) (str/replace #"[,;=\"']" " ") (str/replace #" +" " ") str/trim))

(defn actions-header
  "ntfy Actions header: one http action per choice, POSTing that choice's
   token to `reply-url` and clearing the notification."
  [reply-url tokens choices]
  (str/join "; "
            (map (fn [[id label]]
                   (str "http, " (label-safe label) ", " reply-url
                        ", method=POST, body=" (get tokens id) ", clear=true"))
                 choices)))

(defn- topic-url [url topic] (str (str/replace (str url) #"/+$" "") "/" topic))

(defn- ok-status? [{:keys [status]}] (and (int? status) (<= 200 status 299)))

(defn http-send
  "Send {:method :get|:post :url :headers :body} with a 10 s timeout. Never
   throws; a failure yields {:status nil :error msg}."
  [{:keys [method url headers body]}]
  (rescue {:status nil :error "http-failed"}
    (let [b    (reduce-kv (fn [^java.net.http.HttpRequest$Builder b k v] (.header b (str k) (str v)))
                          (-> (HttpRequest/newBuilder (URI/create url))
                              (.timeout (Duration/ofSeconds 10)))
                          (or headers {}))
          req  (if (= :post method)
                 (.build (.POST b (HttpRequest$BodyPublishers/ofString (str body) StandardCharsets/UTF_8)))
                 (.build (.GET b)))
          resp (.send (-> (HttpClient/newBuilder) (.connectTimeout (Duration/ofSeconds 10)) .build)
                      req (HttpResponse$BodyHandlers/ofString))]
      {:status (.statusCode resp) :body (.body resp)})))

(defn- auth [token] (when token {"Authorization" (str "Bearer " token)}))

(defn- answer-in
  "The choice id whose token appears in `body`, or nil."
  [by-token body]
  (let [s (str body)]
    (some (fn [[tok id]] (when (str/includes? s tok) id)) by-token)))

(defrecord NtfyBackend [url ask-topic reply-topic notify-topic token accept?
                        http now-ms sleep! poll-ms]
  notify/INotify
  (notify-id [_] :ntfy)
  (backend-available? [_] (boolean (and url ask-topic reply-topic)))
  (accepts? [_ event-type] (boolean (and notify-topic (accept? event-type))))
  (notify! [_ {:keys [summary body level]}]
    (if-not notify-topic
      {:delivered? false :backend :ntfy :detail {:reason :no-notify-topic}}
      (let [r (http {:method  :post
                     :url     (topic-url url notify-topic)
                     :headers (merge {"Title"    (header-safe summary)
                                      "Priority" (if (= :error level) "5" "3")}
                                     (auth token))
                     :body    (str body)})]
        {:delivered? (ok-status? r) :backend :ntfy :detail (select-keys r [:status :error])})))

  ask/IAsk
  (ask! [this {:keys [summary body choices] :as question}]
    (cond
      (not (ask/valid-question? question))
      {:answer nil :backend :ntfy :detail {:reason :invalid-question}}

      (not (notify/backend-available? this))
      {:answer nil :backend :ntfy :detail {:reason :not-configured}}

      (> (count choices) max-choices)
      {:answer nil :backend :ntfy :detail {:reason :too-many-choices :max max-choices}}

      :else
      (rescue {:answer nil :backend :ntfy :detail {:reason :interrupted}}
        (let [tokens   (into {} (map (fn [[id _]] [id (new-token)])) choices)
              by-token (into {} (map (fn [[id t]] [t id])) tokens)
              t0       (now-ms)
              deadline (+ t0 (ask/timeout-ms question))
              reply    (topic-url url reply-topic)
              since    (str "since=" (URLEncoder/encode (str (- (quot t0 1000) 5)) "UTF-8"))
              pub      (http {:method  :post
                              :url     (topic-url url ask-topic)
                              :headers (merge {"Title"    (header-safe summary)
                                               "Priority" "5"
                                               "Tags"     "lock"
                                               "Actions"  (actions-header reply tokens choices)}
                                              (auth token))
                              :body    (str body)})]
          (if-not (ok-status? pub)
            {:answer nil :backend :ntfy :detail {:reason :publish-failed
                                                 :status (:status pub)}}
            (loop []
              (let [r   (http {:method :get :url (str reply "/json?poll=1&" since)
                               :headers (auth token)})
                    hit (when (ok-status? r) (answer-in by-token (:body r)))]
                (cond
                  hit                   {:answer hit :backend :ntfy :detail {}}
                  (>= (now-ms) deadline) {:answer nil :backend :ntfy :detail {:reason :timed-out}}
                  :else                 (do (sleep! poll-ms) (recur)))))))))))

(defn ntfy-backend
  "Build an NtfyBackend. opts: :url, :ask-topic, :reply-topic (all three
   required to ask), :notify-topic, :token (Bearer), :accept? (event-type ->
   bool, default none), :http, :now-ms, :sleep!, :poll-ms."
  [{:keys [url ask-topic reply-topic notify-topic token accept? http now-ms sleep! poll-ms]
    :or   {accept? (constantly false) http http-send
           now-ms  #(System/currentTimeMillis) sleep! #(Thread/sleep (long %))
           poll-ms default-poll-ms}}]
  (->NtfyBackend url ask-topic reply-topic notify-topic token accept?
                 http now-ms sleep! poll-ms))

(defn config-file
  "$HIVE_NOTIFY_CONFIG, else $XDG_CONFIG_HOME/hive-notify/config.edn, else
   ~/.config/hive-notify/config.edn."
  []
  (io/file (or (System/getenv "HIVE_NOTIFY_CONFIG")
               (str (or (System/getenv "XDG_CONFIG_HOME")
                        (str (System/getProperty "user.home") "/.config"))
                    "/hive-notify/config.edn"))))

(defn configured-backend
  "An NtfyBackend from the :ntfy map of `file` (default `config-file`), or nil
   when there is none. The :token may be given as :token-env, the name of an
   environment variable. Never throws."
  ([] (configured-backend (config-file)))
  ([file]
   (rescue nil
     (when (.isFile (io/file file))
       (let [{:keys [token-env] :as cfg} (:ntfy (edn/read-string (slurp file)))]
         (when (and (:url cfg) (:ask-topic cfg) (:reply-topic cfg))
           (ntfy-backend (cond-> (dissoc cfg :token-env)
                           token-env (assoc :token (System/getenv (str token-env)))))))))))
