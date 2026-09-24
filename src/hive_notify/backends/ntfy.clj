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
  "ntfy shows at most three action buttons per message; a question with more
   choices spans several messages."
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
   token to `reply-url` and clearing the notification. With `reply-token` each
   action authenticates as its Bearer."
  ([reply-url tokens choices] (actions-header reply-url tokens choices nil))
  ([reply-url tokens choices reply-token]
   (str/join "; "
             (map (fn [[id label]]
                    (str "http, " (label-safe label) ", " reply-url
                         ", method=POST"
                         (when reply-token
                           (str ", headers.Authorization=Bearer " (header-safe reply-token)))
                         ", body=" (get tokens id) ", clear=true"))
                  choices))))

(defn- topic-url [url topic] (str (str/replace (str url) #"/+$" "") "/" topic))

(defn- ok-status? [{:keys [status]}] (and (int? status) (<= 200 status 299)))

(defn http-send
  "Send {:method :get|:post|:delete :url :headers :body} with a 10 s timeout.
   Never throws; a failure yields {:status nil :error msg}. An interrupt keeps
   the thread's interrupt flag set."
  [{:keys [method url headers body]}]
  (rescue {:status nil :error "http-failed"}
    (try
      (let [b    (reduce-kv (fn [^java.net.http.HttpRequest$Builder b k v] (.header b (str k) (str v)))
                            (-> (HttpRequest/newBuilder (URI/create url))
                                (.timeout (Duration/ofSeconds 10)))
                            (or headers {}))
            req  (case method
                   :post   (.build (.POST b (HttpRequest$BodyPublishers/ofString (str body) StandardCharsets/UTF_8)))
                   :delete (.build (.DELETE b))
                   (.build (.GET b)))
            resp (.send (-> (HttpClient/newBuilder) (.connectTimeout (Duration/ofSeconds 10)) .build)
                        req (HttpResponse$BodyHandlers/ofString))]
        {:status (.statusCode resp) :body (.body resp)})
      (catch InterruptedException _
        (.interrupt (Thread/currentThread))
        {:status nil :error "interrupted"}))))

(defn- auth [token] (when token {"Authorization" (str "Bearer " token)}))

(defn- answer-in
  "The choice id whose token appears in `body`, or nil."
  [by-token body]
  (let [s (str body)]
    (some (fn [[tok id]] (when (str/includes? s tok) id)) by-token)))

(defn- page-title
  "`summary`, suffixed (i/n) when the ask spans several messages."
  [summary i n]
  (header-safe (if (= 1 n) summary (str summary " (" (inc i) "/" n ")"))))

(defrecord NtfyBackend [url ask-topic reply-topic notify-topic token reply-token accept?
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

      :else
      (rescue {:answer nil :backend :ntfy :detail {:reason :interrupted}}
        (let [tokens    (into {} (map (fn [[id _]] [id (new-token)])) choices)
              by-token  (into {} (map (fn [[id t]] [t id])) tokens)
              t0        (now-ms)
              deadline  (+ t0 (ask/timeout-ms question))
              reply     (topic-url url reply-topic)
              since     (str "since=" (URLEncoder/encode (str (- (quot t0 1000) 5)) "UTF-8"))
              pages     (vec (partition-all max-choices choices))
              seqs      (vec (repeatedly (count pages) new-token))
              withdraw! (fn []
                          (let [interrupted? (Thread/interrupted)]
                            (doseq [s seqs]
                              (http {:method  :delete
                                     :url     (topic-url url (str ask-topic "/" s))
                                     :headers (auth token)}))
                            (when interrupted? (.interrupt (Thread/currentThread)))))
              pubs      (mapv (fn [i page]
                                (http {:method  :post
                                       :url     (topic-url url ask-topic)
                                       :headers (merge {"Title"         (page-title summary i (count pages))
                                                        "Priority"      "5"
                                                        "Tags"          "lock"
                                                        "X-Sequence-ID" (seqs i)
                                                        "Actions"       (actions-header reply tokens page reply-token)}
                                                       (auth token))
                                       :body    (str body)}))
                              (range) pages)]
          (if-let [failed (first (remove ok-status? pubs))]
            (do (withdraw!)
                {:answer nil :backend :ntfy :detail {:reason :publish-failed
                                                     :status (:status failed)}})
            (try
              (loop []
                (let [r   (http {:method :get :url (str reply "/json?poll=1&" since)
                                 :headers (auth token)})
                      hit (when (ok-status? r) (answer-in by-token (:body r)))]
                  (cond
                    hit                    {:answer hit :backend :ntfy :detail {}}
                    (>= (now-ms) deadline) {:answer nil :backend :ntfy :detail {:reason :timed-out}}
                    :else                  (do (sleep! poll-ms) (recur)))))
              (finally (withdraw!)))))))))

(defn ntfy-backend
  "Build an NtfyBackend. opts: :url, :ask-topic, :reply-topic (all three
   required to ask), :notify-topic, :token (the asker's Bearer), :reply-token
   (the Bearer each action button posts its answer with), :accept? (event-type
   -> bool, default none), :http, :now-ms, :sleep!, :poll-ms."
  [{:keys [url ask-topic reply-topic notify-topic token reply-token accept? http now-ms sleep! poll-ms]
    :or   {accept? (constantly false) http http-send
           now-ms  #(System/currentTimeMillis) sleep! #(Thread/sleep (long %))
           poll-ms default-poll-ms}}]
  (->NtfyBackend url ask-topic reply-topic notify-topic token reply-token accept?
                 http now-ms sleep! poll-ms))

(defn config-file
  "$HIVE_NOTIFY_CONFIG, else $XDG_CONFIG_HOME/hive-notify/config.edn, else
   ~/.config/hive-notify/config.edn."
  []
  (io/file (or (System/getenv "HIVE_NOTIFY_CONFIG")
               (str (or (System/getenv "XDG_CONFIG_HOME")
                        (str (System/getProperty "user.home") "/.config"))
                    "/hive-notify/config.edn"))))

(defn pass-secret
  "The first line of `pass show <path>`, or nil when pass fails, prints
   nothing or does not finish within 30 s. The value is never logged."
  [path]
  (rescue nil
    (let [p (-> (ProcessBuilder. ^java.util.List ["pass" "show" (str path)])
                (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD)
                .start)
          out (future (slurp (.getInputStream p)))]
      (if (and (.waitFor p 30 java.util.concurrent.TimeUnit/SECONDS) (zero? (.exitValue p)))
        (not-empty (str/trim (first (str/split-lines (str (deref out 1000 ""))))))
        (do (.destroyForcibly p) nil)))))

(defonce ^:private pass-cache (atom {}))

(defn- cached-pass-secret
  "pass-secret, remembered per path once it succeeds, so pass (and gpg) run
   once per process rather than once per notification."
  [path]
  (or (get @pass-cache path)
      (when-let [v (pass-secret path)]
        (swap! pass-cache assoc path v)
        v)))

(defn configured-backend
  "An NtfyBackend from the :ntfy map of `file` (default `config-file`), or nil
   when there is none. Credentials are named, never inlined: the asker's token
   comes from :token-pass (a pass entry) or :token-env (an environment
   variable), and the reply buttons' token, if any, from :reply-token-pass or
   :reply-token-env. `secret` resolves a pass entry. Never throws."
  ([] (configured-backend (config-file)))
  ([file] (configured-backend file cached-pass-secret))
  ([file secret]
   (rescue nil
     (when (.isFile (io/file file))
       (let [cfg (:ntfy (edn/read-string (slurp file)))
             env (fn [k] (not-empty (System/getenv (str k))))
             cred (fn [pass-k env-k]
                    (or (some-> (get cfg pass-k) secret)
                        (some-> (get cfg env-k) env)))]
         (when (and (:url cfg) (:ask-topic cfg) (:reply-topic cfg))
           (ntfy-backend (-> (apply dissoc cfg [:token :reply-token :token-env :reply-token-env
                                                :token-pass :reply-token-pass])
                             (assoc :token (cred :token-pass :token-env)
                                    :reply-token (cred :reply-token-pass :reply-token-env))))))))))
