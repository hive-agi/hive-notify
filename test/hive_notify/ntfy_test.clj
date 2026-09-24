(ns hive-notify.ntfy-test
  "NtfyBackend over an injected HTTP double and clock: the published actions,
   token matching on the reply topic, and every way to end without an answer."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-spi.notify :as notify]
            [hive-notify.ask :as ask]
            [hive-notify.backends.ntfy :as ntfy]))

(def ^:private question
  {:summary "Unseal request" :body "Agent asks to unseal x. Allow?"
   :choices [[:allow "Allow"] [:deny "Deny"]] :timeout-ms 10000})

(def ^:private cfg
  {:url "https://ntfy.example/" :ask-topic "ask" :reply-topic "reply"})

(defn- token-of
  "The body= token the published Actions header gives `label`."
  [actions label]
  (some (fn [a] (when (str/includes? a (str "http, " label ","))
                  (second (re-find #"body=([0-9a-f]+)" a))))
        (str/split actions #"; ")))

(defn- backend
  "An NtfyBackend whose poll answers (reply-fn published-actions poll-count)."
  [reply-fn & {:as more}]
  (let [calls (atom [])
        clock (atom 0)
        b     (ntfy/ntfy-backend
               (merge cfg
                      {:now-ms #(deref clock)
                       :sleep! #(swap! clock + %)
                       :http   (fn [req]
                                 (swap! calls conj req)
                                 (if (= :post (:method req))
                                   {:status 200 :body "{}"}
                                   (let [actions (get-in (first @calls) [:headers "Actions"])
                                         n       (count (filter #(= :get (:method %)) @calls))]
                                     {:status 200 :body (str (reply-fn actions n))})))}
                      more))]
    [calls b]))

(deftest publishes-one-http-action-per-choice
  (let [[calls b] (backend (constantly ""))
        _         (ask/ask! b (assoc question :timeout-ms 1))
        pub       (first @calls)
        actions   (get-in pub [:headers "Actions"])]
    (is (= "https://ntfy.example/ask" (:url pub)))
    (is (= "Unseal request" (get-in pub [:headers "Title"])))
    (is (= 2 (count (str/split actions #"; "))))
    (is (every? #(str/includes? % "https://ntfy.example/reply, method=POST, body=")
                (str/split actions #"; ")))
    (is (not= (token-of actions "Allow") (token-of actions "Deny")) "each choice has its own token")
    (is (= 32 (count (token-of actions "Allow"))))))

(deftest answers-with-the-choice-whose-token-came-back
  (let [[_ b] (backend (fn [actions n] (when (= 2 n) (str "{\"message\":\"" (token-of actions "Deny") "\"}"))))
        r     (ask/ask! b question)]
    (is (= {:answer :deny :backend :ntfy} (select-keys r [:answer :backend])))))

(deftest a-reply-without-an-issued-token-is-no-answer
  (let [[calls b] (backend (constantly "{\"message\":\"allow\"}{\"message\":\"0123456789abcdef0123456789abcdef\"}"))
        r         (ask/ask! b question)]
    (is (nil? (:answer r)))
    (is (= :timed-out (get-in r [:detail :reason])))
    (is (< 1 (count (filter #(= :get (:method %)) @calls))) "kept polling until the deadline")))

(deftest refuses-what-ntfy-cannot-show-and-never-publishes-it
  (let [[calls b] (backend (constantly ""))]
    (is (= :too-many-choices
           (get-in (ask/ask! b (assoc question :choices [[:a "A"] [:b "B"] [:c "C"] [:d "D"]]))
                   [:detail :reason])))
    (is (= :invalid-question (get-in (ask/ask! b (dissoc question :summary)) [:detail :reason])))
    (is (empty? @calls)))
  (let [b (ntfy/ntfy-backend (dissoc cfg :reply-topic))]
    (is (false? (notify/backend-available? b)))
    (is (= :not-configured (get-in (ask/ask! b question) [:detail :reason])))))

(deftest a-failed-publish-is-no-answer-without-polling
  (let [calls (atom [])
        b     (ntfy/ntfy-backend (assoc cfg :http (fn [req] (swap! calls conj req) {:status 403})))]
    (is (= {:answer nil :backend :ntfy :detail {:reason :publish-failed :status 403}}
           (ask/ask! b question)))
    (is (= 1 (count @calls)))))

(deftest labels-and-titles-cannot-break-the-header
  (let [h (ntfy/actions-header "u" {:a "t1"} [[:a "Yes, do it; now=1 \"x\"\nnext"]])]
    (is (= "http, Yes do it now 1 x next, u, method=POST, body=t1, clear=true" h)))
  (is (= "caf? ok" (ntfy/header-safe "café\nok"))))

(deftest passive-notify-only-with-a-notify-topic
  (let [[calls b] (backend (constantly "") :notify-topic "news" :accept? #{:workflow/failed})]
    (is (notify/accepts? b :workflow/failed))
    (is (not (notify/accepts? b :workflow/completed)))
    (is (:delivered? (notify/notify! b {:summary "boom" :body "x" :level :error})))
    (is (= "https://ntfy.example/news" (:url (first @calls)))))
  (let [[_ b] (backend (constantly ""))]
    (is (not (notify/accepts? b :workflow/failed)))
    (is (= :no-notify-topic (get-in (notify/notify! b {:summary "s"}) [:detail :reason])))))

(deftest configured-backend-reads-the-ntfy-map
  (let [f (doto (io/file (System/getProperty "java.io.tmpdir")
                         (str "hive-notify-cfg-" (System/nanoTime) ".edn"))
            (.deleteOnExit))]
    (spit f (pr-str {:ntfy cfg}))
    (is (notify/backend-available? (ntfy/configured-backend f)))
    (spit f (pr-str {:ntfy (dissoc cfg :ask-topic)}))
    (is (nil? (ntfy/configured-backend f)))
    (spit f "{:ntfy")
    (is (nil? (ntfy/configured-backend f)) "unreadable config is no backend, not a throw")
    (is (nil? (ntfy/configured-backend (io/file f "missing"))))))
