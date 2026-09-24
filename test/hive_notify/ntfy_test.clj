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
  "An NtfyBackend whose poll answers (reply-fn published-actions poll-count),
   where published-actions joins the Actions of every published message."
  [reply-fn & {:as more}]
  (let [calls (atom [])
        clock (atom 0)
        b     (ntfy/ntfy-backend
               (merge cfg
                      {:now-ms #(deref clock)
                       :sleep! #(swap! clock + %)
                       :http   (fn [req]
                                 (swap! calls conj req)
                                 (if (= :get (:method req))
                                   (let [actions (->> @calls
                                                      (filter #(= :post (:method %)))
                                                      (map #(get-in % [:headers "Actions"]))
                                                      (str/join "; "))
                                         n       (count (filter #(= :get (:method %)) @calls))]
                                     {:status 200 :body (str (reply-fn actions n))})
                                   {:status 200 :body "{}"}))}
                      more))]
    [calls b]))

(defn- of-method [calls m] (filterv #(= m (:method %)) calls))

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

(deftest refuses-an-invalid-or-unconfigured-ask-and-never-publishes-it
  (let [[calls b] (backend (constantly ""))]
    (is (= :invalid-question (get-in (ask/ask! b (dissoc question :summary)) [:detail :reason])))
    (is (empty? @calls)))
  (let [b (ntfy/ntfy-backend (dissoc cfg :reply-topic))]
    (is (false? (notify/backend-available? b)))
    (is (= :not-configured (get-in (ask/ask! b question) [:detail :reason])))))

(deftest the-most-choices-a-question-allows-fit-on-three-messages
  (let [[calls b] (backend (constantly ""))
        _         (ask/ask! b (assoc question :timeout-ms 1
                                     :choices (mapv (fn [i] [(keyword (str "c" i)) (str "C" i)]) (range 8))))]
    (is (= [3 3 2] (mapv #(count (str/split (get-in % [:headers "Actions"]) #"; "))
                         (of-method @calls :post))))))

(deftest a-failed-publish-is-no-answer-without-polling
  (let [calls (atom [])
        b     (ntfy/ntfy-backend (assoc cfg :http (fn [req] (swap! calls conj req) {:status 403})))]
    (is (= {:answer nil :backend :ntfy :detail {:reason :publish-failed :status 403}}
           (ask/ask! b question)))
    (is (= 1 (count (of-method @calls :post))))
    (is (empty? (of-method @calls :get)) "never polls")
    (is (= 1 (count (of-method @calls :delete))) "withdraws whatever did land")))

(deftest labels-and-titles-cannot-break-the-header
  (let [h (ntfy/actions-header "u" {:a "t1"} [[:a "Yes, do it; now=1 \"x\"\nnext"]])]
    (is (= "http, Yes do it now 1 x next, u, method=POST, body=t1, clear=true" h)))
  (is (= "caf? ok" (ntfy/header-safe "café\nok"))))

(deftest more-than-three-choices-spread-over-messages
  (let [choices   (mapv (fn [i] [(keyword (str "c" i)) (str "C" i)]) (range 5))
        [calls b] (backend (fn [actions n] (when (= 2 n) (str "{\"message\":\"" (token-of actions "C4") "\"}"))))
        r         (ask/ask! b (assoc question :choices choices))
        pubs      (of-method @calls :post)]
    (is (= :c4 (:answer r)) "a button on the second message answers")
    (is (= ["Unseal request (1/2)" "Unseal request (2/2)"] (mapv #(get-in % [:headers "Title"]) pubs)))
    (is (= [3 2] (mapv #(count (str/split (get-in % [:headers "Actions"]) #"; ")) pubs)))
    (is (apply distinct? (map #(get-in % [:headers "X-Sequence-ID"]) pubs)))
    (is (= (set (map #(str "https://ntfy.example/ask/" (get-in % [:headers "X-Sequence-ID"])) pubs))
           (set (map :url (of-method @calls :delete))))
        "both messages are withdrawn")))

(deftest every-message-is-withdrawn-once-the-ask-ends
  (testing "answered"
    (let [[calls b] (backend (fn [actions _] (str "{\"message\":\"" (token-of actions "Allow") "\"}")))
          _         (ask/ask! b question)
          seq-id    (get-in (first (of-method @calls :post)) [:headers "X-Sequence-ID"])]
      (is (= [(str "https://ntfy.example/ask/" seq-id)] (mapv :url (of-method @calls :delete))))))
  (testing "timed out"
    (let [[calls b] (backend (constantly ""))
          r         (ask/ask! b (assoc question :timeout-ms 1))]
      (is (= :timed-out (get-in r [:detail :reason])))
      (is (= 1 (count (of-method @calls :delete))))))
  (testing "cancelled because another surface answered"
    (let [[calls b] (backend (constantly "") :sleep! (fn [_] (throw (InterruptedException.))))
          r         (ask/ask! b question)]
      (Thread/interrupted)
      (is (nil? (:answer r)))
      (is (= 1 (count (of-method @calls :delete))) "the phone prompt goes away too"))))

(deftest each-principal-authenticates-with-its-own-token
  (let [[calls b] (backend (fn [actions _] (str "{\"message\":\"" (token-of actions "Deny") "\"}"))
                           :token "asker-tk" :reply-token "phone-tk")
        r         (ask/ask! b question)
        actions   (get-in (first (of-method @calls :post)) [:headers "Actions"])]
    (is (= :deny (:answer r)))
    (is (every? #(= "Bearer asker-tk" (get-in % [:headers "Authorization"])) @calls)
        "publish, poll and withdraw all run as the asker")
    (is (= 2 (count (re-seq #"headers\.Authorization=Bearer phone-tk, " actions)))
        "every button posts its answer as the phone's reply token")
    (is (not (str/includes? actions "asker-tk")) "the asker's token never reaches the phone")))

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
    (testing "credentials are named, never read from the file itself"
      (spit f (pr-str {:ntfy (assoc cfg :token-pass "ntfy/token" :reply-token-env "HIVE_NOTIFY_UNSET_VAR"
                                        :token "inline" :reply-token "inline")}))
      (let [asked (atom [])
            b     (ntfy/configured-backend f (fn [path] (swap! asked conj path) "from-pass"))]
        (is (= ["ntfy/token"] @asked))
        (is (= "from-pass" (:token b)))
        (is (nil? (:reply-token b)) "an unset variable is no token, not the inline one")))
    (testing "an env var stands in when no pass entry is named"
      (spit f (pr-str {:ntfy (assoc cfg :token-env "PATH")}))
      (is (= (System/getenv "PATH") (:token (ntfy/configured-backend f (constantly nil))))))
    (testing "a pass entry that yields nothing leaves the ask unauthenticated, not broken"
      (spit f (pr-str {:ntfy (assoc cfg :token-pass "missing")}))
      (let [b (ntfy/configured-backend f (constantly nil))]
        (is (nil? (:token b)))
        (is (notify/backend-available? b))))
    (spit f (pr-str {:ntfy (dissoc cfg :ask-topic)}))
    (is (nil? (ntfy/configured-backend f)))
    (spit f "{:ntfy")
    (is (nil? (ntfy/configured-backend f)) "unreadable config is no backend, not a throw")
    (is (nil? (ntfy/configured-backend (io/file f "missing"))))))
