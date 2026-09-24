(ns hive-notify.ask-test
  "Two-way asks: question validation, the desktop asker over an injected
   runner, the timed runner against real processes, and ask-first! over fakes."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-spi.notify :as notify]
            [hive-notify.ask :as ask]
            [hive-notify.backends.desktop :as desktop]
            [hive-notify.registry :as registry]
            [hive-notify.shell :as sh]))

(def ^:private question
  {:summary "Deploy now?" :body "staging -> prod" :choices [[:yes "Yes"] [:no "No"]]
   :timeout-ms 500})

(deftest questions-are-validated
  (is (ask/valid-question? question))
  (doseq [[why q] [["blank summary"   (assoc question :summary " ")]
                   ["no choices"      (assoc question :choices [])]
                   ["qualified id"    (assoc question :choices [[:a/yes "Yes"]])]
                   ["id with ="       (assoc question :choices [[(keyword "a=b") "x"]])]
                   ["duplicate ids"   (assoc question :choices [[:yes "Y"] [:yes "Z"]])]
                   ["blank label"     (assoc question :choices [[:yes ""]])]
                   ["bad timeout"     (assoc question :timeout-ms 0)]]]
    (is (not (ask/valid-question? q)) why)))

(deftest answers-must-name-a-choice
  (is (= :yes (ask/answer-for (:choices question) "yes\n")))
  (is (nil? (ask/answer-for (:choices question) "")))
  (is (nil? (ask/answer-for (:choices question) "maybe"))))

(defn- asker [out & {:as more}]
  (let [calls (atom [])]
    [calls (desktop/desktop-backend
            (merge {:os-kind :linux :probe (constantly true)
                    :ask-cmd (fn [args t] (swap! calls conj [args t]) out)}
                   more))]))

(deftest desktop-asks-with-one-button-per-choice
  (let [[calls b] (asker {:exit 0 :out "no\n" :timed-out? false})
        r         (ask/ask! b (assoc question :summary "-A evil=Evil"))
        [args t]  (first @calls)]
    (is (= :no (:answer r)))
    (is (= 500 t))
    (is (= ["-A" "yes=Yes" "-A" "no=No"] (->> args (partition 2 1) (filter #(= "-A" (first %))) (mapcat identity) vec)))
    (is (= ["--" "-A evil=Evil" "staging -&gt; prod"] (subvec args (- (count args) 3)))
        "the text comes after --, so it can never be read as an option")))

(deftest desktop-escapes-body-markup
  (let [[calls b] (asker {:exit 0 :out "yes" :timed-out? false})]
    (ask/ask! b (assoc question :body "<a href=\"x\">click</a> & <b>go</b>"))
    (is (= "&lt;a href=\"x\"&gt;click&lt;/a&gt; &amp; &lt;b&gt;go&lt;/b&gt;"
           (peek (first (first @calls)))))))

(deftest desktop-gives-no-answer-unless-a-choice-came-back
  (doseq [[why out] [["timed out"  {:exit -1 :out "" :timed-out? true}]
                     ["dismissed"  {:exit 0 :out "" :timed-out? false}]
                     ["failed"     {:exit 1 :out "yes" :timed-out? false}]
                     ["stray text" {:exit 0 :out "sure" :timed-out? false}]]]
    (is (nil? (:answer (ask/ask! (second (asker out)) question))) why))
  (testing "invalid question and unsupported os never run anything"
    (let [[calls b] (asker {:exit 0 :out "yes"})]
      (is (= :invalid-question (get-in (ask/ask! b (dissoc question :choices)) [:detail :reason])))
      (is (empty? @calls)))
    (let [[calls b] (asker {:exit 0 :out "yes"} :os-kind :macos)]
      (is (= :unsupported-os (get-in (ask/ask! b question) [:detail :reason])))
      (is (empty? @calls)))))

(deftest run-timed-kills-at-the-deadline
  (let [t0 (System/currentTimeMillis)
        r  (sh/run-timed ["sleep" "5"] 200)]
    (is (:timed-out? r))
    (is (< (- (System/currentTimeMillis) t0) 2000)))
  (let [r (sh/run-timed ["echo" "hi"] 5000)]
    (is (= 0 (:exit r)))
    (is (= "hi\n" (:out r)))
    (is (false? (:timed-out? r))))
  (is (= 127 (:exit (sh/run-timed ["/nonexistent/binary"] 1000)))))

(defn- fake [id available? answer-fn]
  (reify
    notify/INotify
    (notify-id [_] id)
    (backend-available? [_] available?)
    (accepts? [_ _] true)
    (notify! [_ _] {:delivered? false :backend id})
    ask/IAsk
    (ask! [_ q] (answer-fn q))))

(deftest ask-first-takes-the-first-answer-and-cancels-the-rest
  (let [slow-cancelled (atom false)
        slow (fake :slow true (fn [_] (try (Thread/sleep 5000) {:answer :no :backend :slow}
                                           (catch InterruptedException _ (reset! slow-cancelled true)
                                             {:answer nil}))))
        fast (fake :fast true (fn [_] (Thread/sleep 50) {:answer :yes :backend :fast}))
        t0   (System/currentTimeMillis)
        r    (registry/ask-first! [slow fast] (assoc question :timeout-ms 5000))]
    (is (= {:answer :yes :backend :fast} (select-keys r [:answer :backend])))
    (is (= #{:slow :fast} (set (:asked r))))
    (is (< (- (System/currentTimeMillis) t0) 2000) "does not wait for the slow asker")
    (Thread/sleep 100)
    (is @slow-cancelled "the slow asker was interrupted")))

(deftest ask-first-without-an-answer-is-nil-and-never-throws
  (testing "nobody can ask: returns at once"
    (let [t0 (System/currentTimeMillis)
          r  (registry/ask-first! [(fake :off false (fn [_] {:answer :yes}))] question)]
      (is (nil? (:answer r)))
      (is (empty? (:asked r)))
      (is (< (- (System/currentTimeMillis) t0) 400))))
  (testing "askers that throw or decline"
    (let [r (registry/ask-first! [(fake :boom true (fn [_] (throw (ex-info "boom" {}))))
                                  (fake :nope true (fn [_] {:answer nil :backend :nope}))]
                                 question)]
      (is (nil? (:answer r)))
      (is (= #{:boom :nope} (set (:asked r))))))
  (testing "timeout"
    (let [r (registry/ask-first! [(fake :mute true (fn [_] (Thread/sleep 3000) {:answer :yes}))]
                                 (assoc question :timeout-ms 100))]
      (is (nil? (:answer r)))))
  (testing "invalid question asks nobody"
    (let [called (atom false)
          r      (registry/ask-first! [(fake :x true (fn [_] (reset! called true) {:answer :yes}))]
                                      (dissoc question :summary))]
      (is (nil? (:answer r)))
      (is (not @called)))))
