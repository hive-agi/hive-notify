(ns hive-notify.text-ask-test
  "Text questions: the person TYPES the answer. Dialog programs that have an
   input field ask them; button-only backends are skipped by the race."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-spi.notify :as notify]
            [hive-notify.ask :as ask]
            [hive-notify.backends.dialog :as dialog]
            [hive-notify.registry :as registry]))

(def ^:private question
  {:summary "which schema?" :body "v1 has 3 consumers" :input :text :timeout-ms 5000})

(defn- backend
  "A DialogBackend whose PATH holds `bins`, answering every run with `reply`
   and recording [args timeout-ms input] in `calls`."
  [bins reply calls]
  (dialog/dialog-backend {:probe    (set bins)
                          :display? (constantly true)
                          :ask-cmd  (fn [args ms input]
                                      (swap! calls conj [args ms input])
                                      reply)}))

;; =============================================================================
;; Domain
;; =============================================================================

(deftest a-text-question-needs-no-choices
  (is (ask/valid-question? question))
  (is (ask/valid-question? (assoc question :default "v2")))
  (is (not (ask/valid-question? (assoc question :default 7))))
  (is (not (ask/valid-question? (assoc question :summary " "))))
  (is (= :text (ask/question-kind question)))
  (is (= :choice (ask/question-kind {:summary "s" :choices [[:a "A"]]}))))

(deftest a-blank-answer-is-no-answer
  (is (nil? (ask/answer-text "  \n")))
  (is (nil? (ask/answer-text nil)))
  (is (= "v2, keep a v1 view" (ask/answer-text "v2, keep a v1 view\n")))
  (is (= ask/max-text-chars (count (ask/answer-text (apply str (repeat 5000 "x")))))))

(deftest backends-ask-choices-unless-they-say-more
  (is (= #{:choice} (ask/answer-kinds (Object.))))
  (is (= #{:choice :text} (ask/answer-kinds (backend ["zenity"] {} (atom [])))))
  (testing "only the kinds of the programs on PATH count"
    (is (= #{:choice} (ask/answer-kinds (backend ["xmessage"] {} (atom [])))))))

;; =============================================================================
;; Dialog programs
;; =============================================================================

(deftest zenity-opens-an-entry-and-returns-what-was-typed
  (let [calls (atom [])
        r     (ask/ask! (backend ["zenity"] {:exit 0 :out "use v2\n" :timed-out? false} calls)
                        (assoc question :default "v1"))
        [args ms input] (first @calls)]
    (is (= "use v2" (:answer r)))
    (is (= :text (get-in r [:detail :kind])))
    (is (= ["zenity" "--entry"] (take 2 args)))
    (is (= "which schema?\n\nv1 has 3 consumers"
           (second (drop-while #(not= "--text" %) args))))
    (is (= "v1" (second (drop-while #(not= "--entry-text" %) args))))
    (is (= 5000 ms))
    (is (nil? input))))

(deftest cancel-or-timeout-is-no-answer
  (is (nil? (:answer (ask/ask! (backend ["zenity"] {:exit 1 :out "" :timed-out? false} (atom []))
                               question))))
  (is (nil? (:answer (ask/ask! (backend ["zenity"] {:exit 5 :out "half" :timed-out? false} (atom []))
                               question))))
  (is (nil? (:answer (ask/ask! (backend ["zenity"] {:exit 0 :out "x" :timed-out? true} (atom []))
                               question)))))

(deftest xmessage-has-no-input-so-the-next-program-asks
  (let [calls (atom [])
        r     (ask/ask! (backend ["xmessage" "dmenu"] {:exit 0 :out "typed\n" :timed-out? false} calls)
                        question)]
    (is (= "typed" (:answer r)))
    (is (= "dmenu" (ffirst (first @calls))))
    (is (= "" (nth (first @calls) 2)) "dmenu gets an empty list to type into")))

(deftest rofi-and-kdialog-take-free-text
  (let [calls (atom [])]
    (is (= "hi" (:answer (ask/ask! (backend ["rofi"] {:exit 0 :out "hi\n"} calls) question))))
    (is (not-any? #{"-no-custom"} (ffirst @calls))))
  (let [calls (atom [])]
    (is (= "hi" (:answer (ask/ask! (backend ["kdialog"] {:exit 0 :out "hi\n"} calls) question))))
    (is (= "--inputbox" (nth (ffirst @calls) 3)))))

(deftest only-xmessage-installed-cannot-ask-text
  (is (= :no-dialog-program
         (get-in (ask/ask! (backend ["xmessage"] {} (atom [])) question) [:detail :reason]))))

;; =============================================================================
;; The race
;; =============================================================================

(defrecord ButtonsOnly [asked]
  notify/INotify
  (notify-id [_] :buttons)
  (backend-available? [_] true)
  (accepts? [_ _] false)
  (notify! [_ _] {:delivered? false})
  ask/IAsk
  ;; Answers at once: if the race ever asked it, it would win and the test fails.
  (ask! [_ q] (swap! asked conj q) {:answer :never :backend :buttons}))

(deftest the-race-skips-backends-that-cannot-take-text
  (let [asked (atom [])
        r     (registry/ask-first! [(->ButtonsOnly asked)
                                    (backend ["zenity"] {:exit 0 :out "typed\n" :timed-out? false} (atom []))]
                                   question)]
    (is (= "typed" (:answer r)))
    (is (= [:dialog] (:asked r)))
    (is (empty? @asked) "a button-only backend never sees a text question")))
