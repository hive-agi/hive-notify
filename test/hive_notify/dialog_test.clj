(ns hive-notify.dialog-test
  "DialogBackend with injected probes and a recording runner: no display needed."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-spi.notify :as notify]
            [hive-notify.ask :as ask]
            [hive-notify.backends.dialog :as dialog]))

(def ^:private question
  {:summary "allow?" :body "details" :choices [[:yes "Allow"] [:no "Deny"]] :timeout-ms 5000})

(defn- backend
  "A DialogBackend whose PATH holds `bins`, answering every run with `reply`
   and recording [args timeout-ms input] in `calls`."
  [bins reply calls & {:keys [display?] :or {display? true}}]
  (dialog/dialog-backend {:probe    (set bins)
                          :display? (constantly display?)
                          :ask-cmd  (fn [args ms input]
                                      (swap! calls conj [args ms input])
                                      reply)}))

(deftest it-asks-only
  (let [b (backend ["zenity"] {} (atom []))]
    (is (satisfies? ask/IAsk b))
    (is (= :dialog (notify/notify-id b)))
    (is (not (notify/accepts? b :workflow/failed)))
    (is (false? (:delivered? (notify/notify! b {:summary "s"}))))))

(deftest zenity-is-preferred-and-answers-by-label
  (let [calls (atom [])
        b     (backend ["zenity" "rofi" "dmenu"] {:exit 1 :out "Deny\n" :timed-out? false} calls)
        r     (ask/ask! b question)]
    (is (= :no (:answer r)))
    (is (= "zenity" (get-in r [:detail :tool])))
    (let [[args ms input] (first @calls)]
      (is (= "zenity" (first args)))
      (is (= ["--extra-button" "Allow" "--extra-button" "Deny"] (take-last 4 args)))
      (is (= "5" (second (drop-while #(not= "--timeout" %) args))))
      (is (= 5000 ms))
      (is (nil? input)))))

(deftest closing-the-window-is-no-answer
  (let [b (backend ["zenity"] {:exit 1 :out "" :timed-out? false} (atom []))]
    (is (nil? (:answer (ask/ask! b question))))))

(deftest rofi-reads-the-choices-on-stdin-and-answers-by-index
  (let [calls (atom [])
        b     (backend ["rofi"] {:exit 0 :out "0\n" :timed-out? false} calls)]
    (is (= :yes (:answer (ask/ask! b question))))
    (is (= "Allow\nDeny\n" (nth (first @calls) 2)))
    (testing "an escaped menu is no answer"
      (is (nil? (:answer (ask/ask! (backend ["rofi"] {:exit 1 :out "" :timed-out? false} (atom []))
                                   question)))))))

(deftest xmessage-buttons-cannot-be-split-by-a-label
  (let [calls (atom [])
        q     (assoc question :choices [[:yes "Yes, run"] [:no "No: stop"]])
        b     (backend ["xmessage"] {:exit 101 :out "No  stop\n" :timed-out? false} calls)]
    (is (= :no (:answer (ask/ask! b q))))
    (is (some #{"Yes  run,No  stop"} (ffirst @calls)))))

(deftest a-timeout-is-no-answer
  (let [b (backend ["zenity"] {:exit -1 :out "Allow" :timed-out? true} (atom []))]
    (is (nil? (:answer (ask/ask! b question))))))

(deftest unavailable-without-a-display-or-a-program
  (is (false? (notify/backend-available? (backend [] {} (atom [])))))
  (is (false? (notify/backend-available? (backend ["zenity"] {} (atom []) :display? false))))
  (is (= :no-display (get-in (ask/ask! (backend ["zenity"] {} (atom []) :display? false) question)
                             [:detail :reason])))
  (is (= :no-dialog-program (get-in (ask/ask! (backend [] {} (atom [])) question)
                                    [:detail :reason]))))

(deftest an-invalid-question-runs-nothing
  (let [calls (atom [])]
    (is (nil? (:answer (ask/ask! (backend ["zenity"] {} calls) {:summary "" :choices []}))))
    (is (empty? @calls))))
