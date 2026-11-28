(ns hive-notify.inbox-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [hive-notify.ask :as ask]
            [hive-notify.inbox :as inbox]
            [hive-notify.registry :as registry]
            [hive-spi.notify :as notify])
  (:import [java.io BufferedReader PipedReader PipedWriter StringWriter]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(defn- emit! [^PipedWriter input frame]
  (.write input ^String (str (json/write-str frame) "\n"))
  (.flush input))

(defn- fake-helper [^PipedWriter input frames]
  {:from-helper (BufferedReader. (PipedReader. input))
   :to-helper (proxy [StringWriter] []
                (flush []
                  (.put ^LinkedBlockingQueue frames
                        (json/read-str (str this) :key-fn keyword))
                  (.setLength (.getBuffer ^StringWriter this) 0)))
   :stop! #(.close input)})

(defn- frame-with [^LinkedBlockingQueue frames row-count]
  (loop [attempts 8]
    (when (pos? attempts)
      (when-let [frame (.poll frames 300 TimeUnit/MILLISECONDS)]
        (if (= row-count (count (:asks frame)))
          frame
          (recur (dec attempts)))))))

(defn- answerer [id f]
  (reify
    notify/INotify
    (notify-id [_] id)
    (backend-available? [_] true)
    (accepts? [_ _] false)
    (notify! [_ _] {})
    ask/IAskKinds
    (answer-kinds [_] #{:choice :text})
    ask/IAsk
    (ask! [_ question] (f question))))

(def ^:private choice
  {:summary "Allow this synthetic request?" :body "Review **this** first."
   :choices [[:allow "Allow"] [:deny "Deny"]] :timeout-ms 8000})

(def ^:private typed
  {:summary "What should the agent do?" :input :text :timeout-ms 8000})

(deftest a-single-helper-queues-concurrent-asks-and-validates-answers
  (with-open [input (PipedWriter.)]
    (let [frames (LinkedBlockingQueue.)
          launches (atom 0)
          peer (fake-helper input frames)
          backend (inbox/backend
                    (inbox/manager {:available? (constantly true)
                                    :launch #(do (swap! launches inc) peer)})
                    (answerer :legacy (constantly {:answer :unexpected})))
          first-result (future (ask/ask! backend choice))]
      (try
        (emit! input {:type "ready"})
        (let [initial (frame-with frames 1)
              second-result (future (ask/ask! backend typed))]
          (try
            (let [both (frame-with frames 2)
                  [a b] (:asks both)]
              (is (= 1 @launches))
              (is (= (:generation initial) (:generation both) 1))
              (is (= [(:summary choice) (:summary typed)] (mapv :summary [a b])))
              (is (= [["allow" "Allow"] ["deny" "Deny"]] (:choices a)))
              (is (= "text" (:input b)))
              (emit! input {:type "answer" :id (:id a) :value "invented"})
              (emit! input {:type "answer" :id (:id b) :value "  line one\nline two  "})
              (is (= "line one\nline two" (:answer (deref second-result 2500 {}))))
              (is (= (:id a) (get-in (frame-with frames 1) [:asks 0 :id])))
              (emit! input {:type "answer" :id (:id b) :value "duplicate"})
              (emit! input {:type "answer" :id (:id a) :value "allow"})
              (is (= :allow (:answer (deref first-result 2500 {}))))
              (is (= [] (:asks (frame-with frames 0)))))
            (finally (future-cancel second-result))))
        (finally (future-cancel first-result))))))

(deftest another-surface-and-interruption-withdraw-their-rows
  (with-open [input (PipedWriter.)]
    (let [frames (LinkedBlockingQueue.)
          peer (fake-helper input frames)
          backend (inbox/backend
                    (inbox/manager {:available? (constantly true) :launch (constantly peer)})
                    (answerer :legacy (constantly {:answer :unexpected})))
          phone-answer (promise)
          phone (answerer :phone (fn [_] @phone-answer))
          race (future (registry/ask-first! [backend phone] choice))]
      (try
        (emit! input {:type "ready"})
        (is (= 1 (:generation (frame-with frames 1))))
        (deliver phone-answer {:backend :phone :answer :deny})
        (is (= :deny (:answer (deref race 2500 {}))))
        (is (= [] (:asks (frame-with frames 0))))
        (let [caller (future (registry/ask-first! [backend] typed))]
          (try
            (is (= 2 (:generation (frame-with frames 1))))
            (is (future-cancel caller))
            (is (= [] (:asks (frame-with frames 0))))
            (finally (future-cancel caller))))
        (finally (future-cancel race))))))

(deftest a-disconnected-helper-leaves-the-original-time-budget
  (with-open [input (PipedWriter.)]
    (let [frames (LinkedBlockingQueue.)
          now (atom 0)
          budget (promise)
          peer (fake-helper input frames)
          backend (inbox/backend
                    (inbox/manager {:available? (constantly true)
                                    :clock #(deref now)
                                    :launch (constantly peer)})
                    (answerer :legacy (fn [question]
                                       (deliver budget (:timeout-ms question))
                                       {:backend :dialog :answer :deny})))
          waiting (future (ask/ask! backend choice))]
      (try
        (emit! input {:type "ready"})
        (is (= 8000 (get-in (frame-with frames 1) [:asks 0 :remainingMs])))
        (reset! now 2500000000)
        (.close input)
        (is (= :deny (:answer (deref waiting 2500 {}))))
        (is (= 5500 (deref budget 2500 nil)))
        (finally (future-cancel waiting))))))

(deftest a-deadline-withdraws-an-unanswered-row
  (with-open [input (PipedWriter.)]
    (let [frames (LinkedBlockingQueue.)
          peer (fake-helper input frames)
          backend (inbox/backend
                    (inbox/manager {:available? (constantly true) :launch (constantly peer)})
                    (answerer :legacy (constantly {:answer :unexpected})))
          waiting (future (ask/ask! backend (assoc choice :timeout-ms 500)))]
      (try
        (emit! input {:type "ready"})
        (is (= 1 (count (:asks (frame-with frames 1)))))
        (is (= :timed-out (get-in (deref waiting 2500 {}) [:detail :reason])))
        (is (= [] (:asks (frame-with frames 0))))
        (finally (future-cancel waiting))))))
