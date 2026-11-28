(ns hive-notify.inbox
  "The local ask queue. GTK presents a snapshot and returns an answer by ID."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [hive-notify.ask :as ask]
            [hive-notify.markup :as markup]
            [hive-notify.shell :as shell]
            [hive-spi.notify :as notify])
  (:import [java.io BufferedReader Writer]
           [java.lang ProcessBuilder$Redirect]
           [java.util.concurrent ArrayBlockingQueue]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(def ^:private startup-ms 2000)
(def ^:private helper-resource "hive_notify/ask_inbox.js")

(defn- launch-gtk []
  (when-let [source (io/resource helper-resource (.getClassLoader (class launch-gtk)))]
    (let [path (java.nio.file.Files/createTempFile
                "hive-ask-inbox-" ".js" (make-array java.nio.file.attribute.FileAttribute 0))
          file (.toFile path)]
      (.deleteOnExit file)
      (try
        (spit file (slurp source :encoding "UTF-8") :encoding "UTF-8")
        (let [process (-> (ProcessBuilder. ^java.util.List ["gjs" (str path)])
                          (.redirectError ProcessBuilder$Redirect/INHERIT)
                          .start)]
          {:from-helper (io/reader (.getInputStream process) :encoding "UTF-8")
           :to-helper (io/writer (.getOutputStream process) :encoding "UTF-8")
           :stop! (fn []
                    (.destroyForcibly process)
                    (java.nio.file.Files/deleteIfExists path))})
        (catch Exception launch-failed
          (java.nio.file.Files/deleteIfExists path)
          (throw launch-failed))))))

(defn- gtk-available? []
  (boolean (and (or (System/getenv "DISPLAY") (System/getenv "WAYLAND_DISPLAY"))
                (shell/on-path? "gjs"))))

(defn manager
  "Create an inbox manager. The default backend shares one manager per JVM."
  ([] (manager {}))
  ([{:keys [launch clock available?]
     :or {launch launch-gtk clock #(System/nanoTime) available? gtk-available?}}]
   {:state (atom {:generation 0 :rows [] :helper nil})
    :launch launch
    :clock clock
    :available? available?}))

(defonce ^:private desktop-manager (manager))

(defn- remaining-ms [clock deadline]
  (let [nanos (unchecked-subtract (long deadline) (long (clock)))]
    (if (pos? nanos) (inc (quot (dec nanos) 1000000)) 0)))

(defn- wire-row [clock {:keys [id deadline question spans]}]
  (let [ms (remaining-ms clock deadline)
        frame {:id id :summary (:summary question) :body spans :remainingMs ms}]
    (when (pos? ms)
      (if (ask/text-question? question)
        (assoc frame :input "text" :default (:default question))
        (assoc frame :choices (mapv (fn [[choice label]] [(name choice) label])
                                    (:choices question)))))))

(defn- signal! [helper]
  (.offer ^ArrayBlockingQueue (:changes helper) true))

(defn- remove-row [rows id]
  (filterv #(not= id (:id %)) rows))

(defn- disconnect! [state helper]
  (let [disconnected?
        (locking state
          (let [{:keys [rows generation] active :helper} @state]
            (when (identical? helper active)
              (reset! state {:rows [] :generation generation :helper nil})
              (doseq [{:keys [result]} rows]
                (deliver result {:status :unavailable}))
              true)))]
    (when disconnected?
      (deliver (:ready helper) false)
      (signal! helper)
      ((:stop! helper)))))

(defn- answer-for [question value]
  (if (ask/text-question? question)
    (ask/answer-text value)
    (ask/answer-for (:choices question) value)))

(defn- accept! [{:keys [state clock]} helper {:keys [id value]}]
  (when (and (string? id) (string? value))
    (locking state
      (let [{:keys [rows] active :helper} @state]
        (when (identical? helper active)
          (when-let [{:keys [question deadline result]}
                     (some #(when (= id (:id %)) %) rows)]
            (when (pos? (remaining-ms clock deadline))
              (when-let [answer (answer-for question value)]
                (swap! state update :rows remove-row id)
                (deliver result {:status :answered :answer answer})
                (signal! helper)))))))))

(defn- read-helper! [{:keys [state] :as inbox} helper]
  (try
    (with-open [reader ^BufferedReader (:from-helper helper)]
      (loop []
        (when-let [line (.readLine reader)]
          (let [frame (try (json/read-str line :key-fn keyword)
                           (catch Exception malformed-frame nil))]
            (case (:type frame)
              "ready" (deliver (:ready helper) true)
              "answer" (accept! inbox helper frame)
              nil))
          (recur))))
    (finally (disconnect! state helper))))

(defn- write-helper! [{:keys [state clock]} helper]
  (try
    (with-open [writer ^Writer (:to-helper helper)]
      (when (deref (:ready helper) startup-ms false)
        (loop []
          (.take ^ArrayBlockingQueue (:changes helper))
          (let [{:keys [rows generation] active :helper} @state]
            (when (identical? helper active)
              (.write writer ^String (json/write-str
                                      {:type "queue" :generation generation
                                       :asks (into [] (keep (partial wire-row clock)) rows)}
                                      :escape-unicode false))
              (.write writer "\n")
              (.flush writer)
              (recur))))))
    (finally (disconnect! state helper))))

(defn- start! [{:keys [state launch] :as inbox}]
  (when-let [pipes (try (launch) (catch Exception helper-unavailable nil))]
    (let [helper (assoc pipes :ready (promise) :changes (ArrayBlockingQueue. 1))]
      (swap! state assoc :helper helper)
      (future (read-helper! inbox helper))
      (future (write-helper! inbox helper))
      helper)))

(defn- register! [{:keys [state clock] :as inbox} row]
  (locking state
    (when-let [helper (or (:helper @state) (start! inbox))]
      (let [{:keys [rows]} @state]
        (swap! state (fn [s]
                       (cond-> (update s :rows conj row)
                         (not-any? #(pos? (remaining-ms clock (:deadline %))) rows)
                         (update :generation inc)))))
      (signal! helper)
      helper)))

(defn- withdraw! [state helper id]
  (locking state
    (when (identical? helper (:helper @state))
      (swap! state update :rows remove-row id)
      (signal! helper))))

(defn- wait-for-answer! [{:keys [state clock] :as inbox} question deadline]
  (let [id (str (random-uuid))
        result (promise)
        row {:id id :deadline deadline :question question :result result
             :spans (markup/spans (:body question))}]
    (if (pos? (remaining-ms clock deadline))
      (if-let [helper (register! inbox row)]
        (try
          (deref result (remaining-ms clock deadline) {:status :timeout})
          (finally (withdraw! state helper id)))
        {:status :unavailable})
      {:status :timeout})))

(defn- unanswered [reason]
  {:backend :dialog :answer nil :detail {:reason reason}})

(defn- fallback? [fallback question]
  (and (notify/backend-available? fallback) (ask/can-ask? fallback question)))

(defn- local-answer! [{:keys [clock available?] :as inbox} fallback question]
  (if-not (ask/valid-question? question)
    (unanswered :invalid-question)
    (let [deadline (unchecked-add (long (clock)) (* 1000000 (long (ask/timeout-ms question))))]
      (try
        (let [result (if (available?)
                       (wait-for-answer! inbox question deadline)
                       {:status :unavailable})
              left (remaining-ms clock deadline)]
          (case (:status result)
            :answered {:backend :dialog :answer (:answer result) :detail {:surface :inbox}}
            :unavailable (if (and (pos? left) (fallback? fallback question))
                           (ask/ask! fallback (assoc question :timeout-ms left))
                           (unanswered :unavailable))
            (unanswered :timed-out)))
        (catch InterruptedException _
          (.interrupt (Thread/currentThread))
          (unanswered :interrupted))
        (catch Exception local-ask-failed
          (unanswered :failed))))))

(defn backend
  "Prefer the shared GTK inbox for local asks and keep the dialog as fallback."
  ([fallback] (backend desktop-manager fallback))
  ([{:keys [available?] :as inbox} fallback]
   (reify
     notify/INotify
     (notify-id [_] :dialog)
     (backend-available? [_] (or (available?) (notify/backend-available? fallback)))
     (accepts? [_ _] false)
     (notify! [_ _] {:delivered? false :backend :dialog :detail {:reason :ask-only}})
     ask/IAskKinds
     (answer-kinds [_] (if (available?) #{:choice :text} (ask/answer-kinds fallback)))
     ask/IAsk
     (ask! [_ question] (local-answer! inbox fallback question)))))
