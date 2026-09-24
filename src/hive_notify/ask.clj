(ns hive-notify.ask
  "Two-way notifications: put a question with a fixed set of choices in front of
   a person and return the choice they made. `IAsk` is the port a backend
   implements beside INotify; `hive-notify.registry/ask-first!` asks every
   capable backend at once and takes the first answer.

   A question is
     {:summary    \"one line\"            ; required
      :body       \"details\"             ; optional
      :choices    [[:yes \"Yes\"] [:no \"No\"]]
      :timeout-ms 60000}                ; optional
   Choice ids are short unqualified keywords; the answer is one of them, or nil
   when nobody answered in time."
  (:require [clojure.string :as str]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(defprotocol IAsk
  (ask! [backend question]
    "Block until the person answers or the question's :timeout-ms passes.
     Returns {:answer <choice id or nil> :backend <kw> :detail <map>}; a nil
     :answer means no answer. Never throws."))

(def default-timeout-ms 60000)

(defn choice-id?
  "A choice id: an unqualified keyword of lowercase letters, digits and dashes."
  [x]
  (boolean (and (keyword? x) (nil? (namespace x))
                (re-matches #"[a-z0-9][a-z0-9-]{0,31}" (name x)))))

(defn valid-question?
  "True when `question` has a non-blank :summary, 1 to 8 choices with distinct
   valid ids and non-blank labels, and a positive :timeout-ms when given."
  [{:keys [summary choices timeout-ms]}]
  (boolean
   (and (string? summary) (not (str/blank? summary))
        (sequential? choices) (<= 1 (count choices) 8)
        (every? (fn [c] (and (sequential? c) (= 2 (count c))
                             (choice-id? (first c))
                             (string? (second c)) (not (str/blank? (second c)))))
                choices)
        (apply distinct? (map first choices))
        (or (nil? timeout-ms) (pos-int? timeout-ms)))))

(defn timeout-ms
  "The question's :timeout-ms, or `default-timeout-ms`."
  [question]
  (or (:timeout-ms question) default-timeout-ms))

(defn answer-for
  "The choice id `raw` names, or nil when it names none of `choices`."
  [choices raw]
  (let [s (str/trim (str raw))]
    (some (fn [[id _]] (when (= s (name id)) id)) choices)))
