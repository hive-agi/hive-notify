(ns hive-notify.ask
  "Two-way notifications: put a question in front of a person and return what
   they answered. `IAsk` is the port a backend implements beside INotify;
   `hive-notify.registry/ask-first!` asks every capable backend at once and
   takes the first answer.

   A choice question is
     {:summary    \"one line\"            ; required
      :body       \"details\"             ; optional
      :choices    [[:yes \"Yes\"] [:no \"No\"]]
      :timeout-ms 60000}                ; optional
   Choice ids are short unqualified keywords; the answer is one of them, or nil
   when nobody answered in time.

   A text question asks the person to TYPE the answer:
     {:summary    \"one line\"            ; required
      :body       \"context to read\"     ; optional
      :input      :text
      :default    \"prefilled\"           ; optional
      :timeout-ms 60000}                ; optional
   Its answer is the typed string, trimmed, or nil when nothing was typed or
   nobody answered in time. A backend that cannot take typed input answers
   nil at once, so the race falls through to one that can."
  (:require [clojure.string :as str]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(defprotocol IAsk
  (ask! [backend question]
    "Block until the person answers or the question's :timeout-ms passes.
     Returns {:answer <choice id, typed string, or nil> :backend <kw> :detail
     <map>}; a nil :answer means no answer. Never throws."))

(def default-timeout-ms 60000)

(def max-text-chars
  "The longest typed answer kept; longer ones are cut here."
  4000)

(defn text-question?
  "True when `question` asks for a typed answer rather than a choice."
  [question]
  (= :text (:input question)))

(defn question-kind
  "The kind of answer `question` asks for: :text (typed) or :choice. Backends
   dispatch on it."
  [question]
  (if (text-question? question) :text :choice))

(defprotocol IAskKinds
  (answer-kinds [backend]
    "The question kinds (see `question-kind`) this backend can ask. A backend
     that does not extend this asks :choice questions only."))

(extend-protocol IAskKinds
  Object
  (answer-kinds [_] #{:choice}))

(defn can-ask?
  "True when `backend` can ask a question of `question`'s kind."
  [backend question]
  (contains? (answer-kinds backend) (question-kind question)))

(defn choice-id?
  "A choice id: an unqualified keyword of lowercase letters, digits and dashes."
  [x]
  (boolean (and (keyword? x) (nil? (namespace x))
                (re-matches #"[a-z0-9][a-z0-9-]{0,31}" (name x)))))

(defn- valid-summary-and-timeout? [{:keys [summary timeout-ms]}]
  (and (string? summary) (not (str/blank? summary))
       (or (nil? timeout-ms) (pos-int? timeout-ms))))

(defn valid-question?
  "True when `question` has a non-blank :summary and a positive :timeout-ms
   when given, and either asks for text (:input :text, an optional string
   :default) or has 1 to 8 choices with distinct valid ids and non-blank
   labels."
  [{:keys [choices default] :as question}]
  (boolean
   (and (valid-summary-and-timeout? question)
        (if (text-question? question)
          (or (nil? default) (string? default))
          (and (sequential? choices) (<= 1 (count choices) 8)
               (every? (fn [c] (and (sequential? c) (= 2 (count c))
                                    (choice-id? (first c))
                                    (string? (second c)) (not (str/blank? (second c)))))
                       choices)
               (apply distinct? (map first choices)))))))

(defn timeout-ms
  "The question's :timeout-ms, or `default-timeout-ms`."
  [question]
  (or (:timeout-ms question) default-timeout-ms))

(defn answer-for
  "The choice id `raw` names, or nil when it names none of `choices`."
  [choices raw]
  (let [s (str/trim (str raw))]
    (some (fn [[id _]] (when (= s (name id)) id)) choices)))

(defn answer-text
  "The typed answer in `raw`: trimmed and cut to `max-text-chars`, or nil when
   blank. A blank answer must be nil: the race takes the first truthy answer,
   and an empty string would beat a person still typing elsewhere."
  [raw]
  (let [s (str/trim (str raw))]
    (when-not (str/blank? s)
      (subs s 0 (min (count s) max-text-chars)))))

