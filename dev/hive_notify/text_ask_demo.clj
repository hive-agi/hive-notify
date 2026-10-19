(ns hive-notify.text-ask-demo
  "Live demo: put a typed-answer question on this display through the backend
   race and print what came back.

     clojure -Sdeps '{:aliases {:dev {:extra-paths [\"dev\"]}}}' -M:dev -m hive-notify.text-ask-demo"
  (:require [hive-notify.registry :as registry]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(def question
  {:summary    "ling-a asks: which schema should the migration target?"
   :body       "Context: v1 has 3 consumers; v2 has none yet but is the plan."
   :input      :text
   :timeout-ms 180000})

(defn -main [& _]
  (prn (registry/ask-first! question))
  (shutdown-agents))
