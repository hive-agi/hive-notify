(ns hive-notify.shell
  "Never-throwing shell helpers for the notification backends: a PATH probe and
   a command runner. Both degrade to a failure value instead of throwing."
  (:require [clojure.java.shell :as shell]
            [hive-dsl.result :refer [rescue]]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(defn on-path?
  "True when `bin` resolves on PATH (via `command -v`). Never throws."
  [bin]
  (rescue false (zero? (:exit (shell/sh "sh" "-c" (str "command -v " bin))))))

(defn run
  "Run `args` (a vector of strings) through clojure.java.shell/sh. Never throws;
   an exec failure yields {:exit 127 :err <message>}."
  [args]
  (rescue {:exit 127 :err "exec-failed"} (apply shell/sh args)))
