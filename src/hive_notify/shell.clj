(ns hive-notify.shell
  "Never-throwing shell helpers for the notification backends: a PATH probe and
   a command runner. Both degrade to a failure value instead of throwing."
  (:require [clojure.java.shell :as shell]
            [hive-dsl.result :refer [rescue]])
(:import [java.util.concurrent TimeUnit]))

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

(defn run-timed
  "Run `args` (a vector of strings) with a deadline. Never throws. Returns
   {:exit :out :err :timed-out?}. The process is destroyed when the deadline
   passes or the calling thread is interrupted, so a caller that stops waiting
   leaves no process behind."
  [args timeout-ms]
  (rescue {:exit 127 :out "" :err "exec-failed" :timed-out? false}
    (let [p   (.start (ProcessBuilder. ^java.util.List (mapv str args)))
          out (future (slurp (.getInputStream p)))
          err (future (slurp (.getErrorStream p)))]
      (try
        (if (.waitFor p (long timeout-ms) TimeUnit/MILLISECONDS)
          {:exit (.exitValue p) :out @out :err @err :timed-out? false}
          (do (.destroyForcibly p)
              {:exit -1 :out "" :err "" :timed-out? true}))
        (catch InterruptedException _
          (.destroyForcibly p)
          (.interrupt (Thread/currentThread))
          {:exit -1 :out "" :err "" :timed-out? true})))))
