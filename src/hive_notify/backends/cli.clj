(ns hive-notify.backends.cli
  "CliBackend — the always-available, never-throw fallback. Writes the
   notification to an injected emit fn (default: stderr). Always available;
   accepts every event-type unless a filter is supplied."
  (:require [hive-spi.notify :as notify]
            [hive-dsl.result :refer [rescue]]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(defn- default-emit [{:keys [level summary body]}]
  (binding [*out* *err*]
    (println (str "[notify/" (name (or level :info)) "] " summary
                  (when (seq (str body)) (str " — " body))))))

(defrecord CliBackend [accept? emit]
  notify/INotify
  (notify-id [_] :cli)
  (backend-available? [_] true)
  (accepts? [_ event-type] (boolean (accept? event-type)))
  (notify! [_ notification]
    (rescue {:delivered? false :backend :cli :detail {:reason :emit-threw}}
            (do (emit notification)
                {:delivered? true :backend :cli :detail {}}))))

(defn cli-backend
  "Build a CliBackend. opts (optional): :accept? (default accept-all),
   :emit (default: one stderr line)."
  ([] (cli-backend {}))
  ([{:keys [accept? emit] :or {accept? (constantly true) emit default-emit}}]
   (->CliBackend accept? emit)))
