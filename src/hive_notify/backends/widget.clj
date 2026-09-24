(ns hive-notify.backends.widget
  "WidgetBackend — surfaces the notification inside a live Emacs frame. The
   Emacs bridge is reached lazily via requiring-resolve so this lib carries no
   hard Emacs dependency; absent bridge => unavailable, never throws."
  (:require [hive-spi.notify :as notify]
            [hive-dsl.result :refer [rescue]]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(def default-accept
  #{:workflow/failed :workflow/completed})

(defn- resolve-fn [sym] (rescue nil (requiring-resolve sym)))

(defrecord WidgetBackend [accept? bridge-sym]
  notify/INotify
  (notify-id [_] :widget)
  (backend-available? [_] (some? (resolve-fn bridge-sym)))
  (accepts? [_ event-type] (boolean (accept? event-type)))
  (notify! [_ notification]
    (if-let [bridge (resolve-fn bridge-sym)]
      (rescue {:delivered? false :backend :widget :detail {:reason :bridge-threw}}
              (do (bridge notification)
                  {:delivered? true :backend :widget :detail {}}))
      {:delivered? false :backend :widget :detail {:reason :no-emacs-bridge}})))

(defn widget-backend
  "Build a WidgetBackend. opts (optional): :accept?, :bridge-sym — a
   fully-qualified symbol of an Emacs frame-notify fn resolved at call time.
   Defaults to nil (unconfigured => unavailable); supply a real symbol to enable."
  ([] (widget-backend {}))
  ([{:keys [accept? bridge-sym]
     :or   {accept? default-accept}}]
   (->WidgetBackend accept? bridge-sym)))
