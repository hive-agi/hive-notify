(ns hive-notify.backends.desktop
  "DesktopBackend — freedesktop notify-send (linux) / osascript (macos). The
   PATH probe and command runner are injected so the backend is testable without
   a live desktop and degrades (never throws) when the tool is absent."
  (:require [hive-spi.notify :as notify]
            [hive-notify.os :as os]
            [hive-notify.shell :as sh]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(def default-accept
  "Event-types the desktop backend opts into by default — terminal workflow
   states only, to avoid per-step notification spam."
  #{:workflow/failed :workflow/completed})

(def ^:private level->urgency
  {:info "normal" :success "normal" :warn "normal" :error "critical"})

(def ^:private level->icon
  {:info "dialog-information" :warn "dialog-warning"
   :error "dialog-error" :success "dialog-information"})

(defn- linux-args [{:keys [summary body level]} app]
  (cond-> ["notify-send" "-a" app
           "-u" (level->urgency level "normal")
           "-i" (level->icon level "dialog-information")
           (str summary)]
    (seq (str body)) (conj (str body))))

(defn- macos-args [{:keys [summary body]}]
  ["osascript" "-e"
   (format "display notification \"%s\" with title \"%s\"" (str body) (str summary))])

(defrecord DesktopBackend [os-kind app accept? probe run-cmd]
  notify/INotify
  (notify-id [_] :desktop)
  (backend-available? [_]
    (boolean (case os-kind
               :linux (probe "notify-send")
               :macos (probe "osascript")
               false)))
  (accepts? [_ event-type] (boolean (accept? event-type)))
  (notify! [_ notification]
    (if-let [args (case os-kind
                    :linux (linux-args notification app)
                    :macos (macos-args notification)
                    nil)]
      (let [{:keys [exit err]} (run-cmd args)]
        {:delivered? (= 0 exit) :backend :desktop :detail {:exit exit :err err}})
      {:delivered? false :backend :desktop :detail {:reason :unsupported-os :os os-kind}})))

(defn desktop-backend
  "Build a DesktopBackend. opts (all optional): :os-kind (default detect-os),
   :app, :accept? (event-type -> bool), :probe, :run-cmd."
  ([] (desktop-backend {}))
  ([{:keys [os-kind app accept? probe run-cmd]
     :or   {os-kind (os/detect-os) app "hive" accept? default-accept
            probe   sh/on-path? run-cmd sh/run}}]
   (->DesktopBackend os-kind app accept? probe run-cmd)))
