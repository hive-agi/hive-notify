(ns hive-notify.backends.desktop
  "DesktopBackend — freedesktop notify-send (linux) / osascript (macos). The
   PATH probe and command runner are injected so the backend is testable without
   a live desktop and degrades (never throws) when the tool is absent."
  (:require [hive-spi.notify :as notify]
            [hive-notify.os :as os]
            [hive-notify.shell :as sh]
            [hive-notify.ask :as ask]
            [clojure.string :as str]
            [hive-notify.backends.freedesktop :as fd]))

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

(defn escape-markup
  "Escape the characters notification servers read as body markup, so caller
   text shows as written and cannot render links or formatting."
  [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(defn- linux-ask-args
  "notify-send with one action button per choice; it waits and prints the chosen
   action's id. `--` ends the options, so a summary starting with `-` stays text,
   and the body is markup-escaped."
  [{:keys [summary body choices] :as question} app]
  (-> ["notify-send" "-a" app "-u" "critical" "-t" (str (ask/timeout-ms question))]
      (into (mapcat (fn [[id label]] ["-A" (str (name id) "=" label)])) choices)
      (conj "--" (str summary))
      (cond-> (seq (str body)) (conj (escape-markup body)))))

(defn notify-send-bus
  "Ask projection over notify-send, run through `ask-cmd`
   ((fn [args timeout-ms]) -> {:exit :out :timed-out?})."
  [ask-cmd]
  (fn [question app timeout-ms]
    (let [{:keys [exit out timed-out?]} (ask-cmd (linux-ask-args question app) timeout-ms)]
      (cond
        timed-out?   {:status :timed-out}
        (= 127 exit) {:status :unavailable}
        (= 0 exit)   {:status :answered :raw out}
        :else        {:status :failed :exit exit}))))

(defn dbus-bus
  "Ask projection over the freedesktop Notifications service on the session
   D-Bus, in-process."
  [question app timeout-ms]
  (fd/ask! (update question :body escape-markup) app timeout-ms))

(defn- ask-over
  "Ask through the first projection in `buses` ([[id f] ...]) that is not
   :unavailable. Returns [id status-map]."
  [buses question app timeout-ms]
  (or (some (fn [[id f]]
              (let [r (f question app timeout-ms)]
                (when (not= :unavailable (:status r)) [id r])))
            buses)
      [nil {:status :unavailable}]))

(defrecord DesktopBackend [os-kind app accept? probe run-cmd ask-buses]
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
      {:delivered? false :backend :desktop :detail {:reason :unsupported-os :os os-kind}}))

  ask/IAsk
  (ask! [_ question]
    (cond
      (not (ask/valid-question? question))
      {:answer nil :backend :desktop :detail {:reason :invalid-question}}

      (not= :linux os-kind)
      {:answer nil :backend :desktop :detail {:reason :unsupported-os :os os-kind}}

      :else
      (let [[bus r] (ask-over ask-buses question app (ask/timeout-ms question))]
        {:answer  (when (= :answered (:status r))
                    (ask/answer-for (:choices question) (:raw r)))
         :backend :desktop
         :detail  (-> r (dissoc :raw) (assoc :bus bus))}))))

(defn desktop-backend
  "Build a DesktopBackend. opts (all optional): :os-kind (default detect-os),
   :app, :accept? (event-type -> bool), :probe, :run-cmd, :ask-buses
   ([[id (fn [question app timeout-ms])] ...], tried in order; default D-Bus
   then notify-send), :ask-cmd ((fn [args timeout-ms]) -> {:exit :out
   :timed-out?}). An :ask-cmd given without :ask-buses means notify-send only."
  ([] (desktop-backend {}))
  ([{:keys [os-kind app accept? probe run-cmd ask-cmd ask-buses]
     :or   {os-kind (os/detect-os) app "hive" accept? default-accept
            probe   sh/on-path? run-cmd sh/run}}]
   (->DesktopBackend os-kind app accept? probe run-cmd
                     (or ask-buses
                         (if ask-cmd
                           [[:notify-send (notify-send-bus ask-cmd)]]
                           [[:dbus dbus-bus]
                            [:notify-send (notify-send-bus sh/run-timed)]])))))
