(ns hive-notify.backends.dialog
  "DialogBackend — asks through a window the person can click: the first
   installed dialog program in `default-tools` (zenity, rofi, xmessage, dmenu).
   It only asks; plain notifications stay with the desktop backend. The PATH
   probe, the display probe and the runner are injected, so it is testable
   without a display and degrades (never throws) when nothing is installed."
  (:require [clojure.string :as str]
            [hive-spi.notify :as notify]
            [hive-notify.ask :as ask]
            [hive-notify.shell :as sh]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(defn- seconds [timeout-ms]
  (str (max 1 (quot (+ (long timeout-ms) 999) 1000))))

(defn- labels [choices]
  (mapv second choices))

(defn- by-label
  "The choice id whose label is `raw`, trimmed, or nil."
  [choices raw]
  (let [s (str/trim (str raw))]
    (some (fn [[id label]] (when (= s label) id)) choices)))

(defn- by-index
  "The choice id at the 0-based index `raw` names, or nil."
  [choices raw]
  (when-let [i (parse-long (str/trim (str raw)))]
    (first (nth choices i nil))))

(def window-title
  "The title every dialog window carries, so a window manager can place it
   (e.g. an xmonad manageHook `title =? \"hive ask\"`)."
  "hive ask")

(def zenity
  "GTK dialog: one button per choice; zenity prints the clicked label."
  {:bin   "zenity"
   :args  (fn [{:keys [summary body choices]} timeout-ms]
            (-> ["zenity" "--question" "--switch" "--no-markup"
                 "--title" window-title
                 "--text" (str summary (when (seq (str body)) (str "\n\n" body)))
                 "--timeout" (seconds timeout-ms)]
                (into (mapcat (fn [l] ["--extra-button" l])) (labels choices))))
   :parse (fn [{:keys [choices]} {:keys [out]}] (by-label choices out))})

(def rofi
  "rofi menu: one row per choice; rofi prints the chosen row's index."
  {:bin   "rofi"
   :args  (fn [{:keys [summary body]} _]
            (cond-> ["rofi" "-dmenu" "-i" "-no-custom" "-format" "i"
                     "-p" (str summary)]
              (seq (str body)) (into ["-mesg" (str body)])))
   :input (fn [{:keys [choices]}] (str (str/join "\n" (labels choices)) "\n"))
   :parse (fn [{:keys [choices]} {:keys [exit out]}]
            (when (zero? exit) (by-index choices out)))})

(defn- xmessage-label
  "A label xmessage's -buttons list can carry: no comma or colon."
  [l]
  (str/replace (str l) #"[,:]" " "))

(def xmessage
  "X11 message box: one button per choice; -print writes the clicked label."
  {:bin   "xmessage"
   :args  (fn [{:keys [summary body choices]} timeout-ms]
            ["xmessage" "-center" "-print" "-timeout" (seconds timeout-ms)
             "-buttons" (str/join "," (map xmessage-label (labels choices)))
             (str summary (when (seq (str body)) (str "\n\n" body)))])
   :parse (fn [{:keys [choices]} {:keys [out]}]
            (let [s (str/trim (str out))]
              (some (fn [[id label]] (when (= s (str/trim (xmessage-label label))) id))
                    choices)))})

(def dmenu
  "dmenu bar: one item per choice; dmenu prints the chosen label."
  {:bin   "dmenu"
   :args  (fn [{:keys [summary]} _] ["dmenu" "-p" (str summary)])
   :input (fn [{:keys [choices]}] (str (str/join "\n" (labels choices)) "\n"))
   :parse (fn [{:keys [choices]} {:keys [exit out]}]
            (when (zero? exit) (by-label choices out)))})

(def default-tools
  "Dialog programs in preference order; the first one on PATH asks."
  [zenity rofi xmessage dmenu])

(defn display?
  "True when the process can reach a graphical display."
  []
  (boolean (or (System/getenv "WAYLAND_DISPLAY") (System/getenv "DISPLAY"))))

(defn- pick-tool [tools probe]
  (some (fn [t] (when (probe (:bin t)) t)) tools))

(defrecord DialogBackend [tools probe display? ask-cmd]
  notify/INotify
  (notify-id [_] :dialog)
  (backend-available? [_]
    (boolean (and (display?) (pick-tool tools probe))))
  (accepts? [_ _] false)
  (notify! [_ _]
    {:delivered? false :backend :dialog :detail {:reason :ask-only}})

  ask/IAsk
  (ask! [_ question]
    (let [tool (pick-tool tools probe)]
      (cond
        (not (ask/valid-question? question))
        {:answer nil :backend :dialog :detail {:reason :invalid-question}}

        (not (display?))
        {:answer nil :backend :dialog :detail {:reason :no-display}}

        (nil? tool)
        {:answer nil :backend :dialog :detail {:reason :no-dialog-program}}

        :else
        (let [ms  (ask/timeout-ms question)
              r   (ask-cmd ((:args tool) question ms) ms
                           (when-let [f (:input tool)] (f question)))]
          {:answer  (when-not (:timed-out? r) ((:parse tool) question r))
           :backend :dialog
           :detail  {:tool (:bin tool) :exit (:exit r) :timed-out? (:timed-out? r)}})))))

(defn dialog-backend
  "Build a DialogBackend. opts (all optional): :tools (default `default-tools`),
   :probe (bin -> bool), :display? (-> bool), :ask-cmd ((fn [args timeout-ms
   input]) -> {:exit :out :timed-out?})."
  ([] (dialog-backend {}))
  ([{:keys [tools probe display? ask-cmd]
     :or   {tools default-tools probe sh/on-path? display? display? ask-cmd sh/run-timed}}]
   (->DialogBackend tools probe display? ask-cmd)))
