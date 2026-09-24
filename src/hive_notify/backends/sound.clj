(ns hive-notify.backends.sound
  "SoundBackend — plays an audible cue via paplay/aplay (linux) or afplay
   (macos). Player and PATH probe are injected for testability; never throws."
  (:require [hive-spi.notify :as notify]
            [hive-notify.os :as os]
            [hive-notify.shell :as sh]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(def default-accept
  "Sound fires only on failure by default — the one event worth an audible cue."
  #{:workflow/failed})

(def ^:private os->player
  {:linux "paplay" :macos "afplay"})

(defrecord SoundBackend [os-kind player accept? probe run-cmd]
  notify/INotify
  (notify-id [_] :sound)
  (backend-available? [_] (boolean (and player (probe player))))
  (accepts? [_ event-type] (boolean (accept? event-type)))
  (notify! [_ notification]
    (if player
      (let [{:keys [exit err]} (run-cmd [player (str (:sound notification ""))])]
        {:delivered? (= 0 exit) :backend :sound :detail {:exit exit :err err}})
      {:delivered? false :backend :sound :detail {:reason :no-player :os os-kind}})))

(defn sound-backend
  "Build a SoundBackend. opts (optional): :os-kind, :player (default per OS),
   :accept?, :probe, :run-cmd."
  ([] (sound-backend {}))
  ([{:keys [os-kind player accept? probe run-cmd]
     :or   {os-kind (os/detect-os) accept? default-accept
            probe   sh/on-path? run-cmd sh/run}}]
   (->SoundBackend os-kind (or player (os->player os-kind)) accept? probe run-cmd)))
