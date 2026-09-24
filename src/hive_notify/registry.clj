(ns hive-notify.registry
  "Backend registry + notify-fanout!. Holds the active INotify backends and
   delivers a notification to every backend that is both available AND accepts
   its :event-type. Fail-soft: a backend that is unavailable, filters the event,
   or throws is isolated — fanout never throws and always makes progress."
  (:require [hive-spi.notify :as notify]
            [hive-notify.os :as os]
            [hive-notify.backends.desktop :as desktop]
            [hive-notify.backends.sound :as sound]
            [hive-notify.backends.cli :as cli]
            [hive-notify.backends.widget :as widget]
            [hive-dsl.result :refer [rescue]]
            [hive-notify.ask :as ask]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(defn default-backends
  "The core backend set for `os-kind` (default detect-os): desktop, sound,
   widget, and the always-available cli fallback last."
  ([] (default-backends (os/detect-os)))
  ([os-kind]
   [(desktop/desktop-backend {:os-kind os-kind})
    (sound/sound-backend {:os-kind os-kind})
    (widget/widget-backend)
    (cli/cli-backend)]))

(defonce ^:private backends (atom (default-backends)))

(defn set-backends! [bs] (reset! backends (vec bs)) :ok)
(defn current-backends [] @backends)

(defn- available-and-accepts? [b event-type]
  (and (rescue false (notify/backend-available? b))
       (rescue false (notify/accepts? b event-type))))

(defn- deliver-through [b notification]
  (rescue {:delivered? false
           :backend (rescue :unknown (notify/notify-id b))
           :detail {:reason :notify-threw}}
          (notify/notify! b notification)))

(defn notify-fanout!
  "Deliver `notification` through each backend in `bs` that is available and
   accepts its :event-type. Never throws. Returns
     {:event-type <kw> :attempted [<backend-kw>] :delivered [<backend-kw>]
      :results [<per-backend result map>]}."
  ([notification] (notify-fanout! @backends notification))
  ([bs notification]
   (let [et      (:event-type notification)
         results (into [] (comp (filter #(available-and-accepts? % et))
                                (map #(deliver-through % notification)))
                       bs)]
     {:event-type et
      :attempted  (mapv :backend results)
      :delivered  (mapv :backend (filter :delivered? results))
      :results    results})))

(defn- asker? [b]
  (and (satisfies? ask/IAsk b)
       (rescue false (notify/backend-available? b))))

(defn ask-first!
  "Ask `question` (see hive-notify.ask) through every backend in `bs` that is
   available and can ask, all at once, and return the first answer:
     {:answer <choice id or nil> :backend <backend-kw or nil> :asked [<backend-kw>]}.
   The remaining askers are cancelled once one answers. An invalid question, no
   capable backend, no answer before the question's timeout, or backends that
   all fail give :answer nil. Never throws."
  ([question] (ask-first! @backends question))
  ([bs question]
   (if-not (ask/valid-question? question)
     {:answer nil :backend nil :asked [] :detail {:reason :invalid-question}}
     (let [askers (filterv asker? bs)
           first! (promise)
           futs   (mapv (fn [b]
                          (future
                            (let [r (rescue {:answer nil} (ask/ask! b question))]
                              (when (:answer r) (deliver first! r))
                              r)))
                        askers)
           _      (future (doseq [f futs] (rescue nil @f)) (deliver first! nil))
           r      (deref first! (+ (ask/timeout-ms question) 1000) nil)]
       (doseq [f futs] (future-cancel f))
       {:answer  (:answer r)
        :backend (:backend r)
        :asked   (mapv #(rescue :unknown (notify/notify-id %)) askers)}))))
