(ns hive-notify.sink
  "NotifySink — projects a hive-spi.workflow.events WorkflowEvent (or any
   :adt/variant-tagged map) into the INotify notification shape and fans it out.
   Shaped as a 1-arg fn so it can be injected directly as an EvalAlgebra :sink."
  (:require [hive-notify.registry :as registry]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(def ^:private variant->level
  {:workflow/started        :info
   :workflow/step-started   :info
   :workflow/step-completed :info
   :workflow/step-failed    :warn
   :workflow/wave-dispatched :info
   :workflow/wave-completed  :info
   :workflow/completed      :success
   :workflow/failed         :error})

(defn- level->urgency [level]
  (if (= level :error) :critical :normal))

(defn event->notification
  "Project a WorkflowEvent map onto the INotify notification shape."
  [ev]
  (let [variant (:adt/variant ev)
        run-id  (:run-id ev)
        level   (variant->level variant :info)]
    {:event-type variant
     :summary    (str "workflow " run-id " " (some-> variant name))
     :body       (or (some-> (:error ev) pr-str)
                     (some-> (get-in ev [:payload :label]) str)
                     "")
     :urgency    (level->urgency level)
     :level      level}))

(defn notify-sink
  "Return a 1-arg sink `(fn [workflow-event] ...)` that projects the event to a
   notification and fans it out. With no args uses the registry's current
   backends; pass `bs` to target an explicit backend set (tests, custom fanout)."
  ([] (fn [ev] (registry/notify-fanout! (event->notification ev))))
  ([bs] (fn [ev] (registry/notify-fanout! bs (event->notification ev)))))
