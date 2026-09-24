(ns hive-notify.backends-test
  "INotify contract + per-backend behaviour with injected probes/runners."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-spi.notify :as notify]
            [hive-notify.backends.desktop :as desktop]
            [hive-notify.backends.sound :as sound]
            [hive-notify.backends.cli :as cli]
            [hive-notify.backends.widget :as widget]))

(def ^:private recognised #{:desktop :sound :cli :widget})

(deftest all-backends-satisfy-inotify
  (doseq [b [(desktop/desktop-backend) (sound/sound-backend)
             (cli/cli-backend) (widget/widget-backend)]]
    (is (satisfies? notify/INotify b))
    (is (contains? recognised (notify/notify-id b)))))

(deftest desktop-delivers-on-exit-zero
  (let [calls (atom [])
        b     (desktop/desktop-backend {:os-kind :linux :probe (constantly true)
                                        :run-cmd (fn [args] (swap! calls conj args) {:exit 0})})]
    (is (notify/backend-available? b))
    (let [r (notify/notify! b {:event-type :workflow/failed :summary "s" :body "b" :level :error})]
      (is (:delivered? r))
      (is (= :desktop (:backend r)))
      (is (= "notify-send" (ffirst @calls))))))

(deftest desktop-not-delivered-on-nonzero-exit
  (let [b (desktop/desktop-backend {:os-kind :linux :probe (constantly true)
                                    :run-cmd (fn [_] {:exit 1 :err "no dbus"})})]
    (is (false? (:delivered? (notify/notify! b {:summary "s" :level :error}))))))

(deftest desktop-unavailable-when-probe-false
  (is (false? (notify/backend-available?
               (desktop/desktop-backend {:os-kind :linux :probe (constantly false)})))))

(deftest desktop-accepts-only-terminal-by-default
  (let [b (desktop/desktop-backend {:os-kind :linux})]
    (is (notify/accepts? b :workflow/failed))
    (is (notify/accepts? b :workflow/completed))
    (is (not (notify/accepts? b :workflow/step-started)))))

(deftest desktop-unsupported-os-degrades
  (let [b (desktop/desktop-backend {:os-kind :plan9 :probe (constantly true)})]
    (is (false? (notify/backend-available? b)))
    (let [r (notify/notify! b {:summary "s" :level :info})]
      (is (false? (:delivered? r)))
      (is (= :unsupported-os (get-in r [:detail :reason]))))))

(deftest cli-always-available-and-never-throws
  (let [b (cli/cli-backend {:emit (fn [_] (throw (ex-info "boom" {})))})]
    (is (notify/backend-available? b))
    (is (notify/accepts? b :anything/at-all))
    (let [r (notify/notify! b {:summary "s" :level :info})]
      (is (false? (:delivered? r)))
      (is (= :emit-threw (get-in r [:detail :reason]))))))

(deftest cli-delivers-through-emit
  (let [seen (atom nil)
        b    (cli/cli-backend {:emit (fn [n] (reset! seen n))})]
    (is (:delivered? (notify/notify! b {:summary "hi" :level :info})))
    (is (= "hi" (:summary @seen)))))

(deftest widget-unavailable-without-bridge
  (let [b (widget/widget-backend {:bridge-sym 'no.such.ns/missing})]
    (is (false? (notify/backend-available? b)))
    (is (false? (:delivered? (notify/notify! b {:summary "s"}))))))

(deftest sound-player-per-os
  (is (notify/backend-available? (sound/sound-backend {:os-kind :linux :probe (constantly true)})))
  (is (false? (notify/backend-available?
               (sound/sound-backend {:os-kind :unknown :probe (constantly true)})))))
