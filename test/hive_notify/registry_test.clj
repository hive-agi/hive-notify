(ns hive-notify.registry-test
  "notify-fanout! proof tests: the M6 acceptance criteria — :workflow/failed
   reaches desktop+sound; headless/no-DBus degrades to cli and never throws;
   a throwing backend is isolated."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-spi.notify :as notify]
            [hive-notify.registry :as registry]
            [hive-notify.backends.desktop :as desktop]
            [hive-notify.backends.sound :as sound]
            [hive-notify.backends.cli :as cli]))

(defn- ok-desktop []
  (desktop/desktop-backend {:os-kind :linux :probe (constantly true)
                            :run-cmd (fn [_] {:exit 0})}))

(defn- ok-sound []
  (sound/sound-backend {:os-kind :linux :probe (constantly true)
                        :run-cmd (fn [_] {:exit 0})}))

(defn- silent-cli [] (cli/cli-backend {:emit (fn [_] nil)}))

(deftest workflow-failed-fans-to-desktop-and-sound
  (testing ":workflow/failed reaches desktop + sound (+ cli fallback)"
    (let [r (registry/notify-fanout!
             [(ok-desktop) (ok-sound) (silent-cli)]
             {:event-type :workflow/failed :summary "boom" :level :error})]
      (is (= #{:desktop :sound :cli} (set (:delivered r)))))))

(deftest non-terminal-event-filtered-from-desktop-and-sound
  (testing ":workflow/step-started filtered from desktop+sound; cli still takes it"
    (let [r (registry/notify-fanout!
             [(ok-desktop) (ok-sound) (silent-cli)]
             {:event-type :workflow/step-started :summary "x" :level :info})]
      (is (= [:cli] (:delivered r)))
      (is (not-any? #{:desktop :sound} (:attempted r))))))

(deftest headless-no-dbus-degrades-to-cli-and-never-throws
  (testing "desktop+sound unavailable (probe false) => only cli delivers"
    (let [r (registry/notify-fanout!
             [(desktop/desktop-backend {:os-kind :linux :probe (constantly false)})
              (sound/sound-backend {:os-kind :linux :probe (constantly false)})
              (silent-cli)]
             {:event-type :workflow/failed :summary "boom" :level :error})]
      (is (= [:cli] (:delivered r))))))

(deftest throwing-notify-is-isolated
  (testing "a backend whose notify! throws does not abort the fanout"
    (let [bad (reify notify/INotify
                (notify-id [_] :desktop)
                (backend-available? [_] true)
                (accepts? [_ _] true)
                (notify! [_ _] (throw (ex-info "boom" {}))))
          r   (registry/notify-fanout! [bad (silent-cli)]
                                       {:event-type :workflow/failed :summary "s" :level :error})]
      (is (= [:cli] (:delivered r)))
      (is (some #(= :notify-threw (get-in % [:detail :reason])) (:results r))))))

(deftest throwing-probe-is-isolated
  (testing "a backend whose backend-available? throws is skipped, not fatal"
    (let [bad (reify notify/INotify
                (notify-id [_] :sound)
                (backend-available? [_] (throw (ex-info "probe boom" {})))
                (accepts? [_ _] true)
                (notify! [_ _] {:delivered? true :backend :sound :detail {}}))
          r   (registry/notify-fanout! [bad (silent-cli)]
                                       {:event-type :workflow/failed :summary "s" :level :error})]
      (is (= [:cli] (:delivered r))))))
