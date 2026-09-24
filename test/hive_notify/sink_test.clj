(ns hive-notify.sink-test
  "NotifySink projection + fan-out."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-notify.sink :as sink]
            [hive-notify.backends.cli :as cli]))

(deftest event->notification-projection
  (testing ":workflow/failed -> :error/:critical carrying the error body"
    (let [n (sink/event->notification {:adt/variant :workflow/failed :run-id "r7"
                                       :error {:error :boom} :payload {}})]
      (is (= :workflow/failed (:event-type n)))
      (is (= :error (:level n)))
      (is (= :critical (:urgency n)))
      (is (re-find #"r7" (:summary n)))
      (is (re-find #"boom" (:body n)))))
  (testing ":workflow/completed -> :success/:normal with payload label body"
    (let [n (sink/event->notification {:adt/variant :workflow/completed :run-id "r"
                                       :payload {:label "done"}})]
      (is (= :success (:level n)))
      (is (= :normal (:urgency n)))
      (is (= "done" (:body n))))))

(deftest notify-sink-projects-and-fans-out
  (testing "the sink fn projects a WorkflowEvent and delivers via the backend set"
    (let [seen (atom nil)
          sfn  (sink/notify-sink [(cli/cli-backend {:emit (fn [n] (reset! seen n))})])
          r    (sfn {:adt/variant :workflow/failed :run-id "r9" :error {:error :x} :payload {}})]
      (is (= [:cli] (:delivered r)))
      (is (= :workflow/failed (:event-type @seen)))
      (is (= :error (:level @seen))))))
