(ns hive-notify.os-test
  (:require [clojure.test :refer [deftest is]]
            [hive-notify.os :as os]))

(deftest detect-os-returns-known-kw
  (is (contains? #{:linux :macos :windows :unknown} (os/detect-os))))
