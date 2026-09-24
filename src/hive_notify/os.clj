(ns hive-notify.os
  "Host OS detection for backend selection."
  (:require [clojure.string :as str]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(defn detect-os
  "Classify the host OS as :linux | :macos | :windows | :unknown from os.name."
  []
  (let [n (some-> (System/getProperty "os.name") str/lower-case)]
    (cond
      (nil? n)                        :unknown
      (str/includes? n "linux")       :linux
      (or (str/includes? n "mac")
          (str/includes? n "darwin")) :macos
      (str/includes? n "windows")     :windows
      :else                           :unknown)))
