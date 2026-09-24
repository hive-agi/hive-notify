;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; Load every namespace under src/ and exit non-zero if any of them fails.
;;
;; Contract: run with NO -Sdeps override, so the classpath is the one deps.edn
;; declares and every sibling resolves from the registry as published.
;;
;;   clojure -M -i script/load_all.clj

(require '[clojure.string :as str])

(defn- path->ns
  [path]
  (-> path
      (str/replace #"^src/" "")
      (str/replace #"\.cljc?$" "")
      (str/replace "_" "-")
      (str/replace "/" ".")
      symbol))

(let [nss   (->> (file-seq (java.io.File. "src"))
                 (filter #(.isFile ^java.io.File %))
                 (map #(.getPath ^java.io.File %))
                 (filter #(re-find #"\.cljc?$" %))
                 (map path->ns)
                 sort)
      fails (atom [])]
  (doseq [n nss]
    (try
      (require n)
      (catch Throwable t
        (swap! fails conj [n (first (str/split-lines (str (.getMessage t))))]))))
  (println)
  (println (format "namespaces: %d  failed: %d" (count nss) (count @fails)))
  (doseq [[n m] @fails]
    (println (format "  FAIL %s -> %s" n m)))
  (flush)
  (System/exit (if (seq @fails) 1 0)))
