;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors
;;
;; Compile the Hive Ask Inbox GTK4/GJS presentation from ClojureScript into the
;; single self-contained resource hive_notify/ask_inbox.js.
;;
;; Reproduce with:
;;   clojure -M:cljs -i script/compile_ask_inbox.clj
;;
;; (or, without the :cljs alias in deps.edn:)
;;   clojure -Sdeps '{:deps {org.clojure/clojurescript {:mvn/version "1.12.134"}}}' \
;;           -M -i script/compile_ask_inbox.clj
;;
;; Why :simple and not :advanced: the bundle must be ONE file with no module
;; loader (gjs has none), and :advanced renames property names -- including the
;; GI handles (`imports.gi` -> `imports.pb`) and every `.method()` on a GI
;; object -- which gjs cannot survive without a hand-written extern surface.
;; :simple concatenates without renaming, so string-keyed GI access stays intact.
;; The output is checked in; the production classpath never needs ClojureScript
;; at runtime, and the helper is launched with `gjs <file>` (or `gjs -c <src>`).

(require '[cljs.build.api :as b]
         '[clojure.java.io :as io])

(def ^:private src-dir "src-cljs")
(def ^:private out-dir "target/cljs-ask-inbox")
(def ^:private out-file "src/hive_notify/ask_inbox.js")

(b/build src-dir
         {:main 'hive-notify.ask-inbox
          :output-to out-file
          :output-dir out-dir
          :optimizations :simple
          :pretty-print false})

(println (format "compiled %s -> %s (%d bytes)"
                 src-dir out-file (.length (io/file out-file))))
(System/exit 0)
