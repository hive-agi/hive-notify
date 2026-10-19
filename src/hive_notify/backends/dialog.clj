(ns hive-notify.backends.dialog
  "DialogBackend — asks through a window the person can click or type into:
   the first installed dialog program in `default-tools` that can ask the
   question's kind (see `hive-notify.ask/question-kind`). It only asks; plain
   notifications stay with the desktop backend.

   Strata, bottom up (each layer uses only the ones below it):
     data      `DialogTool` records and `default-tools`
     domain    `invocation` and `reading`, pure multimethods keyed on
               [tool-id question-kind]: the command that asks, and the answer
               read off what the program printed
     pipeline  `plan`: collect the tool, promote the question to a command, or
               name why nothing can ask. Pure.
     boundary  `DialogBackend`: runs the one planned command, nothing else

   What a program can ask is exactly the set of [id kind] pairs `invocation`
   has a method for. A new program or a new question kind is new defmethods,
   never an edit here. The PATH probe, the display probe and the runner are
   injected, so it is testable without a display and degrades (never throws)
   when nothing is installed."
  (:require [clojure.string :as str]
            [hive-spi.notify :as notify]
            [hive-notify.ask :as ask]
            [hive-notify.shell :as sh]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

;; =============================================================================
;; Data
;; =============================================================================

(defrecord DialogTool [id bin])

(def zenity   "GTK dialogs."              (->DialogTool :zenity "zenity"))
(def kdialog  "KDE dialogs."              (->DialogTool :kdialog "kdialog"))
(def rofi     "rofi menu."                (->DialogTool :rofi "rofi"))
(def xmessage "X11 message box."          (->DialogTool :xmessage "xmessage"))
(def dmenu    "dmenu bar."                (->DialogTool :dmenu "dmenu"))

(def default-tools
  "Dialog programs in preference order; the first one on PATH that can ask the
   question's kind asks."
  [zenity kdialog rofi xmessage dmenu])

(def window-title
  "The title every dialog window carries, so a window manager can place it
   (e.g. an xmonad manageHook `title =? \"hive ask\"`)."
  "hive ask")

;; =============================================================================
;; Domain: pure helpers
;; =============================================================================

(defn- seconds [timeout-ms]
  (str (max 1 (quot (+ (long timeout-ms) 999) 1000))))

(defn- labels [choices]
  (mapv second choices))

(defn- prompt
  "The question's summary, then its body as the context to read."
  [{:keys [summary body]}]
  (str summary (when (seq (str body)) (str "\n\n" body))))

(defn- by-label
  "The choice id whose label is `raw`, trimmed, or nil."
  [choices raw]
  (let [s (str/trim (str raw))]
    (some (fn [[id label]] (when (= s label) id)) choices)))

(defn- by-index
  "The choice id at the 0-based index `raw` names, or nil."
  [choices raw]
  (when-let [i (parse-long (str/trim (str raw)))]
    (first (nth choices i nil))))

(defn- typed
  "The typed answer of a program that exits 0 on OK, or nil."
  [{:keys [exit out]}]
  (when (zero? exit) (ask/answer-text out)))

(defn- xmessage-label
  "A label xmessage's -buttons list can carry: no comma or colon."
  [l]
  (str/replace (str l) #"[,:]" " "))

;; =============================================================================
;; Domain: what each program runs, and how its output reads
;; =============================================================================

(defn- dispatch [tool question & _]
  [(:id tool) (ask/question-kind question)])

(defmulti invocation
  "Pure: {:args [...] :input <stdin or nil>} that asks `question` through
   `tool` within `timeout-ms`."
  #'dispatch)

(defmulti reading
  "Pure: the answer in the run result `r` ({:exit :out :timed-out?}) of the
   command `invocation` built, or nil for no answer."
  #'dispatch)

(defn supports?
  "True when `tool` can ask a question of `question`'s kind."
  [tool question]
  (contains? (methods invocation) (dispatch tool question)))

;; zenity: one button per choice, or an entry field for typed text.

(defmethod invocation [:zenity :choice] [_ {:keys [choices] :as q} ms]
  {:args (-> ["zenity" "--question" "--switch" "--no-markup"
              "--title" window-title "--text" (prompt q) "--timeout" (seconds ms)]
             (into (mapcat (fn [l] ["--extra-button" l])) (labels choices)))})

(defmethod reading [:zenity :choice] [_ {:keys [choices]} {:keys [out]}]
  (by-label choices out))

(defmethod invocation [:zenity :text] [_ {:keys [default] :as q} ms]
  {:args ["zenity" "--entry" "--title" window-title
          "--text" (prompt q) "--entry-text" (str default)
          "--width" "560" "--timeout" (seconds ms)]})

(defmethod reading [:zenity :text] [_ _ r] (typed r))

;; kdialog: an input box. It has no timeout of its own; run-timed ends it.

(defmethod invocation [:kdialog :text] [_ {:keys [default] :as q} _]
  {:args ["kdialog" "--title" window-title "--inputbox" (prompt q) (str default)]})

(defmethod reading [:kdialog :text] [_ _ r] (typed r))

;; rofi: a menu of the choices (answered by index), or a free entry line.

(defn- rofi-args [{:keys [summary body]} extra]
  (cond-> (into ["rofi" "-dmenu" "-p" (str summary)] extra)
    (seq (str body)) (into ["-mesg" (str body)])))

(defmethod invocation [:rofi :choice] [_ {:keys [choices] :as q} _]
  {:args  (rofi-args q ["-i" "-no-custom" "-format" "i"])
   :input (str (str/join "\n" (labels choices)) "\n")})

(defmethod reading [:rofi :choice] [_ {:keys [choices]} {:keys [exit out]}]
  (when (zero? exit) (by-index choices out)))

(defmethod invocation [:rofi :text] [_ q _]
  {:args (rofi-args q ["-l" "0"]) :input ""})

(defmethod reading [:rofi :text] [_ _ r] (typed r))

;; xmessage: buttons only, so it never asks for text.

(defmethod invocation [:xmessage :choice] [_ {:keys [choices] :as q} ms]
  {:args ["xmessage" "-center" "-print" "-timeout" (seconds ms)
          "-buttons" (str/join "," (map xmessage-label (labels choices)))
          (prompt q)]})

(defmethod reading [:xmessage :choice] [_ {:keys [choices]} {:keys [out]}]
  (let [s (str/trim (str out))]
    (some (fn [[id label]] (when (= s (str/trim (xmessage-label label))) id)) choices)))

;; dmenu: a list of the choices, or an empty list to type into.

(defmethod invocation [:dmenu :choice] [_ {:keys [summary choices]} _]
  {:args  ["dmenu" "-p" (str summary)]
   :input (str (str/join "\n" (labels choices)) "\n")})

(defmethod reading [:dmenu :choice] [_ {:keys [choices]} {:keys [exit out]}]
  (when (zero? exit) (by-label choices out)))

(defmethod invocation [:dmenu :text] [_ {:keys [summary]} _]
  {:args ["dmenu" "-p" (str summary)] :input ""})

(defmethod reading [:dmenu :text] [_ _ r] (typed r))

;; =============================================================================
;; Pipeline: question -> tool -> command (pure)
;; =============================================================================

(defn display?
  "True when the process can reach a graphical display."
  []
  (boolean (or (System/getenv "WAYLAND_DISPLAY") (System/getenv "DISPLAY"))))

(defn pick-tool
  "The first of `tools` on PATH (per `probe`) that can ask `question`, or nil."
  [tools probe question]
  (some (fn [t] (when (and (probe (:bin t)) (supports? t question)) t)) tools))

(defn plan
  "Pure: how to ask `question`. Either {:refusal <reason>} or
   {:tool <DialogTool> :timeout-ms ms :command {:args .. :input ..}}."
  [{:keys [tools probe display?]} question]
  (let [tool (pick-tool tools probe question)
        ms   (ask/timeout-ms question)]
    (cond
      (not (ask/valid-question? question)) {:refusal :invalid-question}
      (not (display?))                     {:refusal :no-display}
      (nil? tool)                          {:refusal :no-dialog-program}
      :else {:tool tool :timeout-ms ms :command (invocation tool question ms)})))

;; =============================================================================
;; Boundary
;; =============================================================================

(defrecord DialogBackend [tools probe display? ask-cmd]
  notify/INotify
  (notify-id [_] :dialog)
  (backend-available? [_]
    (boolean (and (display?) (some (fn [t] (probe (:bin t))) tools))))
  (accepts? [_ _] false)
  (notify! [_ _]
    {:delivered? false :backend :dialog :detail {:reason :ask-only}})

  ask/IAskKinds
  (answer-kinds [_]
    (into #{} (for [[id kind] (keys (methods invocation))
                    t tools
                    :when (and (= id (:id t)) (probe (:bin t)))]
                kind)))

  ask/IAsk
  (ask! [this question]
    (let [{:keys [refusal tool timeout-ms command]} (plan this question)]
      (if refusal
        {:answer nil :backend :dialog :detail {:reason refusal}}
        (let [r (ask-cmd (:args command) timeout-ms (:input command))]
          {:answer  (when-not (:timed-out? r) (reading tool question r))
           :backend :dialog
           :detail  {:tool (:bin tool) :kind (ask/question-kind question)
                     :exit (:exit r) :timed-out? (:timed-out? r)}})))))

(defn dialog-backend
  "Build a DialogBackend. opts (all optional): :tools (default `default-tools`),
   :probe (bin -> bool), :display? (-> bool), :ask-cmd ((fn [args timeout-ms
   input]) -> {:exit :out :timed-out?})."
  ([] (dialog-backend {}))
  ([{:keys [tools probe display? ask-cmd]
     :or   {tools default-tools probe sh/on-path? display? display? ask-cmd sh/run-timed}}]
   (->DialogBackend tools probe display? ask-cmd)))
