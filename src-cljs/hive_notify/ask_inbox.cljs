(ns hive-notify.ask-inbox
  "Native GTK4 (GJS) presentation for queued Hive Ask prompts.

  The JVM owns the queue, the deadlines and the first-answer race; this helper
  owns only presentation and the one explicit answer a person gives. It speaks
  a private UTF-8, newline-delimited JSON protocol over stdin/stdout, and every
  diagnostic goes to stderr so the wire stays clean.

  Strata, bottom up (each layer uses only the ones below it):
    data      the wire codec (emit, wire-init!) and the tag/palette tables
    domain    pure renderers: spans-of, palette-for, normalize-ask
    view      build-window and the render functions that mutate widgets
    boundary  the Gtk.Application that wires stdin events to the view

  Wire, helper -> JVM:
    {\"type\":\"ready\"}                          after Gtk.init_check and app setup
    {\"type\":\"answer\",\"id\":\"uuid\",\"value\":\"yes\"}   choice id, or typed answer
  Wire, JVM -> helper:
    {\"type\":\"queue\",\"generation\":1,\"asks\":[{...}]}   the whole queue, oldest first

  An ask is {id, summary, body:[{text, styles:[...]}], remainingMs, choices:[[id,
  label]]} or, for a typed question, {id, summary, body, remainingMs, input:\"text\",
  default}. The body styles are heading, strong, emphasis, quote, code, code-block,
  keyword, string, number, comment; several may apply to one span. Summary and
  choice labels are always literal -- never parsed as markup.

  Compiled with ClojureScript to a single self-contained GJS script; see
  script/compile_ask_inbox.clj. Gtk 4.0 is selected before the Gtk module is
  first loaded. :simple optimizations are used deliberately: property renaming
  (advanced) would rewrite GI names such as `imports.gi` and every method call.

  GJS notes that shaped this file: `insert_with_tags_by_name` is not exposed, so
  text is inserted and tags applied by name over the inserted range; a Gtk.Paned
  switches orientation (horizontal <-> stacked) rather than swapping widgets; a
  non-unique local application keeps the window out of any session singleton and
  out of xmonad's exact `hive ask` popup rule.")

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

;; ---------------------------------------------------------------------------
;; GI module handles. Select Gtk 4.0 before the Gtk module is first loaded.
;; ---------------------------------------------------------------------------

(aset (.-versions (.-gi js/imports)) "Gtk" "4.0")

(defn- un [x] (or (nil? x) (undefined? x)))

(defn- gi-mod
  "Load a GI namespace, exiting non-zero -- so the JVM's dialog fallback takes
   over -- when its typelib is missing. `imports.gi.<Name>` THROWS rather than
   returning undefined when the namespace is absent, so this guards both."
  [name]
  (try
    (let [m (aget (.-gi js/imports) name)]
      (if (un m)
        (throw (js/Error. (str name " typelib missing")))
        m))
    (catch :default e
      (js/printerr (str "[ask-inbox] " name " unavailable (" e "); exiting"))
      (when-let [sys (aget js/imports "system")]
        ((aget sys "exit") 1))
      nil)))

(def ^:private GLib (gi-mod "GLib"))
(def ^:private Gio (gi-mod "Gio"))
(def ^:private GioUnix (gi-mod "GioUnix"))
(def ^:private Gtk (gi-mod "Gtk"))
(def ^:private Gdk (gi-mod "Gdk"))
(def ^:private Pango (gi-mod "Pango"))

(defn- cls
  "A class/constructor from a GI namespace, addressed by string so that no
   property-renaming pass can reach it."
  [mod name]
  (aget mod name))

(defn- en
  "A GI enum member, addressed by string."
  [mod enum member]
  (aget (aget mod enum) member))

(defn- pen [enum member] (en Pango enum member))

;; Construct a GI type. The class expression is parenthesised on purpose: a bare
;; `(new (cls M \"N\") ...)` compiles to `new cls.call(...)(...)`, which JS reads
;; as `new (cls.call)(...)` and throws \"cls.call is not a constructor\".
(defn- gnew
  ([ctor] (js* "new (~{}) ()" ctor))
  ([ctor props] (js* "new (~{}) (~{})" ctor props)))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def ^:private APP_ID "org.hive.AskInbox")
(def ^:private WINDOW_TITLE "Hive Ask Inbox")

;; Below this width the queue/details Paned stacks instead of sitting side by
;; side. Chosen so two readable columns fit, and below it one column does.
(def ^:private NARROW_WIDTH 620)

;; Finite minimums: the window can never collapse smaller than this, and neither
;; panel can vanish as the divider is dragged.
(def ^:private WINDOW-MIN-W 380)
(def ^:private WINDOW-MIN-H 340)
(def ^:private QUEUE-MIN-W 160)
(def ^:private QUEUE-MIN-H 78)
(def ^:private DETAILS-MIN-W 200)
(def ^:private DETAILS-MIN-H 150)

;; A bounded height for the choice list, so eight long labels stay reachable by
;; scrolling instead of pushing the details out of the window.
(def ^:private CHOICES-MAX-HEIGHT 140)

(def ^:private style-names
  ["heading" "strong" "emphasis" "quote"
   "code" "code-block" "keyword" "string" "number" "comment"])

(def ^:private style-name-set (set style-names))

;; ---------------------------------------------------------------------------
;; State
;; ---------------------------------------------------------------------------

(def ^:private out-stream (atom nil))
(def ^:private app (atom nil))

(def ^:private state
  (atom {:generation nil
         :presented-generation nil
         :asks []
         :selected-id nil
         :rows {}
         :drafts {}
         :sent {}
         :window nil
         :paned nil
         :queue-scroll nil
         :listbox nil
         :body-scroll nil
         :body-view nil
         :body-buffer nil
         :body-adj nil
         :answer-box nil
         :rendered-ask-id nil
         :activated false}))

;; ===========================================================================
;; Data: wire codec
;; ===========================================================================

(defn- log [message]
  (js/printerr (str "[ask-inbox] " message)))

(defn- wire-init! []
  (try
    (reset! out-stream (.new (cls GioUnix "OutputStream") 1 false))
    (catch :default e (log (str "stdout unavailable: " e)))))

(defn- emit [obj]
  (when-let [out @out-stream]
    (try
      (let [line (str (js/JSON.stringify obj) "\n")
            bytes (.encode (gnew js/TextEncoder) line)]
        (.write_bytes out (.new (cls GLib "Bytes") bytes) nil)
        (.flush out nil))
      (catch :default e (log (str "emit failed: " e))))))

;; ===========================================================================
;; Data: palette and tags
;; ===========================================================================

(defn- palette-for [dark]
  (if dark
    {:quote "#9aa0a6" :code-bg "#2b2b2b" :keyword "#c792ea"
     :string "#c3e88d" :number "#f78c6c" :comment "#7f848e"}
    {:quote "#5f6368" :code-bg "#f0f0f0" :keyword "#8250df"
     :string "#0a7d33" :number "#b5560a" :comment "#6a737d"}))

(defn- prefers-dark? []
  (try
    (if-let [settings (.get_default (cls Gtk "Settings"))]
      (or (.-gtk_application_prefer_dark_theme settings)
          (let [name (str (or (.-gtk_theme_name settings) ""))]
            (not= -1 (.indexOf (.toLowerCase name) "dark"))))
      false)
    (catch :default _ false)))

(defn- install-tags! [buffer palette]
  (let [table (.get_tag_table buffer)
        defs {"heading" {:weight (pen "Weight" "BOLD") :scale 1.2 :above 10 :below 6}
              "strong" {:weight (pen "Weight" "BOLD")}
              "emphasis" {:style (pen "Style" "ITALIC")}
              "quote" {:style (pen "Style" "ITALIC") :left-margin 16 :foreground (:quote palette)}
              "code" {:family "monospace" :background (:code-bg palette)}
              "code-block" {:family "monospace" :background (:code-bg palette)
                            :left-margin 12 :above 6 :below 6}
              "keyword" {:family "monospace" :weight (pen "Weight" "BOLD")
                         :foreground (:keyword palette)}
              "string" {:family "monospace" :foreground (:string palette)}
              "number" {:family "monospace" :foreground (:number palette)}
              "comment" {:family "monospace" :style (pen "Style" "ITALIC")
                         :foreground (:comment palette)}}]
    (doseq [name style-names]
      (let [tag (gnew (cls Gtk "TextTag") (js-obj "name" name))
            d (get defs name {})]
        (try
          (when-some [v (:family d)] (set! (.-family tag) v))
          (when-some [v (:weight d)] (set! (.-weight tag) v))
          (when-some [v (:style d)] (set! (.-style tag) v))
          (when-some [v (:scale d)] (set! (.-scale tag) v))
          (when-some [v (:foreground d)] (set! (.-foreground tag) v))
          (when-some [v (:background d)] (set! (.-background tag) v))
          (when-some [v (:left-margin d)] (set! (.-left_margin tag) v))
          (when-some [v (:above d)] (set! (.-pixels_above_lines tag) v))
          (when-some [v (:below d)] (set! (.-pixels_below_lines tag) v))
          (catch :default e (log (str "tag " name " property failed: " e))))
        (.add table tag)))))

;; ===========================================================================
;; Domain: pure helpers
;; ===========================================================================

;; The body is a vector of {text, styles}; a plain string is tolerated as one
;; unstyled span so a malformed snapshot still renders rather than throwing.
(defn- spans-of [body]
  (cond
    (string? body) [{:text body :styles []}]
    (array? body)
    (mapv (fn [span]
            (if (and span (object? span))
              {:text (let [t (aget span "text")] (if (un t) "" (str t)))
               :styles (let [s (aget span "styles")] (if (array? s) (vec s) []))}
              {:text (if (un span) "" (str span)) :styles []}))
          body)
    :else []))

(defn- is-text-ask [ask]
  (and ask (= "text" (:input ask))))

(defn- choice-pairs [ask]
  (let [cs (aget ask "choices")]
    (if (array? cs)
      (->> cs
           (filter #(and (array? %) (>= (.-length %) 2)))
           (mapv (fn [c] [(str (aget c 0)) (str (aget c 1))])))
      [])))

(defn- normalize-ask
  "Read one raw JSON ask into a plain map so the view layer never touches a JS
   object directly."
  [a]
  (let [id (aget a "id")
        summary (let [s (aget a "summary")] (if (un s) "" (str s)))
        body (spans-of (aget a "body"))
        remaining (aget a "remainingMs")
        input (aget a "input")
        default (aget a "default")
        choices (choice-pairs a)
        base {:id id :summary summary :body body :remaining-ms remaining}]
    (if (= input "text")
      (assoc base :input "text" :default (if (un default) nil default))
      (assoc base :choices choices))))

;; ===========================================================================
;; View: queue
;; ===========================================================================

(defn- clear-children! [widget]
  (loop [child (.get_first_child widget)]
    (when child
      (let [next (.get_next_sibling child)]
        (.remove widget child)
        (recur next)))))

(defn- current-ask []
  (let [{:keys [selected-id asks]} @state]
    (when selected-id
      (first (filter #(= (:id %) selected-id) asks)))))

(defn- rebuild-queue! []
  (when-let [lb (:listbox @state)]
    (let [{:keys [asks rows]} @state
          ids (set (map :id asks))]
      (doseq [[id row] rows]
        (when-not (ids id)
          (.remove lb row)
          (swap! state (fn [s]
                         (-> s
                             (update :rows dissoc id)
                             (update :drafts dissoc id)
                             (update :sent dissoc id))))))
      (doseq [ask asks]
        (if-let [existing (get (:rows @state) (:id ask))]
          (when (get (:sent @state) (:id ask))
            (.add_css_class (.get_child existing) "dim-label"))
          (let [row (gnew (cls Gtk "ListBoxRow"))
                label (gnew (cls Gtk "Label") (js-obj "label" (:summary ask)))]
            (.set_activatable row true)
            (.set_selectable row true)
            (.set_xalign label 0)
            (.set_ellipsize label (pen "EllipsizeMode" "END"))
            (.set_margin_top label 6)
            (.set_margin_bottom label 6)
            (.set_margin_start label 10)
            (.set_margin_end label 10)
            (when (get (:sent @state) (:id ask))
              (.add_css_class label "dim-label"))
            (.set_child row label)
            (.set_tooltip_text row (or (:summary ask) ""))
            (set! (.-askId row) (:id ask))
            (swap! state assoc-in [:rows (:id ask)] row)
            (.append lb row)))))))

(defn- restore-selection! []
  (when-let [lb (:listbox @state)]
    (let [{:keys [selected-id asks]} @state
          present (some #(= (:id %) selected-id) asks)
          target (if (and selected-id present)
                   selected-id
                   (when (seq asks) (:id (first asks))))]
      (swap! state assoc :selected-id target)
      (if-let [row (get (:rows @state) target)]
        (.select_row lb row)
        (.unselect_all lb)))))

;; ===========================================================================
;; View: details
;; ===========================================================================

(defn- apply-body! [buffer spans]
  (doseq [span spans]
    (let [start-off (.get_char_count buffer)
          _ (.insert buffer (.get_end_iter buffer) (:text span) -1)
          end-off (.get_char_count buffer)
          start-iter (.get_iter_at_offset buffer start-off)
          end-iter (.get_iter_at_offset buffer end-off)]
      (doseq [name (:styles span)]
        (when (contains? style-name-set name)
          (try
            (.apply_tag_by_name buffer name start-iter end-iter)
            (catch :default e (log (str "apply tag " name " failed: " e)))))))))

(declare send-answer!)

(defn- render-text-answer! [ask box already-sent]
  (let [row (gnew (cls Gtk "Box") (js-obj "orientation" (en Gtk "Orientation" "HORIZONTAL")
                                          "spacing" 6))
        _ (.set_margin_top row 8)
        editor (gnew (cls Gtk "TextView")
                     (js-obj "wrap_mode" (en Gtk "WrapMode" "WORD_CHAR")
                             "accepts_tab" false))
        _ (.set_editable editor (not already-sent))
        _ (.set_left_margin editor 6)
        _ (.set_right_margin editor 6)
        buffer (.get_buffer editor)
        _ (.set_text buffer (or (get (:drafts @state) (:id ask)) (:default ask) "") -1)
        text (fn []
               (let [b (.get_bounds buffer)]
                 (.get_text buffer (aget b 0) (aget b 1) true)))
        scroll (gnew (cls Gtk "ScrolledWindow") (js-obj "hexpand" true "has_frame" true))
        _ (.set_policy scroll (en Gtk "PolicyType" "NEVER") (en Gtk "PolicyType" "AUTOMATIC"))
        _ (.set_min_content_height scroll 64)
        _ (.set_child scroll editor)
        send (gnew (cls Gtk "Button") (js-obj "label" "Send"))
        _ (.set_sensitive send (and (not already-sent) (pos? (.-length (.trim (text))))))]
    (.connect buffer "changed"
              (fn []
                (swap! state assoc-in [:drafts (:id ask)] (text))
                (.set_sensitive send
                                (and (not (get (:sent @state) (:id ask)))
                                     (pos? (.-length (.trim (text))))))))
    (.connect send "clicked"
              (fn []
                (let [value (.trim (text))]
                  (when (pos? (.-length value))
                    (send-answer! (:id ask) (.slice value 0 4000))))))
    (.append row scroll)
    (.append row send)
    (.append box row)))

(defn- render-choice-answer! [ask box already-sent]
  ;; `ask` is already normalized: :choices is a CLJS vector of [id label].
  (let [choices (:choices ask)]
    (when (pos? (count choices))
      ;; Eight long labels must stay usable: a bounded, vertically scrolling
      ;; stack, each button full width with a wrapped, left-aligned label.
      (let [scroll (gnew (cls Gtk "ScrolledWindow"))
            _ (.set_policy scroll (en Gtk "PolicyType" "NEVER") (en Gtk "PolicyType" "AUTOMATIC"))
            _ (.set_min_content_height scroll 64)
            _ (.set_max_content_height scroll CHOICES-MAX-HEIGHT)
            _ (.set_propagate_natural_height scroll true)
            _ (.set_margin_top scroll 8)
            list (gnew (cls Gtk "Box") (js-obj "orientation" (en Gtk "Orientation" "VERTICAL")
                                               "spacing" 6))]
        (doseq [[id label] choices]
          (let [button (gnew (cls Gtk "Button"))
                text (gnew (cls Gtk "Label") (js-obj "label" label))]
            (.set_hexpand button true)
            (.set_sensitive button (not already-sent))
            (.set_wrap text true)
            (.set_wrap_mode text (pen "WrapMode" "WORD_CHAR"))
            (.set_xalign text 0)
            (.set_margin_top text 4)
            (.set_margin_bottom text 4)
            (.set_child button text)
            (.connect button "clicked" (fn [] (send-answer! (:id ask) id)))
            (.append list button)))
        (.set_child scroll list)
        (.append box scroll)))))

(defn- render-answer! [ask]
  (let [box (:answer-box @state)]
    (clear-children! box)
    (when ask
      (let [already-sent (boolean (get (:sent @state) (:id ask)))]
        (if (is-text-ask ask)
          (render-text-answer! ask box already-sent)
          (render-choice-answer! ask box already-sent))))))

(defn- render-details! []
  (let [ask (current-ask)
        ask-id (when ask (:id ask))]
    (when (not= ask-id (:rendered-ask-id @state))
      (swap! state assoc :rendered-ask-id ask-id)
      (when-let [buffer (:body-buffer @state)]
        (.set_text buffer "" 0)
        (when ask
          (apply-body! buffer
                       (into [{:text (str (:summary ask) "\n\n") :styles ["heading"]}]
                             ;; `ask` is already normalized: :body is a CLJS
                             ;; vector of {:text .. :styles ..} spans.
                             (:body ask)))))
      (render-answer! ask)
      (.idle_add GLib GLib.PRIORITY_DEFAULT_IDLE
                 (fn []
                   (when (and (:body-adj @state) (= (:rendered-ask-id @state) ask-id))
                     (.set_value (:body-adj @state) 0))
                   false)))))

;; ===========================================================================
;; Boundary: answering, presentation, and the application
;; ===========================================================================

(defn- send-answer! [id value]
  (when (and id (not (get (:sent @state) id)))
    (swap! state assoc-in [:sent id] true)
    (emit (js-obj "type" "answer" "id" id "value" (str value)))
    (rebuild-queue!)
    (render-answer! (current-ask))))

(defn- has-pending? []
  (boolean (some #(not (get (:sent @state) (:id %))) (:asks @state))))

(defn- present-window! []
  (when-let [win (:window @state)]
    (try
      (.unminimize win)
      (.present win)
      (catch :default e (log (str "present failed: " e))))))

(defn- hide-window! []
  (when-let [win (:window @state)]
    (try
      (.set_visible win false)
      (catch :default e (log (str "hide failed: " e))))))

(defn- apply-queue! [generation asks]
  (let [selected (:selected-id @state)]
    (swap! state assoc :generation generation :asks asks)
    (rebuild-queue!)
    (swap! state assoc :selected-id selected)
    (restore-selection!)
    (render-details!)
    (cond
      (empty? asks) (hide-window!)
      (not= generation (:presented-generation @state))
      ;; First snapshot of a new generation presents once; later coalesced
      ;; snapshots update in place without stealing focus again.
      (do (swap! state assoc :presented-generation generation)
          (present-window!)))))

(defn- initial-size []
  (let [w (atom 940)
        h (atom 700)]
    (try
      (when-let [display (.get_default (cls Gdk "Display"))]
        (let [mons (.get_monitors display)
              n (.get_n_items mons)
              monitor (loop [i 0]
                        (if (< i n)
                          (let [m (.get_item mons i)
                                primary (try (boolean (.is_primary m))
                                             (catch :default _ false))]
                            (if primary m (recur (inc i))))
                          nil))
              monitor (or monitor (when (pos? n) (.get_item mons 0)))]
          (when monitor
            (let [g (.get_geometry monitor)
                  gw (.-width g)
                  gh (.-height g)]
              (reset! w (js/Math.round (js/Math.min (js/Math.max (* gw 0.45) 640) 1080)))
              (reset! h (js/Math.round (js/Math.min (js/Math.max (* gh 0.70) 520) 900)))
              (reset! w (js/Math.min @w (js/Math.max 400 (- gw 80))))
              (reset! h (js/Math.min @h (js/Math.max 360 (- gh 80))))))))
      (catch :default e (log (str "monitor query failed: " e))))
    [(js/Math.max @w WINDOW-MIN-W) (js/Math.max @h WINDOW-MIN-H)]))

(defn- apply-responsive! [actual-width]
  (let [win (:window @state)
        paned (:paned @state)]
    (when (and win paned)
      (let [width (or actual-width
                      (try
                        (let [size (.get_default_size win)]
                          (or (.get_width win) (when size (aget size 0))))
                        (catch :default _ 0)))
            narrow (and (pos? width) (< width NARROW_WIDTH))
            want (en Gtk "Orientation" (if narrow "VERTICAL" "HORIZONTAL"))]
        (when-not (= (.get_orientation paned) want)
          (.set_orientation paned want)
          (.set_position paned (if narrow 86 220)))))))

(defn- build-queue-panel []
  (let [scroll (gnew (cls Gtk "ScrolledWindow"))
        lb (gnew (cls Gtk "ListBox"))]
    (.set_policy scroll (en Gtk "PolicyType" "NEVER") (en Gtk "PolicyType" "AUTOMATIC"))
    (.set_size_request scroll QUEUE-MIN-W QUEUE-MIN-H)
    (.set_vexpand scroll true)
    (.set_hexpand scroll true)
    (.set_selection_mode lb (en Gtk "SelectionMode" "SINGLE"))
    (.set_activate_on_single_click lb true)
    (.connect lb "row-selected"
              (fn [_ row]
                (when row
                  (swap! state assoc :selected-id (.-askId row))
                  (render-details!))))
    (.set_child scroll lb)
    (swap! state assoc :queue-scroll scroll :listbox lb)
    scroll))

(defn- build-details-panel []
  (let [box (gnew (cls Gtk "Box") (js-obj "orientation" (en Gtk "Orientation" "VERTICAL")
                                          "spacing" 6))]
    (.set_size_request box DETAILS-MIN-W DETAILS-MIN-H)
    (.set_vexpand box true)
    (.set_hexpand box true)
    (.set_margin_top box 8)
    (.set_margin_bottom box 8)
    (.set_margin_start box 8)
    (.set_margin_end box 8)
    (let [body-scroll (gnew (cls Gtk "ScrolledWindow"))
          view (gnew (cls Gtk "TextView"))]
      (.set_policy body-scroll (en Gtk "PolicyType" "NEVER") (en Gtk "PolicyType" "AUTOMATIC"))
      (.set_min_content_height body-scroll 48)
      (.set_vexpand body-scroll true)
      (.set_wrap_mode view (en Gtk "WrapMode" "WORD_CHAR"))
      (.set_editable view false)
      (.set_cursor_visible view false)
      (.set_monospace view false)
      (.set_accepts_tab view false)
      (.set_top_margin view 6)
      (.set_bottom_margin view 6)
      (.set_left_margin view 6)
      (.set_right_margin view 6)
      (.set_pixels_below_lines view 2)
      (let [buffer (.get_buffer view)]
        (install-tags! buffer (palette-for (prefers-dark?)))
        (.set_child body-scroll view)
        (.append box body-scroll)
        (.append box (gnew (cls Gtk "Separator")
                           (js-obj "orientation" (en Gtk "Orientation" "HORIZONTAL"))))
        (let [answer-box (gnew (cls Gtk "Box")
                               (js-obj "orientation" (en Gtk "Orientation" "VERTICAL")
                                       "spacing" 6))]
          (.append box answer-box)
          (swap! state assoc :answer-box answer-box))
        (swap! state assoc
               :body-scroll body-scroll
               :body-view view
               :body-buffer buffer
               :body-adj (.get_vadjustment body-scroll))))
    box))

(defn- build-window [app]
  (let [win (gnew (cls Gtk "ApplicationWindow")
                  (js-obj "application" app "title" WINDOW_TITLE))
        [w h] (initial-size)]
    (.set_default_size win w h)
    (.set_size_request win WINDOW-MIN-W WINDOW-MIN-H)
    (.connect win "notify::default-width" (fn [_ _] (apply-responsive! nil)))
    (.connect win "realize"
              (fn [widget]
                (let [surface (.get_surface widget)]
                  (.connect surface "notify::width"
                            (fn [s _] (apply-responsive! (.get_width s)))))))
    (.connect win "close-request"
              (fn [_]
                (if (has-pending?)
                  ;; The person closed the window while answers are still owed:
                  ;; minimize so the queue stays reachable instead of losing the asks.
                  (do (try (.minimize win)
                           (catch :default e (log (str "minimize failed: " e))))
                      true)
                  false)))
    (let [paned (gnew (cls Gtk "Paned")
                      (js-obj "orientation" (en Gtk "Orientation" "HORIZONTAL")))]
      (.set_wide_handle paned false)
      (.set_position paned 300)
      (.set_resize_start_child paned false)
      (.set_shrink_start_child paned false)
      (.set_start_child paned (build-queue-panel))
      (.set_end_child paned (build-details-panel))
      (.set_child win paned)
      (swap! state assoc :paned paned))
    (.connect win "show" (fn [_] (apply-responsive! nil)))
    (swap! state assoc :window win)
    win))

;; ===========================================================================
;; Boundary: stdin events
;; ===========================================================================

(defn- handle-line! [line]
  (let [msg (try
              (js/JSON.parse line)
              (catch :default e (log (str "bad JSON on stdin: " e)) nil))]
    (when (and msg (object? msg))
      (if (= (aget msg "type") "queue")
        (let [g (aget msg "generation")
              generation (if (number? g) g (:generation @state))
              raw (aget msg "asks")
              asks (if (array? raw) (mapv normalize-ask raw) [])]
          (apply-queue! generation asks))
        (log (str "unknown message type: " (aget msg "type")))))))

(defn- read-step [reader]
  (.read_line_async
   reader GLib.PRIORITY_DEFAULT nil
   (fn [_src res]
     (let [line (try
                  (let [r (.read_line_finish_utf8 reader res)]
                    (if (and r (not (un (aget r 0)))) (aget r 0) nil))
                  (catch :default e (log (str "read failed: " e)) nil))]
       (if (nil? line)
         ;; stdin EOF: the JVM is gone, so the window has nothing left to serve.
         (do (log "stdin closed")
             (when-let [win (:window @state)] (.set_visible win false))
             (.quit @app))
         (do (when (pos? (.-length line)) (handle-line! line))
             (read-step reader)))))))

(defn- start-reading! []
  (let [stream (try
                 (.new (cls GioUnix "InputStream") 0 false)
                 (catch :default e (log (str "stdin unavailable: " e)) nil))]
    (when stream
      (let [reader (gnew (cls Gio "DataInputStream") (js-obj "base_stream" stream))]
        (read-step reader)))))

;; ===========================================================================
;; Boundary: main
;; ===========================================================================

(defn- main []
  (if-not ((cls Gtk "init_check"))
    (do (log "no display; exiting")
        1)
    (do
      (reset! app (gnew (cls Gtk "Application")
                        (js-obj "application_id" APP_ID
                                "flags" (en Gio "ApplicationFlags" "NON_UNIQUE"))))
      (.connect @app "activate"
                (fn []
                  (when-not (:activated @state)
                    (swap! state assoc :activated true)
                    (.hold @app)
                    (build-window @app)
                    (start-reading!)
                    (emit (js-obj "type" "ready")))))
      (wire-init!)
      (.run @app (array)))))

(def ^:private code (main))

(when-not (un code)
  (when-let [sys (aget js/imports "system")]
    ((aget sys "exit") (if (number? code) code 0))))
