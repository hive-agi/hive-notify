(ns hive-notify.markup
  "Markdown → styled spans for the Ask inbox body.

   `spans` turns a Markdown string (or nil) into a flat vector of
     {:text \"literal text\" :styles [\"heading\" \"strong\" ...]}
   for the GTK helper to render. Every :text is inert literal text: the helper
   never parses Markdown, and never receives a live tag or a clickable link.
   :styles uses the wire vocabulary only —
     block  \"heading\" \"quote\" \"code\" \"code-block\"
     inline \"strong\" \"emphasis\"
     code   \"keyword\" \"string\" \"number\" \"comment\"
   and a span may carry several (a bold word in a quote is [\"quote\"
   \"strong\"]; a keyword in a fenced block is [\"code-block\" \"keyword\"]).

   The walk is the CommonMark AST (org.commonmark). Text is literal and nothing
   is dropped: an HTML fragment is shown as inert text, tags and all, so a value
   like <workspace> survives; a link or image shows its visible text and then
   its destination as inert text when the text does not already show it.
   Fenced and indented code keep their characters verbatim
   and are lexed for Clojure/EDN, JSON and shell when the fence's info string
   names one; any other language (or none) stays plain monospace.

   Layout is not decided here: a soft line break in prose becomes a space, a
   hard break and every block boundary a \"\\n\". Word and grapheme wrapping is
   the GTK side's job."
  (:require [clojure.string :as str])
  (:import [org.commonmark.parser Parser]
           [org.commonmark.node Document Text Code FencedCodeBlock IndentedCodeBlock
            Link Image HtmlInline HtmlBlock Paragraph Heading
            BulletList OrderedList ListItem BlockQuote
            SoftLineBreak HardLineBreak Emphasis StrongEmphasis ThematicBreak]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

;; =============================================================================
;; Styles
;; =============================================================================

(def style-order
  "The wire style vocabulary (see the Ask inbox protocol), in the order a span's
   :styles are listed. A span may carry several."
  ["heading" "quote" "strong" "emphasis" "code" "code-block"
   "keyword" "string" "number" "comment"])

(def ^:private style-rank (zipmap style-order (range)))

(defn- ordered-styles
  "`styles` de-duplicated and put in `style-order`, so a span is deterministic."
  [styles]
  (->> styles (filter style-rank) distinct (sort-by style-rank) vec))

(defn- span [text styles]
  {:text text :styles (ordered-styles styles)})

;; =============================================================================
;; Tree walk
;; =============================================================================

(defn- children [node]
  (loop [c (.getFirstChild node), acc (transient [])]
    (if c (recur (.getNext c) (conj! acc c)) (persistent! acc))))

(declare node-spans)

(defn- child-inline
  "The spans of `node`'s children, run together (inline content has no break)."
  [node styles]
  (into [] (mapcat #(node-spans % styles)) (children node)))

(defn- child-blocks
  "The spans of `node`'s block children, joined by `sep` and with empty blocks
   dropped."
  [node styles sep]
  (let [parts (into [] (remove empty?) (map #(node-spans % styles) (children node)))]
    (into [] (mapcat identity) (interpose [(span sep styles)] parts))))

(defn- list-item-spans [item styles marker]
  (into [(span marker styles)] (child-blocks item styles "\n")))

(defn- bullet-spans [node styles]
  (into [] (mapcat identity)
        (interpose [(span "\n" styles)]
                   (map #(list-item-spans % styles "- ") (children node)))))

(defn- ordered-spans [node styles]
  (let [start (.getStartNumber ^OrderedList node)]
    (into [] (mapcat identity)
          (interpose [(span "\n" styles)]
                     (map-indexed (fn [i item]
                                    (list-item-spans item styles (str (+ start i) ". ")))
                                  (children node))))))

;; =============================================================================
;; HTML and links: inert literal text, nothing dropped
;; =============================================================================

(defn- html-literal
  "The literal characters of an HTML fragment: shown as inert text, tags and
   all, so nothing a person is asked to approve is silently dropped (a value
   like <workspace> must survive)."
  [node]
  (let [s (if (instance? HtmlInline node)
            (str (.getLiteral ^HtmlInline node))
            (str (.getLiteral ^HtmlBlock node)))]
    (if (str/ends-with? s "\n") (subs s 0 (dec (count s))) s)))

(defn- html-spans [node styles]
  (let [text (html-literal node)]
    (if (str/blank? text) [] [(span text styles)])))

(defn- linkish-spans
  "A link's or image's visible text, then its destination as inert text when the
   visible text does not already show it — a person approving must see where a
   link points, and nothing is clickable."
  [node styles dest]
  (let [visible (child-inline node styles)
        shown   (apply str (map :text visible))
        d       (str dest)]
    (cond
      (str/blank? d)          visible
      (str/includes? shown d) visible
      (empty? visible)        [(span d styles)]
      :else                   (into visible [(span (str " (" d ")") styles)]))))

;; =============================================================================
;; Code: verbatim characters, lexed when the language is known
;; =============================================================================

(defn- code-text
  "The characters of a code block: verbatim but for the single trailing newline
   CommonMark keeps at the end of the block."
  [literal]
  (let [s (str literal)]
    (if (str/ends-with? s "\n") (subs s 0 (dec (count s))) s)))

(defn- language
  "The first token of a fence's info string, lowercased, or nil."
  [info]
  (when info
    (let [token (first (str/split (str/trim (str info)) #"\s+"))]
      (when (seq token) (str/lower-case token)))))

(defn- lex
  "Tokenize `s` with `rules` ([[pattern style] ...]): at each position the first
   rule that matches wins; unmatched characters are unstyled. Returns
   [[text style] ...] runs, in order."
  [s rules]
  (let [n        (count s)
        matchers (mapv (fn [[re style]]
                         (let [m (re-matcher re s)]
                           (.useTransparentBounds m true)
                           [m style]))
                       rules)]
    (loop [pos 0, out []]
      (if (>= pos n)
        out
        (let [[text style] (some (fn [[^java.util.regex.Matcher m style]]
                                   (.region m pos n)
                                   (when (.lookingAt m) [(.group m) style]))
                                 matchers)]
          (if (and text (pos? (count text)))
            (recur (+ pos (count text)) (conj out [text style]))
            (recur (inc pos) (conj out [(subs s pos (inc pos)) nil]))))))))

(def ^:private string-rule
  "A double-quoted run without an embedded quote — enough for basic spans."
  [#"\"[^\"]*\"" "string"])

(def ^:private clojure-rules
  [[#";[^\n]*" "comment"]
   string-rule
   [#"\\[A-Za-z][A-Za-z0-9]*" "string"]
   [#":{1,2}[A-Za-z0-9*+!?_<>=./-]+" "keyword"]
   [#"(?<![A-Za-z0-9_?!*-])[+-]?(?:0[xX][0-9a-fA-F]+|[0-9]+/[0-9]+|[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?)[MN]?" "number"]
   [#"\b(?:defprotocol|defrecord|defmacro|defmethod|defmulti|defonce|deftype|defn|def|letfn|let|if-let|if-not|if|when-let|when-not|when-first|when|doto|doseq|dotimes|do|condp|cond|case|loop|recur|try|catch|finally|throw|new|quote|var|ns|require|import|use|reify|extend-type|extend-protocol|for|while|binding|with-open|and|or|not|nil|true|false|comment)\b" "keyword"]])

(def ^:private json-rules
  [string-rule
   [#"-?[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?" "number"]
   [#"\b(?:true|false|null)\b" "keyword"]])

(def ^:private shell-rules
  [[#"#[^\n]*" "comment"]
   string-rule
   [#"'[^']*'" "string"]
   [#"\b(?:readonly|function|continue|declare|unset|until|while|shift|local|export|return|break|trap|eval|exec|source|alias|case|esac|done|elif|then|else|fi|for|select|time|do|in|if|set|exit)\b" "keyword"]
   [#"\b[0-9]+\b" "number"]])

(def ^:private lexers
  "The languages with lexical spans. Anything else is plain monospace."
  {"clojure" clojure-rules, "clj" clojure-rules, "cljc" clojure-rules
   "cljs" clojure-rules, "cljd" clojure-rules, "edn" clojure-rules
   "json" json-rules
   "sh" shell-rules, "bash" shell-rules, "shell" shell-rules, "zsh" shell-rules
   "console" shell-rules, "shellsession" shell-rules})

(defn- highlight
  "`code` as spans of `base-style` with a lexical style where a rule matches.
   An unknown language is one plain `base-style` span."
  [lang code base-style]
  (if-let [rules (get lexers lang)]
    (into [] (map (fn [[text style]]
                    (span text (if style (conj base-style style) base-style))))
          (lex code rules))
    [(span code base-style)]))

(defn- code-block-spans [node styles]
  (let [fenced? (instance? FencedCodeBlock node)
        literal (code-text (if fenced?
                             (.getLiteral ^FencedCodeBlock node)
                             (.getLiteral ^IndentedCodeBlock node)))
        lang    (when fenced? (language (.getInfo ^FencedCodeBlock node)))]
    (highlight lang literal (conj styles "code-block"))))

;; =============================================================================
;; The walk
;; =============================================================================

(defn- node-spans
  "The spans of `node`, `styles` inherited from its ancestors."
  [node styles]
  (cond
    (instance? Text node)          [(span (.getLiteral ^Text node) styles)]
    (instance? SoftLineBreak node) [(span " " styles)]
    (instance? HardLineBreak node) [(span "\n" styles)]
    (instance? Code node)          [(span (.getLiteral ^Code node) (conj styles "code"))]

    (instance? HtmlInline node)    (html-spans node styles)
    (instance? HtmlBlock node)     (html-spans node styles)
    (instance? ThematicBreak node) []

    (instance? FencedCodeBlock node)   (code-block-spans node styles)
    (instance? IndentedCodeBlock node) (code-block-spans node styles)

    (instance? Emphasis node)       (child-inline node (conj styles "emphasis"))
    (instance? StrongEmphasis node) (child-inline node (conj styles "strong"))
    (instance? Link node)           (linkish-spans node styles (.getDestination ^Link node))
    (instance? Image node)          (linkish-spans node styles (.getDestination ^Image node))
    (instance? Paragraph node)      (child-inline node styles)
    (instance? Heading node)        (child-inline node (conj styles "heading"))

    (instance? Document node)       (child-blocks node styles "\n\n")
    (instance? BlockQuote node)     (child-blocks node (conj styles "quote") "\n")
    (instance? ListItem node)       (child-blocks node styles "\n")
    (instance? BulletList node)     (bullet-spans node styles)
    (instance? OrderedList node)    (ordered-spans node styles)

    :else (child-inline node styles)))

(defn- coalesce
  "Merge adjacent spans with identical :styles; drop empty ones."
  [spans]
  (reduce (fn [acc {:keys [text styles]}]
            (cond
              (empty? text) acc
              (and (seq acc) (= styles (:styles (peek acc))))
              (conj (pop acc) (update (peek acc) :text str text))
              :else (conj acc {:text text :styles styles})))
          [] spans))

(def ^:private parser (.build (Parser/builder)))

(defn spans
  "Render `body` (Markdown, or nil) as a vector of {:text <literal>
   :styles [<style> ...]} spans for the Ask inbox helper. A nil or blank body is
   []. Never throws: malformed Markdown renders as the text it parses to."
  [body]
  (if (str/blank? (str body))
    []
    (->> (node-spans (.parse parser (str body)) [])
         coalesce
         (remove #(empty? (:text %)))
         vec)))
