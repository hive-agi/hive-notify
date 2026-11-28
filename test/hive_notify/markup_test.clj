(ns hive-notify.markup-test
  "Markdown → styled spans: the wire vocabulary, literal-only text, code
   preservation and highlighting, and malformed input."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-notify.markup :as markup]))

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2026 hive-agi contributors

(def ^:private wire-styles
  "The only styles the helper understands (the Ask inbox wire)."
  #{"heading" "quote" "strong" "emphasis" "code" "code-block"
    "keyword" "string" "number" "comment"})

(defn- text
  "Every span's literal text, run together."
  [spans]
  (apply str (map :text spans)))

(defn- styles-of
  "The :styles of the first span whose text is exactly `t`, or nil."
  [spans t]
  (some #(when (= t (:text %)) (:styles %)) spans))

(deftest nil-and-blank-bodies-render-nothing
  (is (= [] (markup/spans nil)))
  (is (= [] (markup/spans "")))
  (is (= [] (markup/spans "   \n\t "))))

(deftest every-span-is-literal-and-in-the-wire-vocabulary
  (let [spans (markup/spans
               "# Hi\n\n**bold** and `code` and [link](https://example.invalid/secret)\n\n- a\n\n```clj\n(def x 1)\n```")]
    (is (seq spans))
    (is (every? (fn [{:keys [text styles]}]
                  (and (string? text) (vector? styles)
                       (every? wire-styles styles)))
                spans)
        "every span is a literal string with wire styles only")
    (is (some #(str/includes? (:text %) "example.invalid") spans)
        "a link destination is shown as inert text, not dropped")))

(deftest prose-is-kept-verbatim
  (is (= [{:text "just words" :styles []}] (markup/spans "just words")))
  (is (= "a b" (text (markup/spans "a\nb"))) "a soft break is a space")
  (is (= "a\nb" (text (markup/spans "a  \nb"))) "a hard break is a newline"))

(deftest emphasis-strong-and-headings-carry-their-style
  (is (= [{:text "Title" :styles ["heading"]}] (markup/spans "# Title")))
  (let [spans (markup/spans "plain **bold** and *em* end")]
    (is (= ["strong"] (styles-of spans "bold")))
    (is (= ["emphasis"] (styles-of spans "em")))
    (is (= [] (styles-of spans "plain "))))
  (testing "inline code inside a heading carries both styles"
    (is (= ["heading" "code"] (styles-of (markup/spans "# use `x`") "x")))))

(deftest links-and-images-show-their-text-and-destination
  (let [spans (markup/spans "see [the docs](https://example.invalid/secret) and ![a diagram](img-secret.png)")]
    (is (str/includes? (text spans) "the docs"))
    (is (str/includes? (text spans) "a diagram") "an image shows its alt text")
    (is (str/includes? (text spans) "https://example.invalid/secret")
        "a link destination is shown as inert text")
    (is (str/includes? (text spans) "img-secret.png")
        "an image destination is shown as inert text"))
  (testing "a destination already visible in the text is not repeated"
    (is (= [{:text "https://x/y" :styles []}]
           (markup/spans "[https://x/y](https://x/y)")))))

(deftest html-is-shown-literally-and-angle-brackets-stay-text
  (testing "an HTML fragment is inert text, tags and all"
    (is (= "<b>bold</b>" (text (markup/spans "<b>bold</b>"))))
    (is (= "<span class=\"x\">raw</span> text"
           (text (markup/spans "<span class=\"x\">raw</span> text")))))
  (testing "a value in angle brackets survives for approval"
    (is (= "the value <workspace> is required"
           (text (markup/spans "the value <workspace> is required")))))
  (testing "angle brackets written as text stay text"
    (is (= "a < b > c & d" (text (markup/spans "a < b > c & d"))))
    (is (= "1 < 2" (text (markup/spans "1 < 2"))))))

(deftest lists-render-their-markers-and-items
  (is (= [{:text "- a\n- b" :styles []}] (markup/spans "- a\n- b")))
  (is (= "1. one\n2. two" (text (markup/spans "1. one\n2. two"))))
  (testing "a list item keeps inline styles"
    (is (= ["strong"] (styles-of (markup/spans "- **loud**") "loud")))))

(deftest quotes-carry-the-quote-style-and-compose
  (let [spans (markup/spans "> quoted **text**")]
    (is (= ["quote"] (styles-of spans "quoted ")))
    (is (= ["quote" "strong"] (styles-of spans "text"))
        "a style composes with its block's")))

(deftest fenced-code-keeps-its-characters-verbatim
  (let [src   "(defn f [x]\n  (* x 2)) ; twice\n"
        spans (markup/spans (str "```clojure\n" src "```"))]
    (is (every? #(contains? (set (:styles %)) "code-block") spans))
    (is (= (str/replace src #"\n\z" "") (text spans))
        "the code characters survive exactly, but for the trailing newline")))

(deftest clojure-and-edn-fences-are-highlighted
  (let [spans (markup/spans "```clojure\n(def x 42) ; n\n```")]
    (is (= ["code-block" "keyword"] (styles-of spans "def")))
    (is (= ["code-block" "number"] (styles-of spans "42")))
    (is (= ["code-block" "comment"] (styles-of spans "; n"))))
  (let [spans (markup/spans "```edn\n{:a 1 :b \"s\"}\n```")]
    (is (= ["code-block" "keyword"] (styles-of spans ":a")))
    (is (= ["code-block" "number"] (styles-of spans "1")))
    (is (= ["code-block" "string"] (styles-of spans "\"s\"")))))

(deftest json-and-shell-fences-are-highlighted
  (let [spans (markup/spans "```json\n{\"a\": true, \"n\": -1.5}\n```")]
    (is (= ["code-block" "string"] (styles-of spans "\"a\"")))
    (is (= ["code-block" "keyword"] (styles-of spans "true")))
    (is (= ["code-block" "number"] (styles-of spans "-1.5"))))
  (let [spans (markup/spans "```sh\necho \"hi\" # c\n```")]
    (is (= ["code-block" "string"] (styles-of spans "\"hi\"")))
    (is (= ["code-block" "comment"] (styles-of spans "# c")))))

(deftest unknown-languages-fall-back-to-plain-monospace
  (let [spans (markup/spans "```rust\nfn main() {}\n```")]
    (is (= [{:text "fn main() {}" :styles ["code-block"]}] spans))
    (is (nil? (styles-of spans "fn")) "nothing is guessed at"))
  (testing "no info string is monospace too"
    (is (= [{:text "x = 1" :styles ["code-block"]}] (markup/spans "```\nx = 1\n```"))))
  (testing "an indented code block is monospace"
    (is (= [{:text "(foo)" :styles ["code-block"]}] (markup/spans "    (foo)\n")))))

(deftest a-code-block-inside-a-quote-composes
  (let [spans (markup/spans "> a\n>\n> ```clj\n> (x)\n> ```")]
    (is (= ["quote" "code-block"] (styles-of spans "(x)")))))

(deftest unicode-survives
  (let [spans (markup/spans "café — naïve **日本** 🎉")]
    (is (= ["strong"] (styles-of spans "日本")))
    (is (str/includes? (text spans) "café"))
    (is (str/includes? (text spans) "🎉"))))

(deftest malformed-content-never-throws-and-loses-nothing-visible
  (doseq [body ["```\nunclosed\n"
                "**bold"
                "a `tick"
                "[link]("
                "<"
                ">"
                "#"
                "*"
                "```clj"
                "- "
                "1. "
                "> > > deep"
                "\u0000"
                (apply str (repeat 200 "#"))]]
    (is (vector? (markup/spans body)) (pr-str body)))
  (testing "an unclosed fence keeps the rest as code"
    (is (= [{:text "unclosed" :styles ["code-block"]}] (markup/spans "```\nunclosed\n"))))
  (testing "unmatched emphasis markers stay literal"
    (is (str/includes? (text (markup/spans "**bold")) "**bold"))))

(deftest adjacent-spans-with-the-same-style-coalesce
  (testing "a hard-wrapped line is one span"
    (is (= [{:text "a\nb" :styles []}] (markup/spans "a\\\nb"))
        "a backslash at end of line is a hard break"))
  (testing "an emphasis run joins its neighbours when the style matches"
    (is (= [{:text "x" :styles ["strong"]}] (markup/spans "**x**"))
        "a lone bold run is one strong span")))

(deftest runs-of-plain-text-are-one-span
  (let [spans (markup/spans "one **two** three")]
    (is (= 3 (count spans)))
    (is (= [] (:styles (first spans))))
    (is (= ["strong"] (:styles (second spans))))))
