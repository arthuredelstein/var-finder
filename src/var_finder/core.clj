; Copyright 2011 Arthur Edelstein

(ns var-finder.core
 (:import [clojure.lang LineNumberingPushbackReader]
          [java.io BufferedReader StringReader]
          [java.lang StringBuilder]))


(defn empty-line?
  "True if a line of clojure code is empty (whitespace or comments)"
  [line]
  (when line
    (if-let [before-comment (first (.split line ";"))]
      (zero? (count (.trim before-comment)))
      false)))

(defn read-clojure-source
  "Takes the path to a clojure source file and returns a sequence
  of maps, each containing an s-expression with metadata including
  the line number and the file name."
  [file]
  (let [text (slurp file)
        code-reader (LineNumberingPushbackReader. (StringReader. (str \newline text)))
        text-reader (BufferedReader. (StringReader. (str text)))]
    (take-while identity (repeatedly
      (fn [] (let [nbotlast (.getLineNumber code-reader)
                   sexpr (try (read code-reader) (catch Exception e nil))
                   nbot (.getLineNumber code-reader)
                   latest-lines (repeatedly (- nbot nbotlast) #(.readLine text-reader))
                   code-lines (drop-while empty-line? latest-lines)
                   line (- nbot (count code-lines))]
               (if sexpr (with-meta sexpr {:line line :file file}))))))))

;; namespace: { :full-name :short-name :doc :author :members :subspaces :see-also}
;; vars: {:name :doc :arglists :var-type :file :line :added :deprecated :dynamic}

(defn get-meta-deflike
  [sexpr]
  (meta (second sexpr)))

;; TODO: :arglists
(defn get-meta-defnlike
  [sexpr]
  (let [[_ t2 t3 t4] sexpr
        d (if (string? t3) t3)]
    (merge
      (meta t2)
      (if (map? t3) t3)
      (if (and d (map? t4)) t4)
      (if d {:doc d}))))

(defn get-meta-tail-doc
  [sexpr n]
  (merge
    (get-meta-deflike sexpr)
    (let [[_ _ t3 t4] sexpr]
      {:doc
        (condp = n
          3 t3
          4 t4)})))

;; TODO: 'ns (namespaces)
(defn analyze-sexpr
	"Analyze the s-expression for docs and metadata."
  [sexpr]
    (condp #(some #{%2} %1) (first sexpr)
      '(def defhinted defonce defstruct)
        (get-meta-deflike sexpr)
      '(defn definline defmacro defmulti defn-memo defnk)
        (get-meta-defnlike sexpr)
      '(defprotocol defunbound)
        (get-meta-tail-doc 3)
      '(defalias defvar)
        (get-meta-tail-doc 4)
      nil))

(defn get-var-type [sexpr]
  (or
    ({'defn      "function"
      'definline "function"
      'defmacro  "macro"
      'defmulti  "multimethod"
      'defnmemo  "function"
      'defnk     "function"}
     (first sexpr))
    "var"))

(defn build-var-info [sexpr]
  (merge
    (select-keys (meta sexpr) [:file :line])
    (select-keys (analyze-sexpr sexpr) [:arglists :doc :added :deprecated :dynamic])
    {:name (name (second sexpr))
     :var-type (get-var-type sexpr)}))

(defn collect-vars [sexprs]
  (map build-var-info sexprs))

;; tests

(def test-file
  ;"https://github.com/clojure/clojure/raw/b578c69d7480f621841ebcafdfa98e33fcb765f6/src/clj/clojure/core.clj")
  "src/var_finder/code_sample.clj")

(defn test-read []
  (read-clojure-source test-file))

(defn test-collect []
  (collect-vars (test-read)))


