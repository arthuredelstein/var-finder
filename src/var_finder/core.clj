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
  of maps, each containing an s-expression, the corresponding lines of text
  and the line number."
  [file]
  (let [text (slurp file)
        code-reader (LineNumberingPushbackReader. (StringReader. (str \newline text)))
        text-reader (BufferedReader. (StringReader. (str text)))]
    (take-while identity (repeatedly
      (fn [] (let [ntop (.getLineNumber code-reader)
                   sexpr (try (read code-reader) (catch Exception e nil))
                   nbot (.getLineNumber code-reader)
                   all-lines (repeatedly (- nbot ntop) #(.readLine text-reader))
                   code-lines (drop-while empty-line? all-lines)
                   nlines (count code-lines)]
               (if sexpr {:sexpr sexpr, :text-lines code-lines, :line (- nbot nlines)})))))))
			   
;; tests

(def test-file
  "https://github.com/clojure/clojure/raw/b578c69d7480f621841ebcafdfa98e33fcb765f6/src/clj/clojure/core.clj")

(defn test-read []
  (read-clojure-source test-file))

  
