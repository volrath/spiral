(ns spiral.tools.load-file)


(defn load-file-code
  "Given the contents of a file, its _source-path-relative_ path,
   and its filename, will load those contents with appropriate filename
  references and line numbers in metadata, etc."
  [file file-path file-name]
  (clojure.lang.Compiler/load (java.io.StringReader. file)
                              file-path
                              file-name))
