(ns spiral.tools.completion
  (:require
   [compliment.core :as jvm-completion]))


(defn complete [prefix ns context]
  (jvm-completion/completions prefix
                              {:ns (when ns (find-ns ns))
                               :context context}))
