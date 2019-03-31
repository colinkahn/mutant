(ns mutant.mutations
  (:require [rewrite-clj.zip :as z]))


(def qualified-sym 'mutant-qualified-sym)
(def other-qualified-sym 'mutant-other-qualified-sym)


(def illogical-swap-mutations
  {'and      '[or]
   'or       '[and]
   '>        '[>= = <]
   '<        '[<= = >]
   '>=       '[> = <]
   '<=       '[< = >]
   '=        '[not=]
   'not=     '[=]
   'boolean  '[not]
   'not      '[boolean]
   'empty?   '[seq]
   'seq      '[empty?]
   'for      '[doseq]
   'if       '[if-not when when-not]
   'if-not   '[if when when-not]
   'when     '[if-not when-not]
   'when-not '[if when]})


(defn illogical-swap [node]
  (when-let [ops (get illogical-swap-mutations (z/sexpr node))]
    (mapv #(z/replace node %) ops)))


(defn is-top-level-symbol?
  "A symbol that most likely represents a top-level name,
  e.g. `defn <..>`, `def <..>` or similar. Because of how
  *ns* reloading works, if we change the name, the old one
  will still exist and hence the mutation will live.
  Just going to skip these for now."
  [node]
  (and (-> node z/sexpr symbol?)
       (some? (-> node z/left))
       (some? (-> node z/up z/up))
       (nil? (-> node z/left z/left))
       (nil? (-> node z/up z/up z/up))))


(defn random-rename [node]
  (let [sexpr (z/sexpr node)]
    (cond
      (is-top-level-symbol? node)
      nil

      (qualified-keyword? sexpr)
      (case sexpr
        ::mutant-ns-kw [(z/replace node ::other-mutant-ns-kw)]
        [(z/replace node ::mutant-ns-kw)])

      (keyword? sexpr)
      (case sexpr
        :mutant-kw [(z/replace node :other-mutant-kw)]
        [(z/replace node :mutant-kw)])

      (qualified-symbol? sexpr)
      (case sexpr
        `qualified-sym [(z/replace node `other-qualified-sym)]
        [(z/replace node `qualified-sym)])

      (symbol? sexpr)
      (case sexpr
        'mutant-sym [(z/replace node 'other-mutant-sym)]
        [(z/replace node 'mutant-sym)]))))


(defn random-re-pattern [node]
  (let [sexpr (z/sexpr node)]
    (when (seq? sexpr)
      (let [[type chars & _] sexpr]
        (when (and (= 're-pattern type)
                   (string? chars))
          (if (= chars "mutant-regex")
            [(z/replace node #"other-mutant-regex")]
            [(z/replace node #"mutant-regex")]))))))


(defn rm-fn-body [node]
  (let [sexpr (z/sexpr node)]
    (when (seq? sexpr)
      (let [[defn _name args & _more] sexpr]
        (if (and (#{'defn 'defn-} defn)
                 (vector? args))
          (for [idx (drop 3 (range (count sexpr)))]
            (-> (iterate z/right (z/down node))
                (nth idx)
                z/remove
                z/up
                z/up)))))))


(defn rm-args [node]
  (let [sexpr (z/sexpr node)]
    (when (seq? sexpr)
      (let [[defn _name args & _more] sexpr]
        (if (and (#{'defn 'defn-} defn)
                 (vector? args))
          (mapv (fn [arg]
                  (-> node z/down z/right z/right
                      (z/edit (partial filterv (complement #{arg})))
                      (z/up)))
                args))))))


(def mutations
  [illogical-swap
   random-rename
   random-re-pattern
   rm-fn-body
   rm-args])


(defn mutate-with [m zipper]
  (try
    (m zipper)
    (catch UnsupportedOperationException ex
      nil)))

(defn mutate [zipper]
  (->> mutations
       (mapcat #(mutate-with % zipper))
       (remove nil?)))
