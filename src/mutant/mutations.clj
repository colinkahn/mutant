(ns mutant.mutations
  (:require [mutant.zip :as z]
            [clojure.string :as str]))


(def ok-sentinel identity)

(defn not-ok-sentinel [& args]
  (assert nil "Mutant qualified-sym sentinel"))


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
   'when-not '[if when]
   '+        '[- * /]
   '-        '[+ * /]
   '/        '[+ - *]
   '*        '[+ - /]
   'true     '[false nil]
   'false    '[true]
   'true?    '[false? nil?]
   'false?   '[true? nil?]
   'nil?     '[some?]
   'some?    '[nil?]})


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
       (nil? (-> node z/left z/left))
       (z/list? (-> node z/up))
       (nil? (-> node z/up z/up z/up))))


(defn is-top-level-def? [node]
  (and (-> node z/sexpr symbol?)
       (nil? (-> node z/left))
       (some? (-> node z/up))
       (z/list? (-> node z/up))
       (nil? (-> node z/up z/up))))


(defn is-spec-keyword?
  "Similarly to `is-top-level-symbol?`, clojure.specs are
  in a global registry and will not correctly be purged
  during a rename operation."
  [node]
  (and (-> node z/sexpr qualified-keyword?)
       (some? (-> node z/left))
       (= 's/def (-> node z/left z/sexpr))))

(defn is-defmulti-dispatch?
  [node]
  (or (some-> node z/left z/left z/left z/sexpr #{'defmulti})
      (some-> node z/left z/left z/sexpr #{'defmulti})
      (some-> node z/left z/sexpr #{'defmulti})))

(defn is-defmethod-dispatch?
  [node]
  (some-> node z/left z/left z/sexpr #{'defmethod}))

(defn is-ignoreable-symbol?
  "By convention, symbols named _ or starting with _
  should be ignored, since they are solely for documentation
  and will not influence the test results."
  [node]
  (and (-> node z/sexpr symbol?)
       (str/starts-with? (-> node z/sexpr name) "_")))


(defn is-dynamic-symbol?
  "By convention, symbols named *foo* are dynamic and global
  in nature, often changing behavior in subtle ways that is
  not easily tested. Let's just ignore for now."
  [node]
  (and (-> node z/sexpr symbol?)
       (str/starts-with? (-> node z/sexpr name) "*")
       (str/ends-with? (-> node z/sexpr name) "*")))


(defn ignore-node? [node]
  ((some-fn is-top-level-symbol?
            is-top-level-def?
            is-spec-keyword?
            is-ignoreable-symbol?
            is-dynamic-symbol?
            is-defmulti-dispatch?
            is-defmethod-dispatch?)
   node))


(defn random-rename [node]
  (let [sexpr (z/sexpr node)]
    (cond
      (ignore-node? node)
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
      (condp = sexpr
        'clojure.core/identity
        [(z/replace node `mutant-sym)
         (z/replace node `not-ok-sentinel)]

        `mutant-sym
        [(z/replace node `other-mutant-sym)
         (z/replace node `ok-sentinel)
         (z/replace node `not-ok-sentinel)]

        [(z/replace node `mutant-sym)
         (z/replace node `ok-sentinel)
         (z/replace node `not-ok-sentinel)])

      (symbol? sexpr)
      (condp = sexpr
        'identity
        [(z/replace node 'mutant-sym)
         (z/replace node `not-ok-sentinel)]

        'mutant-sym
        [(z/replace node 'other-mutant-sym)
         (z/replace node `ok-sentinel)
         (z/replace node `not-ok-sentinel)]

        [(z/replace node 'mutant-sym)
         (z/replace node `ok-sentinel)
         (z/replace node `not-ok-sentinel)]))))


(defn swap-values [node]
  (let [sexpr (z/sexpr node)]
    (cond
      (and (number? sexpr) (zero? sexpr))
      [(z/replace node -1)
       (z/replace node 1)]

      (number? sexpr)
      [(z/replace node (- sexpr))
       (z/replace node 0)])))


(defn replace-with-nil [node]
  (let [sexpr (z/sexpr node)]
    (cond
      (ignore-node? node)
      nil

      ((some-fn symbol? keyword?) sexpr)
      [(z/replace node nil)])))


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
            (some-> (iterate z/right (z/down node))
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


(defn noop-swap! [node]
  (let [sexpr (z/sexpr node)]
    (when (seq? sexpr)
      (let [[sym ref f & more] sexpr]
        (when (and (= 'swap! sym) ref f)
          [(z/replace node `(~f (deref ~ref) ~@more))])))))


(defn noop-reset! [node]
  (let [sexpr (z/sexpr node)]
    (when (seq? sexpr)
      (let [[sym _ref val] sexpr]
        (when (and (= 'reset! sym) ref val)
          [(z/replace node val)])))))


(def mutations
  [illogical-swap
   swap-values
   replace-with-nil
   random-rename
   random-re-pattern
   rm-fn-body
   rm-args
   noop-swap!
   noop-reset!])


(defn mutate-with [m zipper]
  (try
    (m zipper)
    (catch UnsupportedOperationException ex
      nil)))

(defn mutate [zipper]
  (->> mutations
       (mapcat #(mutate-with % zipper))
       (remove nil?)))
