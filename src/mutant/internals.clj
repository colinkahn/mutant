(ns mutant.internals
  (:require [mutant.zip :as z]
            [mutant.mutations :refer [mutate]]
            [clojure.java.io :as jio]
            [clojure.java.shell :as shell]
            [clojure.tools.namespace
             [find :as find]
             [file :as file]
             [parse :as parse]
             [dependency :as dep]]
            [clojure.string :as str]))


(defn x-remove-ns-forms []
  (remove (comp #{'ns} z/sexpr z/down)))


(defn code-touches-diff?
  [[diff-start diff-stop] def-start def-stop]
  (let [def-stop (dec (or def-stop diff-stop))]
    (or (<= def-start diff-start def-stop)
        (<= def-start diff-stop def-stop)
        (<= diff-start def-start diff-stop))))


(defn x-keep-forms-with-lines [line-ranges]
  (if (empty? line-ranges)
    (map identity)
    (filter (fn [node]
              (let [start (-> node z/position first)
                    stop  (some-> node z/right-no-skip-ws z/position first dec)]
                (some #(code-touches-diff? % start stop) line-ranges))))))


(defn file-to-zippers
  [file {:keys [:line-ranges]}]
  (into []
        (comp (take-while some?)
              (x-remove-ns-forms)
              (x-keep-forms-with-lines line-ranges))
        (->> (z/of-file file {:track-position? true})
             (iterate z/right))))


(defn dirs-to-namespaces [src-paths]
  (let [files      (mapcat find/find-clojure-sources-in-dir
                           (map jio/file src-paths))
        namespaces (mapv (comp second file/read-file-ns-decl) files)]
    (zipmap files namespaces)))


(defn paths-in-zipper [zipper]
  (let [directions [z/down z/right]
        rec        (fn rec [prefix node]
                     (mapcat (fn [dir]
                               (if-let [sub-node (dir node)]
                                 (cons (conj prefix dir)
                                       (rec (conj prefix dir) sub-node))))
                             directions))]
    (cons [] (rec [z/down] (z/down zipper)))))


(defn mutants [zipper paths]
  (mapcat
   (fn [path]
     (let [node     (reduce (fn [node dir] (dir node)) zipper path)
           rev-path (map {z/down z/up, z/right z/left} (reverse path))]
       (remove nil?
               (for [mutant (mutate node)]
                 (reduce (fn [node dir] (dir node)) mutant rev-path)))))
   paths))


(defn dependency-graph [directory-names]
  (let [decls (find/find-ns-decls (map jio/file directory-names))]
    (->> decls
         (map (juxt second parse/deps-from-ns-decl))
         (reduce (fn [graph [ns deps]]
                   (reduce #(dep/depend %1 ns %2)
                           graph
                           deps))
                 (dep/graph)))))


(defn candidate-files [git-since]
  (let [files (->> (shell/sh "git" "diff" "--name-only" "--relative" git-since)
                   :out
                   (str/split-lines)
                   (remove (partial str/blank?))
                   (filter #(.exists (jio/file %))))]
    files))


(defn candidate-lines [files git-since]
  (into {}
        (map (fn [file]
               (let [parse (fn [s]
                             (->> (re-find #"@@ -(\d+),?(\d*)\s+\+(\d+),?(\d*) @@" s)
                                  (rest)
                                  (map #(or (and (str/blank? %) 0)
                                            (Integer/parseInt %)))
                                  (zipmap [:old-line :old-count :new-line :new-count])))
                     outputs (->> (shell/sh "git" "diff" "--unified=0" "--patch" git-since file)
                                  :out
                                  (str/split-lines)
                                  (filter #(str/starts-with? % "@@ "))
                                  (map parse)
                                  (mapcat #(vector [(:old-line %) (+ (:old-line %) (:old-count %))]
                                                   [(:new-line %) (+ (:new-line %) (:new-count %))]))
                                  (vec))]
                 {(jio/file file) outputs}))
             files)))


(defn git-diff-ranges [git-since]
  (if git-since
    (candidate-lines (candidate-files git-since) git-since)
    {}))


(defn- dependants [graph ns]
  (letfn [(rec [sym]
            (if-let [deps (seq (dep/immediate-dependents graph sym))]
              (reduce into [] (conj (mapv rec deps) deps))))]
    (reverse (distinct (rec ns)))))


(defn mutate-zippers [zippers]
  (into {}
        (comp (map (fn [z] [(z/string z) z]))
              (map (fn [[form zipper]]
                     [form (mutants zipper (paths-in-zipper zipper))])))
        zippers))


(defn run-ns
  [ns zippers dep-graph test-fn]
  (let [cur-ns (symbol (str *ns*))
        deps (dependants dep-graph ns)
        forms (map z/string zippers)
        zipped (zipmap forms zippers)]
    (for [candidate forms
          mutant (mutants (zipped candidate)
                          (paths-in-zipper (zipped candidate)))]
      (try
        (in-ns ns)
        (doseq [form forms]
          (if (= form candidate)
            (eval (read-string {:read-cond :allow} (z/string mutant)))
            (eval (read-string {:read-cond :allow} form))))
        (doseq [dep deps]
          (require dep :reload))
        (if (test-fn)
          {:survivor {:mutant (z/string mutant)
                      :original candidate
                      :ns ns}}
          {})
        (catch Throwable ex
          {})
        (finally
          (require ns :reload)
          (doseq [dep deps]
            (require dep :reload))
          (in-ns cur-ns))))))
