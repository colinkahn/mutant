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


(defn- file [^String name]
  (java.io.File. name))


(defn namespaces
  "Returns a map from files to namespaces in a given directory."
  [directory-name]
  (into {}
        (map (juxt identity (comp second file/read-file-ns-decl)))
        (find/find-clojure-sources-in-dir (file directory-name))))


(defn dependency-graph [directory-names]
  (let [decls (find/find-ns-decls (map file directory-names))]
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
                 {file outputs}))
             files)))


(defn make-git-diff [git-since]
  (when git-since
    (candidate-lines (candidate-files git-since) git-since)))


(defn count-lines [file]
  (with-open [rdr (jio/reader file)]
    (-> rdr line-seq count)))


(defn allow-candidate? [git-diff file node]
  (or (empty? git-diff)
      (let [touches? (fn [[def-start def-stop] [diff-start diff-stop]]
                       (or (<= def-start diff-start def-stop)
                           (<= def-start diff-stop def-stop)
                           (<= diff-start def-start diff-stop)))
            start    (-> node (z/position) first)
            stop     (or (and (-> node (z/right))
                              (-> node (z/right) (z/position) first))
                         (count-lines file))]
        (boolean (some #(touches? [start stop] %)
                       (get git-diff (str (.toPath file))))))))


(defn forms
  "Return a collection of zippers for top-level sexprs found in a given file."
  [git-diff file]
  (->> (z/of-file file {:track-position? true})
       (iterate z/right)
       (remove (comp #{'ns} z/sexpr))
       (remove (comp #{'ns} z/sexpr z/down))
       (take-while boolean)
       (filter (partial allow-candidate? git-diff file))))

(defn- dependants [graph ns]
  (letfn [(rec [sym]
            (if-let [deps (seq (dep/immediate-dependents graph sym))]
              (reduce into [] (conj (mapv rec deps) deps))))]
    (reverse (distinct (rec ns)))))

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
