(ns mutant.core
  (:require [mutant.internals :as mi]
            [jansi-clj [core :as jansi] auto]
            [clj-diffmatchpatch :as dmp]))


(defn run [{:keys [:src-paths :test-fn :git-since]}]
  (let [dep-graph     (mi/dependency-graph src-paths)
        file->ranges  (mi/git-diff-ranges git-since)
        file->ns      (mi/dirs-to-namespaces src-paths)
        file->zippers (reduce
                       (fn [m file]
                         (assoc m file
                                (mi/file-to-zippers file {:line-ranges (file->ranges file)})))
                       {} (keys file->ns))]
    (->> file->ns
         (mapcat (fn [[file ns]]
                   (mi/run-ns ns (file->zippers file) dep-graph test-fn)))
         (reductions (fn [{:keys [total survivors]} {:keys [survivor]}]
                       {:survivors (if survivor
                                     (cons survivor survivors)
                                     survivors)
                        :total     (inc total)})
                     {:survivors ()
                      :total     0})
         (rest))))


(defn pprint [{:keys [survivors total]}]
  (printf "%s out of %s mutants\n\n"
          (if (seq survivors)
            (jansi/red (count survivors) " survivors")
            "No survivors")
          total)
  (doseq [{:keys [mutant original ns]} survivors]
    (printf "(ns %s)\n" ns)
    (doseq [[op chunk] (dmp/wdiff original mutant)]
      (print (case op
               :equal  chunk
               :insert (jansi/green "{+" chunk "+}")
               :delete (jansi/red   "[-" chunk "-]"))))
    (print "\n\n")))
