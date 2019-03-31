(ns mutant.test-runner
  (:require [clojure.tools.namespace.find :as find]
            [clojure.tools.cli :as tools.cli]
            [mutant.core :as mutant]
            [mutant.helpers.clojure-test :as clojure-test])
  (:import (java.io File))
  (:refer-clojure :exclude [test]))


(defn survivor? [prev curr]
  (not= (-> prev :survivors first)
        (-> curr :survivors first)))


(defn report
  ([] {})
  ([prev curr]
   (print (if (survivor? prev curr) \x \.))
   (flush)
   curr))


(defn final-report [result]
  (print "\n\n")
  (mutant/pprint result)
  (flush))


(defn require-tests
  [{:keys [:test-paths]}]
  (let [test-namespaces (->> test-paths
                             (mapcat #(find/find-namespaces-in-dir (File. %))))]
    (run! require test-namespaces)))


(defn run-mutation
  [{:keys [:src-paths :git-since]}]
  (binding [*out* *err*]
    (->> (mutant/run {:src-paths src-paths
                      :test-fn   clojure-test/test-fn
                      :git-since git-since})
         (reduce report)
         final-report))
  (shutdown-agents))


(defn test [options]
  (println "Running tests!")
  (require-tests options)
  (assert (clojure-test/test-fn {:silent? true}) "Tests are not green")
  (run-mutation options))


(defn- accumulate [m k v]
  (update-in m [k] (fnil conj #{}) v))


(def cli-options
  [["-t" "--test DIRNAME" "Name of the directory containing tests. Defaults to \"test\"."
    :id       :test-paths
    :parse-fn str
    :assoc-fn accumulate]
   ["-s" "--src  DIRNAME" "Name of the directory containing sources. Defaults to \"src\"."
    :id       :src-paths
    :parse-fn str
    :assoc-fn accumulate]
   ["-c" "--since  SHA" "Git SHA to start mutations from."
    :id       :git-since
    :parse-fn str]
   ["-H" "--test-help" "Display this help message"]])


(defn- help
  [args]
  (println "\nUSAGE:\n")
  (println "clj -m" (namespace `help) "<options>\n")
  (println (:summary args))
  (println "\nAll options may be repeated multiple times for a logical OR effect."))


(defn -main
  "Entry point for the test runner"
  [& args]
  (let [args (tools.cli/parse-opts args cli-options)]
    (if (:errors args)
      (do (doseq [e (:errors args)]
            (println e))
          (help args))
      (if (-> args :options :test-help)
        (help args)
        (try
          ;; (let [{:keys [fail error]} (test (:options args))]
          ;;   (System/exit (if (zero? (+ fail error)) 0 1)))
          (test (:options args))
          (finally
            ;; Only called if `test` raises an exception
            (shutdown-agents)))))))
