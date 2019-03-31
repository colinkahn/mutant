(ns mutant.core-test
  (:require [clojure.test :refer :all]
            [mutant.core :as m]
            [mutant.helpers.clojure-test :refer [test-fn]]
            mutant.core-test.test.test))

(deftest t-run
  (is (= (for [index (range 1 15)]
           {:survivors [], :total index})
         (m/run {:src-paths ["old-test/mutant/core_test/src"]
                 :test-fn   #(test-fn {:regex #"mutant.core-test.test.test"})})))
  (is (= {:survivors [{:original "(defn non-neg? [x]\n  (<= 0 x))"
                       :mutant   "(defn non-neg? [x]\n  (< 0 x))"
                       :ns       'mutant.core-test.src.nums}
                      {:original "(defn non-neg? [x]\n  (<= 0 x))"
                       :mutant   "(defn non-neg? [x])"
                       :ns       'mutant.core-test.src.nums}
                      {:original "(defn all? [xs]\n  (reduce #(and %1 %2) (for [x xs] (boolean x))))"
                       :mutant   "(defn all? [xs]\n  (reduce #(or %1 %2) (for [x xs] (boolean x))))"
                       :ns       'mutant.core-test.src.colls}]

          :total 14}
         (last (m/run {:src-paths ["old-test/mutant/core_test/src"]
                       :test-fn   #(test-fn {:regex #"mutant.core-test.test.incomplete-test"})})))))
