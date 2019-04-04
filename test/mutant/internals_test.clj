(ns mutant.internals-test
  (:require [mutant.internals :as sut]
            [mutant.zip :as z]
            [clojure.java.io :as jio]
            [clojure.test :refer [deftest testing is are]]))


(deftest t-forms
  (let [file (jio/file "test/mutant/sample_test_code.clj")]
    (is (= '[(defn naught? [x] (= 0 x))
             ;; (comment (defn answer [] 43))
             ;; (comment (defn answer [] 44))
             (defn answer [] 42 (comment (+ 0 47)))
             (s/def :user/integer int?)]
           (->> (sut/forms nil file)
                (map z/sexpr))))))


(deftest t-mutants
  (let [file    (jio/file "test/mutant/sample_test_code.clj")
        zippers (sut/forms nil file)]
    (is (= [["(defn naught? [x])"
             "(defn naught? []\n  (= 0 x))"
             "(defn naught? [nil]\n  (= 0 x))"
             "(defn naught? [mutant-sym]\n  (= 0 x))"
             "(defn naught? [mutant.mutations/ok-sentinel]\n  (= 0 x))"
             "(defn naught? [mutant.mutations/not-ok-sentinel]\n  (= 0 x))"
             "(defn naught? [x]\n  (not= 0 x))"
             "(defn naught? [x]\n  (nil 0 x))"
             "(defn naught? [x]\n  (mutant-sym 0 x))"
             "(defn naught? [x]\n  (mutant.mutations/ok-sentinel 0 x))"
             "(defn naught? [x]\n  (mutant.mutations/not-ok-sentinel 0 x))"
             "(defn naught? [x]\n  (= -1 x))"
             "(defn naught? [x]\n  (= 1 x))"
             "(defn naught? [x]\n  (= 0 nil))"
             "(defn naught? [x]\n  (= 0 mutant-sym))"
             "(defn naught? [x]\n  (= 0 mutant.mutations/ok-sentinel))"
             "(defn naught? [x]\n  (= 0 mutant.mutations/not-ok-sentinel))"]
            ["#_(identity 46)"
             "(defn answer []\n  #_(identity 46)\n  -42\n  (comment\n    ;; comments should be ignored\n    (+ 0 47)))"
             "(defn answer []\n  #_(identity 46)\n  0\n  (comment\n    ;; comments should be ignored\n    (+ 0 47)))"]
            ["(s/def ::integer nil)"
             "(s/def ::integer mutant-sym)"
             "(s/def ::integer mutant.mutations/ok-sentinel)"
             "(s/def ::integer mutant.mutations/not-ok-sentinel)"]]
           (mapv
            (fn [z]
              (mapv #(z/string %)
                   (sut/mutants z (sut/paths-in-zipper z))))
            zippers)))))
