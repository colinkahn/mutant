(ns mutant.mutations-test
  (:require [mutant.mutations :as sut]
            [rewrite-clj.zip :as z]
            [clojure.test :refer [deftest testing is are]]))


(def ^:private form->zipper
  (comp z/of-string pr-str))


(def ^:private zipper->form
  z/sexpr)


(defn- mutations [form]
  (->> form
       form->zipper
       sut/mutate
       (map zipper->form)
       set))


(deftest t-rm-fn-body
  (is (contains? (mutations '(defn inv [x] (- x)))
                 '(defn inv [x])))
  (is (contains? (mutations '(defn inc-f [] #(inc %)))
                 '(defn inc-f [])))
  (are [form] (contains? (mutations '(defn inv [x] (prn x) (- x)))
                         form)
    '(defn inv [x] (prn x))
    '(defn inv [x] (- x))))


(deftest t-random-keyword
  (are [input output] (contains? (mutations input) output)
    :asdf :foo
    :foo :bar))
