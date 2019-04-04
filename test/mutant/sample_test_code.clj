(ns mutant.sample-test-code.clj
  (:require [clojure.spec.alpha :as s]))


(defn naught? [x]
  (= 0 x))

(comment
  ;; comments should be ignored
  (defn answer [] 43))

(comment (defn answer [] 44))

;; (def answer 45)

(defn answer []
  #_(identity 46)
  42
  (comment
    ;; comments should be ignored
    (+ 0 47)))

(s/def ::integer int?)
