(ns example.nums
  (:require [clojure.spec.alpha :as s]))


(defn naught? [x]
  (= 0 x))


(defn non-neg? [x]
  (<= 0 x))


(defn stricly-pos? [x]
  (> x 0))


(s/def ::integer int?)
