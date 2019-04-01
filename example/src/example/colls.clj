(ns example.colls
  (:require [clojure.string :as str]))


(defn all? [xs]
  (reduce #(and %1 %2) (for [x xs] x)))


(defn mutate-swap [s]
  (swap! s update :counter inc))


(defn mutate-reset [s]
  (reset! s 42))


(defn naive-transliterate [input]
  (when-not (str/blank? input)
    (reduce (fn [s [pat repl]]
              (str/replace s pat repl))
            input
            [[#"[ąĄ]" "a"]
             [#"[ćĆ]" "c"]
             [#"[ęĘ]" "e"]
             [#"[łŁ]" "l"]
             [#"[ńŃ]" "n"]
             [#"[óÓ]" "o"]
             [#"[śŚ]" "s"]
             [#"[żźŻŹ]" "z"]])))
