(ns example.colls
  (:require [clojure.string :as str]))


(defn all? [xs]
  (reduce #(and %1 %2) (for [x xs] x)))


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
