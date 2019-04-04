(ns mutant.zip
  "Extra zipper helpers."
  (:require [rewrite-clj.zip :as rzip])
  (:refer-clojure :exclude [remove replace list? next]))


(def down rzip/down)
(def edit rzip/edit)
(def end? rzip/end?)
(def list? rzip/list?)
(def of-file rzip/of-file)
(def of-string rzip/of-string)
(def position rzip/position)
(def remove rzip/remove)
(def replace rzip/replace)
(def sexpr rzip/sexpr)
(def string rzip/string)
(def up rzip/up)


(defn zip-skip-ws
  "Skip whitespace, comments, and uneval nodes."
  [loc next-fn skip-fn end?-fn]
  (loop [loc loc]
    (cond
      (nil? loc) loc
      (end?-fn loc) loc

      (#{:comment :whitespace :newline :comma} (rzip/tag loc))
      (recur (next-fn loc))

      (= :uneval (rzip/tag loc))
      (recur (skip-fn loc))


      (and (some-> loc rzip/list?)
           (= 'comment (some-> loc rzip/down rzip/value)))
      (recur (skip-fn loc))

      :else loc)))


(defn left
  "Like [[rewrite-clj.zip/left], but also skip over uneval nodes"
  [loc]
  (some-> loc rzip/left (zip-skip-ws rzip/left rzip/left (constantly false))))


(defn right
  "Like [[rewrite-clj.zip/right]], but also skip over uneval nodes"
  [loc]
  (some-> loc rzip/right (zip-skip-ws rzip/right rzip/right (constantly false))))


(defn next
  "Like [[rewrite-clj.zip/next]], but also skip over uneval nodes"
  [loc]
  (some-> loc rzip/next (zip-skip-ws rzip/next rzip/right rzip/end?)))


(defn prewalk-subtree
  [p? f zloc]
  (loop [loc zloc]
    (if (end? loc)
      loc
      (if (p? loc)
        (if-let [n (f loc)]
          (recur (next n))
          (recur (next loc)))
        (recur (next loc))))))


(defn prewalk
  "Perform a depth-first pre-order traversal starting at the given zipper location
   and apply the given function to each child node. If a predicate `p?` is given,
   only apply the function to nodes matching it."
  ([zloc f] (prewalk zloc (constantly true) f))
  ([zloc p? f]
   (if-let [new-node (prewalk-subtree p? f zloc)]
     (rzip/subedit-node zloc (fn [_] new-node))
     (rzip/remove zloc))))
