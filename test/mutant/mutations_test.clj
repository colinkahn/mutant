(ns mutant.mutations-test
  (:require [mutant.mutations :as sut]
            [mutant.zip :as z]
            [clojure.test :refer [deftest testing is are]]))


(def form->zipper
  (comp z/of-string pr-str))


(def zipper->form
  z/sexpr)


(defn mutations [op form]
  (->> form
       form->zipper
       (sut/mutate-with op)
       (map zipper->form)
       set))

(defn all-mutations [form]
  (->> form
       form->zipper
       (sut/mutate)
       (map zipper->form)
       set))

(defn check [op sexpr expected-mutations]
  (let [muts (mutations op sexpr)]
    (is (seq muts) (str "No mutations: " (pr-str sexpr)))
    (is (= (set expected-mutations) muts))))


(deftest t-illogical-swap
  (is (= (set '[and or
                > >=
                < <=
                = not=
                boolean not
                empty? seq
                for
                if if-not when when-not
                true false true? false? nil? some?
                + - * /])
         (set (keys sut/illogical-swap-mutations))))
  (doseq [[k vs] sut/illogical-swap-mutations]
    (check sut/illogical-swap k vs)))


(deftest t-random-rename
  (check sut/random-rename :foo [:mutant-kw])
  (check sut/random-rename :mutant-kw [:other-mutant-kw])
  (check sut/random-rename ::foo [::sut/mutant-ns-kw])
  (check sut/random-rename ::sut/mutant-ns-kw [::sut/other-mutant-ns-kw])
  (check sut/random-rename 'foo ['mutant-sym
                                 `sut/ok-sentinel
                                 `sut/not-ok-sentinel])
  (check sut/random-rename 'mutant-sym ['other-mutant-sym
                                        `sut/ok-sentinel
                                        `sut/not-ok-sentinel])
  (check sut/random-rename `foo [`sut/mutant-sym
                                 `sut/ok-sentinel
                                 `sut/not-ok-sentinel])
  (check sut/random-rename `sut/mutant-sym [`sut/other-mutant-sym
                                            `sut/ok-sentinel
                                            `sut/not-ok-sentinel]))


(deftest t-random-rename-def-names
  (is (empty? (->> '(defn foo [] :foo)
                   form->zipper
                   z/next
                   z/next
                   (sut/mutate-with sut/random-rename)
                   (map zipper->form)))))


(deftest t-swap-values
  (check sut/swap-values 42 [-42 0])
  (check sut/swap-values -42 [42 0])
  (check sut/swap-values 0 [1 -1]))


(deftest t-replace-with-nil
  (check sut/replace-with-nil :foo [nil])
  (check sut/replace-with-nil ::foo [nil])
  (check sut/replace-with-nil 'foo [nil])
  (check sut/replace-with-nil `foo [nil]))


(deftest t-noop-swap!
  (check sut/noop-swap!
         '(swap! s update :counter inc)
         '[(update (clojure.core/deref s) :counter inc)]))


(deftest t-noop-reset!
  (check sut/noop-reset!
         '(reset! s 42)
         '[42]))


(deftest t-random-re-pattern
  (letfn [(check [sexpr expected-mutations]
            (let [muts (mutations sut/random-re-pattern sexpr)]
              (is (seq muts) (str "No mutations: " (pr-str sexpr)))
              (is (every? #(instance? java.util.regex.Pattern %) muts))
              (is (= (set (map str expected-mutations))
                     (set (map str muts))))))]
    (check #"foo" [#"mutant-regex"])
    (check #"mutant-regex" [#"other-mutant-regex"])))


(deftest t-rm-args
  (check sut/rm-args
         '(defn foo [x y] (+ x y))
         '[(defn foo [x] (+ x y))
           (defn foo [y] (+ x y))]))


(deftest t-rm-args
  (check sut/rm-args
         '(defn foo [x y] (+ x y))
         '[(defn foo [x] (+ x y))
           (defn foo [y] (+ x y))]))


(deftest t-rm-fn-body
  (check sut/rm-fn-body
         '(defn inv [x] (- x))
         '[(defn inv [x])]))
