(ns example.incomplete-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.spec.alpha :as s]
            [example.colls :as colls]
            [example.nums :as nums]))


(deftest t []
  (is (colls/all? [1 () []]))

  (is (nums/naught? 0))

  (is (nums/stricly-pos? 1))

  (is (not (nums/non-neg? -1)))

  (is (= "acelnosz" (colls/naive-transliterate "ąćęłńóśż")))

  (is (s/valid? ::nums/integer 42))

  (let [state (atom {:counter 0})]
    (is (= {:counter 1} (colls/mutate-swap state))))

  (let [state (atom 0)]
    (is (= 42 (colls/mutate-reset state))))

  )
