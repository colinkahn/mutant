(ns example.complete-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.spec.alpha :as s]
            [example.colls :as colls]
            [example.nums :as nums]))


(deftest t []
  (is (colls/all? [1 () [] ""]))
  (is (not (colls/all? [1 () nil [] ""])))

  (is (nums/naught? 0))
  (is (not (nums/naught? 1)))

  (is (nums/stricly-pos? 1))
  (is (not (nums/stricly-pos? 0)))

  (is (nums/non-neg? 0))
  (is (nums/non-neg? 1))
  (is (not (nums/non-neg? -1)))

  (is (= "acelnosz" (colls/naive-transliterate "ąćęłńóśż")))
  (is (= nil (colls/naive-transliterate " ")))

  (is (s/valid? ::nums/integer 42))
  (is (not (s/valid? ::nums/integer "42")))

  (let [state (atom {:counter 0})]
    (is (= {:counter 1} (colls/mutate-swap state)))
    (is (= {:counter 2} (colls/mutate-swap state))))

  (let [state (atom 0)]
    (is (= 42 (colls/mutate-reset state)))
    (is (= 42 @state)))
  )
