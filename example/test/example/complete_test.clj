(ns example.complete-test
  (:require [clojure.test :refer [deftest is]]
            [example.colls :as colls]
            [example.nums :as nums]))


(deftest t []
  (is (colls/all? [1 () []]))
  (is (not (colls/all? [1 () nil []])))

  (is (nums/naught? 0))
  (is (not (nums/naught? 1)))

  (is (nums/stricly-pos? 1))
  (is (not (nums/stricly-pos? 0)))

  (is (nums/non-neg? 0))
  (is (not (nums/non-neg? -1)))


  (is (= "acelnosz" (colls/naive-transliterate "ąćęłńóśż")))

  )
