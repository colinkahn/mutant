(ns example.incomplete-test
  (:require [clojure.test :refer [deftest is]]
            [example.colls :as colls]
            [example.nums :as nums]))


(deftest t []
  (is (colls/all? [1 () []]))

  (is (nums/naught? 0))

  (is (nums/stricly-pos? 1))

  (is (not (nums/non-neg? -1))))
