(ns mutant.tester.core
  (:require [eftest.runner :as eftest]
            [mutant.tester.report :as report]
            [mutant.log :as log]))


(def default-test-warn-time 1000)

(def default-suite-warn-time 5000)


(defn find-tests
  "Find test vars specified by a source. The source may be a var, symbol
  namespace or directory path, or a collection of any of the previous types."
  [source]
  (eftest/find-tests "test"))


(defn run-tests
  "Run the supplied test vars. Accepts the following options:
    :multithread?    - one of: true, false, :namespaces or :vars (defaults to
                       true). If set to true, namespaces and vars are run in
                       parallel; if false, they are run in serial. If set to
                       :namespaces, namespaces are run in parallel but the vars
                       in those namespaces are run serially. If set to :vars,
                       the namespaces are run serially, but the vars inside run
                       in parallel.
    :thread-count    - the number of threads used to run the tests in parallel
                       (as per :multithread?). If not specified, the number
                       reported by java.lang.Runtime.availableProcessors (which
                       is not always accurate) *plus two* will be used.
    :test-warn-time  - print a warning for any test that exceeds this time
                       (measured in milliseconds)"
  ([vars] (run-tests vars {}))
  ([vars opts]
   (let [{:keys [pass fail error duration]}
         (eftest/run-tests
          vars
          (merge {:capture-output? false
                  :multithread?    false
                  :fail-fast?      true
                  :report          report/report
                  :test-warn-time  default-test-warn-time}
                 (select-keys opts [:multithread?
                                    :thread-count
                                    :test-warn-time])))
         total (+ pass fail error)]
     (when (> duration default-suite-warn-time)
       (log/warn "Test suite took" (/ duration 1000) "seconds."))
     (assert (> total 0) (str "ERROR: No tests were found. Checking: " (pr-str vars)))
     (zero? (+ fail error)))))
