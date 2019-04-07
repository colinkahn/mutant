(ns mutant.tester.report
  (:require [eftest.report :as report]
            [mutant.log :as log]
            [clojure.test :as test]))


(defn set-state [old-state new-state]
  (case [old-state new-state]
    [nil   :pass]  :pass
    [nil   :fail]  :fail
    [nil   :error] :error
    [:pass :fail]  :fail
    [:pass :error] :error
    [:fail :error] :error
    old-state))


(def fail-fast-ex
  "Throw an exception to quickly bail out of a test, on first failure.
   Otherwise, we would run all remaing `(is ..)` assertions in that
   test, even if :fail-fast? was true. If we want to get rid of this,
   we need to customize how `eftest/runner` calls `clojure.test/test-var`."
  (ex-info "Test assertion failure" {}))


(defmulti report :type)


(defmethod report :pass [m]
  (swap! report/*context* update-in [:state] set-state :pass)
  (test/inc-report-counter :pass))


(defmethod report :fail [m]
  (swap! report/*context* update-in [:state] set-state :fail)
  (test/inc-report-counter :fail)
  (throw (fail-fast-ex)))


(defmethod report :error [m]
  (swap! report/*context* update-in [:state] set-state :error)
  (test/inc-report-counter :error)
  (throw (fail-fast-ex)))


(defmethod report :long-test [{:keys [duration]}]
  ;; TODO: how to detect current test correctly?
  ;; (log/warn "Long Test:" (pr-str report/*testing-path*))
  (when duration
    (log/warn "Test took" (/ duration 1000) "seconds to run")))


(defmethod report :default [m]
  nil)
