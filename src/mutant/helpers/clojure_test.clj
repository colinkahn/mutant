(ns mutant.helpers.clojure-test
  (:require clojure.test))

(defn- silently
  "Invoke function f suppressing clojure.test's output."
  [f]
  (binding [clojure.test/*test-out* (java.io.StringWriter.)]
    (f)))

(defn test-fn
  "Invokes clojure.test/run-all-tests with an optional regex filtering test
  namespaces. clojure.test's is suppressed. Returns true iff all tests passed."
  ([]
   (test-fn {}))
  ([{:keys [:regex :silent?]
     :or   {regex   #".*"
            silent? true}}]
   (let [wrapper              (if silent? silently #(%))
         {:keys [fail error]} (wrapper #(clojure.test/run-all-tests regex))]
     (zero? (+ fail error)))))
