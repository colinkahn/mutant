(ns mutant.log)

(defmacro info [& args]
  `(println ~@args))

(defmacro warn [& args]
  `(println "WARNING:" ~@args))

(defmacro error [& args]
  `(println "ERROR:" ~@args))
