(ns aoc.instrument
  (:require [speculative.instrument :refer [instrument]]
            [clojure.test :as t :refer [run-tests]]))

(defmethod clojure.test/report [:cljs.test/default :begin-test-var] [m]
  ;; for debugging:
  ;; (println ":begin-test-var" (cljs.test/testing-vars-str m))
  )

(println "Instrumenting...")
(println (instrument))
