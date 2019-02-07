(ns aoc.instrument
  (:require [speculative.instrument :refer [instrument]]
            ;; #?(:clj [orchestra.spec.test :as stest]
            ;;   :cljs [orchestra-cljs.spec.test :as stest])
            [clojure.test :as t :refer [run-tests]]))

(defmethod clojure.test/report [:cljs.test/default :begin-test-var] [m]
  ;; for debugging:
  ;; (println ":begin-test-var" (cljs.test/testing-vars-str m))
  )

(println "Instrumenting...")
;; (println (stest/instrument))
(println (instrument))

;; NOTE: to instrument with orchestra, uncomment the above lines.
