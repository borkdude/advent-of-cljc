(ns aos.instrument
  (:require [speculative.instrument :refer [instrument]]))

(println "Instrumenting...")
(println (instrument))
