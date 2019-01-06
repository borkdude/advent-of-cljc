(ns aoc.y2017.d04.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d04.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn valid? [s]
  (->> (s/split s #" ")
       frequencies
       vals
       (into #{})
       (= #{1})))

(defn solve-1 []
  (->> input s/split-lines (filter valid?) count))

(defn valid2? [s]
  (->> (s/split s #" ")
       (map frequencies)
       frequencies
       vals
       (into #{})
       (= #{1})))

(defn solve-2 []
  (->> input s/split-lines (filter valid2?) count))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
)
