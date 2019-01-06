(ns aoc.y2017.d01.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d01.data :refer [input answer-1 answer-2]]
   [clojure.test :as t :refer [is testing]]))

(defn solve-1 []
  (let [s            input
        s2           (take (count s) (drop 1 (cycle s)))
        matching     (filter some? (map (fn [x y] (#{x} y)) s s2))
        digit-to-int (into {} (map-indexed (fn [i c] [c i]) "0123456789"))]
    (reduce + (map digit-to-int matching))))

(defn solve-2 []
  (let [s            input
        half         (int (/ (count s) 2))
        s2           (take (count s) (drop half (cycle s)))
        matching     (filter some? (map (fn [x y] (#{x} y)) s s2))
        digit-to-int (into {} (map-indexed (fn [i c] [c i]) "0123456789"))]
    (reduce + (map digit-to-int matching))))

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
