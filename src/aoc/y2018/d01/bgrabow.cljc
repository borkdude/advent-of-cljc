(ns aoc.y2018.d01.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
    [clojure.string :as str]
    [clojure.test :refer [is testing]]))

(defn first-duplicate [coll]
  (loop [seen-values #{}
         remaining-values coll]
    (cond
      (empty? remaining-values) nil
      (seen-values (first remaining-values)) (first remaining-values)
      :else (recur (conj seen-values (first remaining-values)) (rest remaining-values)))))

(defn solve-1 []
  (reduce + (map u/read-string (str/split-lines input))))

(defn solve-2 []
  ;; Day 01 - Part 2
  ;; Cycle the changes in frequency and find the first
  ;; intermediate sum that appears more than once.
  (first-duplicate
    (reductions +
                (cycle
                  (map u/read-string (str/split-lines input))))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
