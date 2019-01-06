(ns aoc.y2017.d06.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d06.data :refer [input answer-1 answer-2]]
   [clojure.test :as t :refer [is testing]]))

(defn distribute
  [xs idx cnt]
  (let [idx (mod idx (count xs))]
    (if (pos? cnt)
      (recur
       (assoc-in xs [idx] (inc (xs idx)))
       (inc idx)
       (dec cnt))
      xs)))

(defn reallocate
  [xs prevs i] ;; banks as a vector, previous vectors, iterations
  (if (prevs xs)
    [(count prevs) i]
    (let [max-val (apply max xs)
          max-idx (first (first (filter
                                 (comp #{max-val} second)
                                 (map-indexed vector xs))))
          new-xs  (distribute (assoc-in xs [max-idx] 0)
                              (inc max-idx)
                              max-val)]
      (recur new-xs (into prevs [xs]) (inc i)))))

(defn read-input []
  (read-string (str "[" input "]")))

(defn solve-1 []
  (first (reallocate (read-input) #{} 0)))

(defn reallocate2
  [xs prevs i] ;; banks as a vector, previous vectors map, iterations
  (if (prevs xs)
    (- i (prevs xs))
    (let [max-val (apply max xs)
          max-idx (first (first (filter
                                 (comp #{max-val} second)
                                 (map-indexed vector xs))))
          new-xs  (distribute (assoc-in xs [max-idx] 0)
                              (inc max-idx)
                              max-val)]
      (recur new-xs (into prevs {xs i}) (inc i)))))

(defn solve-2 []
  (reallocate2 (read-input) {} 0))

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
