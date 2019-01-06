(ns aoc.y2017.d02.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d02.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn read-input []
  (->> (s/split-lines input)
       (map #(str "[" % "]"))
       (mapv read-string)))

(defn solve-1 []
  (let [seqs (read-input)]
    (reduce +
            (map - (map #(apply max %) seqs)
                   (map #(apply min %) seqs)))))

(defn check-row [xs]
  (first (for [a (range (count xs))
               b (range (count xs))
               :let [xa (xs a)
                     xb (xs b)]
               :when (and (not= a b)            ;; a and b are not the same index
                          (zero? (rem xa xb)))] ;; numbers divide evenly
           (int (/ xa xb)))))

(defn solve-2 []
  (let [seqs (read-input)]
    (reduce + (map check-row seqs))))

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
