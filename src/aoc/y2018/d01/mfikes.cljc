(ns aoc.y2018.d01.mfikes
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d01.data :refer [input answer-1 answer-2] :rename {input input-str}]
   [clojure.string :as string]
   [clojure.test :refer [is testing]]))

(def input (string/split-lines input-str))

(def data (map u/read-string input))

(defn solve-1 []
  (reduce + data))

(defn first-duplicate [xs]
  (let [result (reduce (fn [seen x]
                         (if (seen x)
                           (reduced x)
                           (conj seen x)))
                 #{} xs)]
    (if (set? result)
      nil
      result)))

(defn solve-2 []
  (let [freqs (cons 0 (reductions + (cycle data)))]
    (first-duplicate freqs)))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
