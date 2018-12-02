(ns aoc.y2018.d02.dfuenzalida
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(def data
  (str/split-lines input))

(defn letter-counts [s]
  (->> s frequencies vals (apply hash-set)))

(defn checksum [ws]
  (let [counts (map letter-counts ws)
        twos   (count (filter #(some #{2} %) counts))
        threes (count (filter #(some #{3} %) counts))]
    (* twos threes)))

(defn diff-by-1? [a b]
  (->> (map = a b)
       (filter false?)
       count
       (= 1)))

(defn common-letters [ws]
  (let [[w1 w2] (first
                 (for [x ws, y ws, :when (diff-by-1? x y)]
                   [x y]))]    
    (->> (map (fn [a b] [(= a b) a]) w1 w2)
         (filter first)
         (map second)
         (apply str))))

(defn solve-1 []
  (checksum data))

(defn solve-2 []
  (common-letters data))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
