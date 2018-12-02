(ns aoc.y2018.d01.mrmcc3
  (:require
    [aoc.utils :as u :refer [deftest]]
    [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(def freqs
  (map u/read-string (str/split-lines input)))

(defn dupe [observed val]
  (if (contains? observed val)
    (reduced val)
    (conj observed val)))

(defn solve-1 []
  (reduce + freqs))

(defn solve-2 []
  (->> (cycle freqs)
       (reductions +)
       (reduce dupe #{0})))

(deftest part-1
         (is (= answer-1 (solve-1))))

(deftest part-2
         (is (= answer-2 (solve-2))))
