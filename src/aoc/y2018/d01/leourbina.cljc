(ns aoc.y2018.d01.leourbina
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

(defn parse-data [input]
  (map u/read-string
    (clojure.string/split input #"\n")))

(defn compute-frequency [input]
  (->> input
    parse-data
    (apply +)))

(defn solve-1 []
  (compute-frequency input))

(defn get-repeated-freq [input]
  (loop [frequencies (->> input parse-data cycle (reductions + 0))
         seen-freqs #{}]
    (let [[freq & rest] frequencies]
      (if (contains? seen-freqs freq)
        freq
        (recur rest (conj seen-freqs freq))))))

(defn solve-2 []
  (get-repeated-freq input))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
