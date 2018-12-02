(ns aoc.y2018.d01.dfornika
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(def data
  (map u/read-string
       (str/split-lines input)))

(defn solve-1 []
  (apply + data))

(defn solve-2 []
  (loop [input (cycle data)
         observed-frequencies #{}
         current-frequency 0]
    (if (not (contains? observed-frequencies current-frequency))
      (recur (rest input)
             (conj observed-frequencies current-frequency)
             (+ current-frequency (first input)))
      current-frequency)))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
