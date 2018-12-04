(ns aoc.y2018.d01.transducer
  (:require [aoc.utils :as utils :refer [deftest]]
            [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
            [clojure.string :as string]
            [clojure.test :refer [is testing]]))

(def data
  (->> input
       (utils/format "[%s]")
       utils/read-string))

(defn solve-1 []
  (apply + data))

(defn solve-2 []
  (let [freqs-seen (reductions + 0 (cycle data))]
    (->> (distinct freqs-seen)
         (map (fn [a b] (when (not= a b) a)) freqs-seen)
         (remove nil?)
         first)))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
