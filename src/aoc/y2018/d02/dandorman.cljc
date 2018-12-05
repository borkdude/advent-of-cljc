(ns aoc.y2018.d02.dandorman
  (:require
   [aoc.utils :refer [deftest]]
   [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is]]))

(defn solve-1 [box-ids]
  (let [letter-freqs (->> box-ids
                          (map frequencies)
                          (mapcat (comp set vals))
                          frequencies)
        twos&threes (select-keys letter-freqs [2 3])]
    (apply * (vals twos&threes))))

(defn pairs [box-ids]
  (->> (take-while seq (iterate #(drop 1 %) box-ids))
       (mapcat (fn [[fst & rst]] (map #(vector fst %) rst)))
       (map #(apply map vector %))))

(defn lev [pair]
  (->> pair
       (remove (partial apply =))
       count))

(defn solve-2 [box-ids]
  (let [pairs (pairs box-ids)
        match (first (filter #(= 1 (lev %)) pairs))]
    (->> match
         (filter (partial apply =))
         (map first)
         (apply str))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1 (str/split-lines input))))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2 (str/split-lines input))))))
