(ns aos.y2017.d02.borkdude
  (:require
   [aos.utils :as u]
   [aos.y2017.d02.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(defn solve-1 []
  (transduce
   (comp
    (map #(str/split % #"\s"))
    (map #(map u/parse-int
               %))
    (map (fn [row]
           [(apply max row)
            (apply min row)]))
    (map (fn [[max min]]
           (- max min))))
   +
   (str/split-lines input)))

(defn find-divisibles [nums]
  (let [desc (sort-by - nums)
        asc  (sort nums)]
    (for [greater desc
          smaller asc
          :while (> greater smaller)
          :when (zero? (mod greater smaller))]
      [greater smaller])))

(defn solve-2 []
  (transduce
   (comp
    (map #(str/split % #"\s"))
    (map #(map u/parse-int
               %))
    (map (fn [row]
           (first (find-divisibles row))))
    (map (fn [[greater smaller]]
           (/ greater smaller))))
   +
   (str/split-lines input)))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))


