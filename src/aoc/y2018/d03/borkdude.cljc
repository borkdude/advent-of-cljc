(ns aoc.y2018.d03.borkdude
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as str]))

(def data (str/split-lines input))

(def re #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

(defn parse-line [line]
  (let [parse (rest (re-find re line))
        [id x1 y1 w d] (map u/parse-int parse)
        [x1 y1 x2 y2] [x1 y1 (+ x1 w) (+ y1 d)]]
    (for [x (range x1 x2)
          y (range y1 y2)]
      {:id id :coordinate [x y]})))

(def parsed (memoize #(mapcat parse-line data)))

(def freqs (memoize #(frequencies (map :coordinate (parsed)))))

(defn solve-1 []
  (count (filter (fn [[_ v]] (>= v 2)) (freqs))))

(defn solve-2 []
  (let [only-one (set (keep (fn [[k v]]
                              (when (= 1 v) k))
                            (freqs)))
        by-id (group-by :id (parsed))]
    (reduce (fn [_ [id vs]]
              (when (every? only-one (map :coordinate vs))
                (reduced id)))
            by-id)))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (str (solve-2)))))
