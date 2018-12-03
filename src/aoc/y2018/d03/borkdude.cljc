(ns aoc.y2018.d03.borkdude
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as str]))

(def data (str/split-lines input))

(defn parse-line [line]
  (let [[id _ x*y w*d] (str/split line #" ")
        id (subs id 1)
        [x1 y1] (str/split (str/replace x*y #":" "")
                           #",")
        [w d] (str/split w*d #"x")
        [id x1 y1 w d] (map u/parse-int [id x1 y1 w d])
        [x1 y1 x2 y2] [x1 y1 (+ x1 w) (+ y1 d)]]
    (for [x (range x1 x2)
          y (range y1 y2)]
      {:id id :coordinate [x y]})))

(defn parsed* []
  (mapcat parse-line data))

(def parsed (memoize parsed*))

(defn freqs* []
  (let [coordinates (map :coordinate (parsed))]
    (frequencies coordinates)))

(def freqs (memoize freqs*))

(defn solve-1 []
  (count (filter (fn [[_ v]] (>= v 2))
                 (freqs))))

(defn solve-2 []
  (let [only-one (set (keep (fn [[k v]]
                              (when (= 1 v) k))
                            (freqs)))
        by-id (group-by :id (parsed))]
    (reduce (fn [_ [id vs]]
              (let [coordinates (map :coordinate vs)]
                (when (every? only-one coordinates)
                  (reduced id))))
            by-id)))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  ;; NOTE: Advent of Code expects the id without the leading #
  (is (= answer-2 (str (solve-2)))))
