(ns aoc.y2018.d03.transducer
  (:require [aoc.utils :as utils :refer [deftest parse-int]]
            [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
            [clojure.string :as string]
            [clojure.test :refer [is testing]]))

(defn parse [input]
  (->> (string/split-lines input)
       (map #(zipmap [:id :x :y :width :height]
                     (map parse-int (rest (re-find #"#(.*) @.(\d+),(\d+): (\d+)x(\d+)" %)))))))

(def data
  (memoize #(parse input)))

(def grid-width
  (memoize
   #(->> (map (fn [{:keys [x width]}] (+ x width)) (data))
         (apply max))))

(def grid-height
  (memoize
   #(->> (map (fn [{:keys [y height]}] (+ y height)) (data))
         (apply max))))

(defn inc-indices [v idxs]
  (reduce #(update %1 %2 inc) v idxs))

(def grid
  (memoize #(vec (repeat (* (grid-width) (grid-height)) 0))))

(defn square-indices
  "Indices of the square at (`x`, `y`) with width `width` and height `height`."
  [x y width height]
  (apply concat
         (for [n (range y (+ y height))]
           (range (+ x (* n (grid-width)))
                  (+ x (* n (grid-width)) width)))))

(def grid-claim-counts
  "Collection that shows per index the amount of times it is claimed."
  (memoize
   #(reduce
     (fn [grid {:keys [x y width height]}]
       (inc-indices grid (square-indices x y width height)))
     (grid)
     (data))))

(defn solve-1 []
  (->> (grid-claim-counts)
       (filter #(> % 1))
       count))

(defn solve-2 []
  (->> (data)
       (filter (fn [{:keys [x y width height]}]
                 (every? #(= 1 %)
                         (mapv (grid-claim-counts) (square-indices x y width height)))))
       first
       :id))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  ;; NOTE: Advent of Code expects the id without the leading #
  (is (= answer-2 (str (solve-2)))))
