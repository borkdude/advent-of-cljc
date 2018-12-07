(ns aoc.y2018.d06.mfikes
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d06.data :refer [input answer-1 answer-2]]
   [clojure.string :as string]
   [clojure.test :refer [is testing]]))

(def input-lines (string/split-lines input))

(defn read-coordinates [s]
  (mapv read-string (re-seq #"\d+" s)))

(defn add-vectors [& xs]
  (apply mapv + xs))

(defn unique-min-key
  [k & xs]
  (first (reduce (fn [[min-x min dup?] x]
                   (let [k-x (k x)]
                     (cond
                       (< k-x min) [x k-x false]
                       (== k-x min) [nil k-x true]
                       :else [min-x min dup?])))
           [nil ##Inf false] xs)))

(defn manhattan-distance [a b]
  (reduce + (map (comp #(Math/abs ^long %) -) a b)))

(defn boundary [center extent]
  (let [size (* 2 extent)]
    (into #{(add-vectors center [extent extent])}
      (mapcat (fn [n]
                (map #(add-vectors center [(- extent) (- extent)] %)
                  [[0 n] [size n] [n 0] [n size]]))
        (range 0 size)))))

(defn coordinate-stats [coordinates]
  (let [xs    (map first coordinates)
        ys    (map second coordinates)
        min-x (apply min xs)
        min-y (apply min ys)
        max-x (apply max xs)
        max-y (apply max ys)
        mid   (fn [min max] (quot (+ min max) 2))
        span  (fn [min max] (- max min))]
    [[(mid  min-x max-x) (mid  min-y max-y)]
     [(span min-x max-x) (span min-y max-y)]]))

(defn solve-1 []
  (let [coordinates     (map read-coordinates input-lines)
        [midpoint span] (coordinate-stats coordinates)
        terminal-extent (+ 2 (quot (apply max span) 2))
        closest-coord   (fn [coord]
                          (apply unique-min-key #(manhattan-distance coord %) coordinates))]
    (reduce
      (fn [coord->area extent]
        (let [closest-coords (map closest-coord (boundary midpoint extent))
              coord->area'   (merge-with + coord->area (frequencies closest-coords))]
          (if (== terminal-extent extent)
            (reduced (apply max (vals (apply dissoc coord->area' closest-coords))))
            coord->area')))
      {}
      (range))))

(defn solve-2 []
  (let [coordinates    (map read-coordinates input-lines)
        [midpoint]     (coordinate-stats coordinates)
        distance-sum   (fn [coord]
                         (reduce + (map #(manhattan-distance coord %) coordinates)))
        within-region? (fn [coord]
                         (< (distance-sum coord) 10000))]
    (reduce (fn [area extent]
              (let [additional-area (count (filter within-region? (boundary midpoint extent)))]
                (if (zero? additional-area)
                  (reduced area)
                  (+ area additional-area))))
      0
      (range))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
