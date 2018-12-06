(ns aoc.y2018.d06.borkdude
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d06.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(defn parse-coordinate [s]
  (mapv u/parse-int (rest (re-find #"(\d+), (\d+)" s))))

(def data (map parse-coordinate
               (str/split-lines input)))

(def max-x (delay (apply max (map #(nth % 0) data))))
(def max-y (delay (apply max (map #(nth % 1) data))))

(def coordinates
  (for [x (range 0 (inc ^long @max-x))
        y (range 0 (inc ^long @max-y))]
    [x y]))

(defn manhattan [[^long x ^long y] [^long x* ^long y*]]
  (+ (Math/abs (- x x*))
     ( Math/abs (- y y*))))

(defn closest-to [[x y]]
  (let [closest
        (sort-by first
                 (map (fn [[x* y*]]
                        [(manhattan [x y] [x* y*]) [x* y*]])
                      data))]
    (when (not= (first (first closest))
                (first (second closest)))
      [(second (first closest)) [x y]])))

(defn infinite-area? [area]
  (some (fn [[^long x ^long y]]
          (or (zero? x)
              (zero? y)
              (>= x ^long @max-x)
              (>= y ^long @max-y))) area))

(defn solve-1 []
  (let [groups (group-by first (keep closest-to coordinates))]
    (apply max (keep #(when (not (infinite-area? (map second %)))
                        (count %)) (vals groups)))))

(defn solve-2 []
  (count
   (filter #(< ^long (reduce + (map (partial manhattan %) data))
               10000)
           coordinates)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true)
  )
