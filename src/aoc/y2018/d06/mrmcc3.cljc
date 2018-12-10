(ns aoc.y2018.d06.mrmcc3
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d06.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(defn parse-line [s]
  (u/read-string (str "[" s "]")))

(defn out-of-bounds? [[min-x min-y max-x max-y] [x y]]
  (not (and (<= min-x x max-x) (<= min-y y max-y))))

(defn closest [[[p1 d1] [_ d2]]]
  (when-not (= d1 d2) p1))

(defn manhattan-distance [[^long x1 ^long y1] [^long x2 ^long y2 :as p]]
  [p (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))])

(defn process-square [points bounds square]
  (let [dist (map (partial manhattan-distance square) points)]
    {:out-of-bounds (out-of-bounds? bounds square)
     :closest       (closest (sort-by second dist))
     :safe          (< ^long (reduce + (map second dist)) 10000)}))

(def result
  (delay
    (let [points  (map parse-line (str/split-lines input))
          min-x   (apply min (map first points))
          min-y   (apply min (map second points))
          max-x   (apply max (map first points))
          max-y   (apply max (map second points))
          width   (+ (- max-x min-x) 3)
          height  (+ (- max-y min-y) 3)
          squares (for [x (range width) y (range height)]
                    [(+ x min-x -1) (+ y min-y -1)])
          bounds  [min-x min-y max-x max-y]
          process (partial process-square points bounds)]
      (#?(:clj pmap :cljs map) process squares))))

(defn solve-1 []
  (let [infinite
        (->> (filter :out-of-bounds @result)
             (map :closest)
             (remove nil?)
             (into #{}))]
    (->> (map :closest @result)
         (remove nil?)
         (remove infinite)
         frequencies
         (map second)
         (apply max))))

(defn solve-2 []
  (count (filter :safe @result)))

(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest part-2
         (is (= (str answer-2)
                (str (solve-2)))))