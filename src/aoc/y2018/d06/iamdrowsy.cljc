(ns aoc.y2018.d06.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d06.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]
    [com.rpl.specter :as s]))

#?(:clj (set! *unchecked-math* :warn-on-boxed))

(defn my-pmap [f coll]
  #?(:clj (pmap f coll)
     :cljs (map f coll)))

(defn metric [[^long x1 ^long y1] [^long x2 ^long y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn norm [[^long x ^long y]]
  (+ x y))

(defn read-points []
   (map (fn [line]
          (let [[_ x y] (re-matches #"\s*(\d+),\s*(\d+)" line)]
            [(u/parse-int x) (u/parse-int y)]))
        (str/split-lines input)))

(defn left+right+top+bottom [points]
  (let [min+max (juxt (partial apply min)
                      (partial apply max))
        gap 1
        [left right] (min+max (s/select [s/ALL s/FIRST] points))
        [top bottom] (min+max (s/select [s/ALL s/LAST] points))]
    [(- ^long left gap) (+ ^long right gap)
     (- ^long top gap) (+ ^long bottom gap)]))

(defn closest-point [point points]
  (let [[a b] (take 2 (sort-by (partial metric point) points))]
    (if (= (metric point a) (metric point b))
      nil
      a)))

(defn border-points [width closest-field]
  (let [rows (partition width closest-field)]
    (into #{} (concat (first rows) (last rows)
                      (map first rows) (map last rows)))))

(defn build-input []
  (let [points (read-points)
        [left right top bottom] (left+right+top+bottom points)]
    {:field (for [y (range top bottom)
                  x (range left right)]
              [x y])
     :width (- ^long right ^long left)
     :points points}))

(def mem-input
  (memoize build-input))

(defn solve-1 []
  (let [input (mem-input)
        points (:points input)
        field (map #(closest-point % points) (:field input))
        infinite-points (border-points (:width input) field)]
    (->> field
         (remove infinite-points)
         (frequencies)
         (vals)
         (apply max))))

(defn solve-2 []
  (let [input (mem-input)]
    (count (filter #(< ^long (reduce + (map (partial metric %) (:points input)))
                       10000)
                   (:field input)))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
