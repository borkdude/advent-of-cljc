(ns aoc.y2018.d11.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d11.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]))

(def grid-serial-number input)

(def x-max 300)
(def y-max 300)
(def range-x (range 1 (inc x-max)))
(def range-y (range 1 (inc y-max)))

(def my-pmap #?(:clj pmap :cljs map))

(defn rack-id [[x _]]
  (+ x 10))

(defn hundreds-digit [x]
  (rem (quot x 100) 10))

(defn power-level [[_ y :as p]]
  (-> p
      rack-id
      (* y)
      (+ grid-serial-number)
      (* (rack-id p))
      hundreds-digit
      (- 5)))

(defn summed-area-table [colls]
  (->> colls
       (my-pmap #(reductions + %))
       (reductions #(map + %1 %2))
       (map #(into [] %))
       (into [])))

(defn fuel-cell-capacity [[x y] sat size]
  (let [x' (dec (dec x))
        y' (dec (dec y))]
    (+ (+ (or (get-in sat [y' x']) 0))
       (+ (or (get-in sat [(+ y' size) (+ x' size)]) 0))
       (- (or (get-in sat [(+ y' size) x']) 0))
       (- (or (get-in sat [y' (+ x' size)]) 0)))))

(defn biggest-fuel-cell [sat size]
  (->> (for [x (range 1 (inc (- x-max (dec size))))
             y (range 1 (inc (- y-max (dec size))))]
         [[x y] (fuel-cell-capacity [x y] sat size)])
       (apply max-key second)))

(defn solve-1 []
  (let [sat (->> (for [y range-y
                       x range-x]
                   (power-level [x y]))
                 (partition x-max)
                 summed-area-table)]
    (->> (biggest-fuel-cell sat 3)
         (#(let [[[x y] _] %]
             (str x "," y))))))

(defn solve-2 []
  (let [sat (->> (for [y range-y
                       x range-x]
                   (power-level [x y]))
                 (partition x-max)
                 summed-area-table)]
    (->> (my-pmap (fn [size] [size (biggest-fuel-cell sat size)]) (range 1 301))
         (apply max-key (comp second second))
         (#(let [[size [[x y] _]] %]
             (str x "," y "," size))))))

(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest ^:slow part-2
         (is (= (str answer-2)
                (str (solve-2)))))

;;;; Scratch

(comment
  (let [sat (summed-area-table [[1 2 3 4]
                                [5 6 7 8]
                                [9 10 11 12]])
        size 2]
    (->> (for [x (range 1 (inc (- 4 (dec size))))
               y (range 1 (inc (- 3 (dec size))))]
           [[x y] (fuel-cell-capacity [x y] sat size)])))

  (let [sat (summed-area-table [[1 2 3 4]
                                [5 6 7 8]
                                [9 10 11 12]])]
    (biggest-fuel-cell sat 3))

  (map println (summed-area-table [[1 2 3 4]
                                   [5 6 7 8]
                                   [9 10 11 12]]))

  (let [sat (summed-area-table [[1 2 3 4]
                                [5 6 7 8]
                                [9 10 11 12]])]
    (for [size (range 1 5)]
      [size (biggest-fuel-cell sat size)]))


  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true)
  (t/run-tests))

