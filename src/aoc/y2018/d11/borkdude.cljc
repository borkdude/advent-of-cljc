(ns aoc.y2018.d11.borkdude
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d11.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :as t :refer [is testing]]))

(defn power-level* [^long serial ^long x ^long y]
  (let [rack-id (+ x 10)
        pl (* rack-id y)
        pl (+ pl serial)
        pl (* pl rack-id)
        pl (int (mod (/ pl 100) 10))
        pl (- pl 5)]
    pl))

(def power-level (memoize power-level*))

(defn first-divisor* [^long n]
  (first
   (for [i (range 2 (long (/ n 2)))
         :when (zero? ^long (rem n ^long i))]
     i)))

(def first-divisor (memoize first-divisor*))

(declare power-level-nxn)

(defn power-level-nxn* [serial ^long x ^long y ^long n]
  (if (= 1 n)
    [[x y n] (power-level serial x y)]
    (let [fd (or (first-divisor n) 1)
          dr (range 0 n fd)
          xs (map #(+ ^long % x) dr)
          ys (map #(+ ^long % y) dr)]
      [[x y n] (reduce + (for [x xs
                               y ys]
                           (second (power-level-nxn serial x y fd))))])))

(def power-level-nxn (memoize power-level-nxn*))

(defn max-power-level-nxn* [n]
  (apply max-key second
         (for [x (range 1 300)
               y (range 1 300)]
           (power-level-nxn input x y n))))

(def max-power-level-nxn (memoize max-power-level-nxn*))

(defn solve-1 []
  (str/join ","
            (take 2 (first (max-power-level-nxn 3)))))

(defn max-in-range [r]
  (loop [ns r
         [v ^long max] (power-level-nxn input 1 1 1)]
    (if (empty? ns)
      [v max]
      (let [[new-v ^long new-max]
            (max-power-level-nxn (first ns))]
        (recur (rest ns)
               (if (> new-max max)
                 [new-v new-max]
                 [v max]))))))

(defn find-optimum []
  (loop [ranges (partition-all 3 (range 1 301))
         [v ^long max] (power-level-nxn input 1 1 1)]
    (if (empty? ranges) [v max]
        (let [[new-v ^long new-max]
              (max-in-range (first ranges))]
          (if (> new-max max)
            (recur (rest ranges) [new-v new-max])
            [v max])))))

(defn solve-2 []
  (str/join ","
            (first (find-optimum))))

(deftest ^:slow part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest ^:skip-cljs ^:slow part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true)
  (t/run-tests)
  )
