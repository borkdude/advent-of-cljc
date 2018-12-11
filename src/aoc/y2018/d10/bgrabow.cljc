(ns aoc.y2018.d10.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d10.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(defn parse [input]
  (->> input
       str/split-lines
       (map #(->> %
                  (re-seq #"-?\d+")
                  (map u/parse-int)))
       (map (fn [[x y dx dy]]
              {:x x
               :y y
               :dx dx
               :dy dy}))))

(defn step-n [n [x y] [dx dy]]
  [(+ x (* dx n))
   (+ y (* dy n))])

(defn step [[x y] [dx dy]]
  [(+ x dx)
   (+ y dy)])

(defn time-until-nearby [pa pb]
  (let [h (- (:y pa) (:y pb))
        ddy (- (:dy pb) (:dy pa))]
    (quot h ddy)))

(defn min-max-by [pred coll]
  (when coll
    (let [value-predvalue-pairs (map (fn [x] [x (pred x)]) coll)]
      (map first (reduce (fn [[min-pair max-pair] new-pair]
                           [(min-key second min-pair new-pair)
                            (max-key second max-pair new-pair)])
                         [(first value-predvalue-pairs) (first value-predvalue-pairs)]
                         (rest value-predvalue-pairs))))))

(defn y-height [stars]
  (let [[y-min y-max] (min-max-by identity (map second stars))]
    (- y-max y-min)))

(defn stars-to-string [stars]
  (let [[x-min x-max] (map first (min-max-by first stars))
        [y-min y-max] (map second (min-max-by second stars))
        stars-set (into #{} stars)]
    (->> (for [y (range y-min (inc y-max))]
           (apply str (map (fn [x]
                             (if (stars-set [x y]) \* \space))
                           (range x-min (inc x-max)))))
         (str/join \newline))))

(defn left-most-x-neighbor [all-points point]
  (let [x-vals (into #{} (map first all-points))]
    (loop [x (first point)]
      (if (x-vals (dec x))
        (recur (dec x))
        x))))

(defn group-stars [stars]
  (group-by
    (partial left-most-x-neighbor stars)
    stars))

(defn solve-1 []
  (let [parsed-input (parse input)
        [fastest-down fastest-up] (min-max-by :dy parsed-input)
        search-start-time (- (time-until-nearby fastest-down fastest-up)
                             10)
        velocities (map #(map % [:dx :dy]) parsed-input)]
    (->> (loop [stars (->> parsed-input
                           (map #(map % [:x :y]))
                           (#(map (fn [star v] (step-n search-start-time star v)) % velocities)))
                height (y-height stars)
                time search-start-time]
           (let [new-stars (map step stars velocities)
                 new-height (y-height new-stars)]
             (if (> new-height height)
               stars
               (recur new-stars new-height (inc time)))))
         stars-to-string)))

(defn solve-2 []
  (let [parsed-input (parse input)
        [fastest-down fastest-up] (min-max-by :dy parsed-input)
        search-start-time (- (time-until-nearby fastest-down fastest-up)
                             10)
        velocities (map #(map % [:dx :dy]) parsed-input)]
    (->> (loop [stars (->> parsed-input
                           (map #(map % [:x :y]))
                           (#(map (fn [star v] (step-n search-start-time star v)) % velocities)))
                height (y-height stars)
                time search-start-time]
           (let [new-stars (map step stars velocities)
                 new-height (y-height new-stars)]
             (if (> new-height height)
               time
               (recur new-stars new-height (inc time))))))))

(deftest part-1
  (is (= 2072437284
         (hash (str (solve-1))))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))