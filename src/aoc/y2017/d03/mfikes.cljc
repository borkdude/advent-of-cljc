(ns aoc.y2017.d03.mfikes
  (:require
   [aoc.utils :as u :refer [deftest nth']]
   [aoc.y2017.d03.data :refer [input answer-1 answer-2]]
   [clojure.test :as t :refer [is testing]]))

(defn candidate-locations [[x y]]
  (map (fn [[dx dy]]
         [(+ x dx) (+ y dy)])
       [[0 -1] [-1 0] [0 1] [1 0]]))

(defn step [[location used-locations]]
  (let [next-location (->> location
                           candidate-locations
                           (map (fn [candidate-location]
                                  [candidate-location (contains? used-locations candidate-location)]))
                           cycle
                           (drop-while (complement second))
                           (drop-while second)
                           ffirst)]
    [next-location (conj used-locations next-location)]))

(def spiral (eduction (map first) (iterate step [[1 0] #{[0 0] [1 0]}])))

(defn location [square]
  (nth' spiral (- square 2)))

(defn distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(deftest part-1
  (is (= answer-1 (distance (location input)))))

(defn adjacent-locations [[x y]]
  (map (fn [[dx dy]]
         [(+ x dx) (+ y dy)])
       [[-1  1] [0  1] [1  1]
        [-1  0]        [1  0]
        [-1 -1] [0 -1] [1 -1]]))

(deftest part-2
  (is (= answer-2
         (reduce (fn [acc location]
                   (let [value (apply + (map #(acc % 0) (adjacent-locations location)))]
                     (if (< input value)
                       (reduced value)
                       (assoc acc location value))))
                 {[0 0] 1}
                 spiral))))
