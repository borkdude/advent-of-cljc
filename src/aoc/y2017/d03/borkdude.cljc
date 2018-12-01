(ns aoc.y2017.d03.borkdude
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2017.d03.data :refer [input answer-1 answer-2]]
   [clojure.test :as t :refer [is testing]]))

(def directions
  "Infite seq of directions through
  spiral: :right :up :left :left :down, etc."
  (let [dirs (cycle [[:right :up] [:left :down]])
        amount (map inc (range))]
    (mapcat (fn [[d1 d2] amount]
              (concat (repeat amount d1)
                      (repeat amount d2)))
            dirs
            amount)))

(defn next-tile
  "Calculates (n+1)th tile from nth tile"
  [tile direction]
  (let [[axis delta]
        (case direction
          :right [:x 1]
          :left  [:x -1]
          :down  [:y 1]
          :up    [:y -1])]
    (update tile axis + delta)))

(defn tile-at
  "Returns nth tile in spiral"
  [n]
  (reduce next-tile
          {:x 0 :y 0}
          (take (dec n)
                directions)))

(deftest part-1
  (is (= answer-1
         (let [tile (tile-at input)]
           (+ (Math/abs (:x tile))
              (Math/abs (:y tile)))))))

(defn sum-of-neighbours
  "Sum of neighbouring tiles"
  [{:keys [x y]}
   tiles]
  (let [neighbour-positions
        (for [dx [-1 0 1]
              dy [-1 0 1]
              :when (not= 0 dx dy)]
          [(+ x dx)
           (+ y dy)])
        neighbours (keep #(get tiles %)
                         neighbour-positions)]
    (reduce + (map :v neighbours))))

(defn tile-with-bigger-sum
  "Returns first tile with sum > n"
  [n]
  (let [init-tile {:x 0 :y 0 :v 1}]
    (loop [tile init-tile
           tiles {[0 0] init-tile}
           directions directions]
      (let [next-direction (first directions)
            new-tile (next-tile tile next-direction)
            sum (sum-of-neighbours
                 new-tile
                 tiles)
            new-tile (assoc new-tile :v sum)]
        (if (> sum n)
          new-tile
          (recur new-tile
                 (assoc tiles [(:x new-tile)
                               (:y new-tile)]
                        new-tile)
                 (rest directions)))))))

(deftest part-2
  (is (= answer-2 (:v (tile-with-bigger-sum input)))))

#_(deftest ^:instrumented sanity-check
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                  :cljs ExceptionInfo)
               (merge 1))))
