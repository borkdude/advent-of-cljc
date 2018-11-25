(ns aos.y2017.d03
  (:require
   [aos.y2017.input :refer [input-d03] :rename {input-d03 input}]
   [clojure.test :as t :refer [deftest is testing]]))

;;;;

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

(defn solution-9ffdac28-p1 []
  (let [tile (tile-at input)]
    (+ (Math/abs (:x tile))
       (Math/abs (:y tile)))))

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

(defn solution-9ffdac28-p2 []
  (:v (tile-with-bigger-sum input)))

;;;;

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

(defn nth' [coll n]
  (transduce (drop n) (completing #(reduced %2)) nil coll))

(defn location [square]
  (nth' spiral (- square 2)))

(defn distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn solution-c06e27a1-p1 []
  (distance (location input)))

(defn adjacent-locations [[x y]]
  (map (fn [[dx dy]]
         [(+ x dx) (+ y dy)])
       [[-1  1] [0  1] [1  1]
        [-1  0]        [1  0]
        [-1 -1] [0 -1] [1 -1]]))

(defn solution-c06e27a1-p2 []
  (reduce (fn [acc location]
            (let [value (apply + (map #(acc % 0) (adjacent-locations location)))]
              (if (< input value)
                (reduced value)
                (assoc acc location value))))
          {[0 0] 1}
          spiral))

;;;; Tests

(deftest p1-test
  (is (= 419 (solution-9ffdac28-p1)))
  (is (= 419 (solution-c06e27a1-p1)))
  )

(deftest p2-test
  (is (= 295229 (solution-9ffdac28-p2)))
  (is (= 295229 (solution-c06e27a1-p2)))
  )

(deftest ^:instrumented sanity-check
  (is (thrown? clojure.lang.ExceptionInfo (merge 1))))

;;;; Scratch

(comment
  (require '[speculative.instrument :refer [instrument]])
  (require '[patch.clj-2443])
  (instrument)
  (t/run-tests))
