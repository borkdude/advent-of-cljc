(ns aoc.y2017.d03.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d03.data :refer [input answer-1 answer-2]]
   [clojure.test :as t :refer [is testing]]))

(defn moves [n]
  (concat (repeat n (if (odd? n) :right :left))
          (repeat n (if (odd? n) :up :down))))

(defn path [n]
  (->> (range)
       (drop 1)
       (map moves)
       (apply concat)
       (take (dec n))))

(def shifts {:left [-1 0] :right [1  0]
             :up   [ 0 1] :down  [0 -1]})

(defn distance [n]
  (let [steps (map shifts (path n))]
    (+
     (Math/abs (reduce + (map first steps)))
     (Math/abs (reduce + (map second steps))))))

(defn solve-1 []
  (distance input))

(def all-moves ;; lazy-seq of all the moves from the center of the grid
  (->> (range)
       (drop 1)
       (map moves)
       (apply concat)))

(defn coords-plus [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn compute-grid [grid [x y]]
  (reduce +
          (for [i [-1 0 1]
                j [-1 0 1]
                :when (and (not= 0 i j)
                           (some? (grid [(+ x i) (+ y j)])))]
            (grid [(+ x i) (+ y j)]))))

(defn fill-grid [grid [x y] moves-seq limit]
  (let [curr (compute-grid grid [x y])]
    (if (>= curr limit)
      curr
      (let [new-grid  (assoc grid [x y] curr)
            curr-move (first moves-seq)
            new-coord (coords-plus [x y] (shifts curr-move))]
        (fill-grid new-grid new-coord (rest moves-seq) limit)))))

(defn solve-2 []
  (fill-grid {[0 0] 1} [1 0] (drop 1 all-moves) input))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
)
