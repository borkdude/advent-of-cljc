(ns aoc.y2018.d10.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format nth' count']]
    [aoc.y2018.d10.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]
    [com.rpl.specter :refer [path collect-one terminal multi-transform ALL multi-path
                             setval nthpath]]))

(defn strs->point [vals]
  (zipmap [:x :y :vx :vy]
          (map u/parse-int vals)))

(defn parse-input []
  (map strs->point
       (map #(re-seq #"-?\d+" %) (str/split-lines input))))

(defn move-coord [steps k vk]
  (path (collect-one vk) k (terminal #(+ (* steps %1) %2))))

(defn move-points [steps points]
  (multi-transform [ALL (multi-path (move-coord steps :x :vx)
                                    (move-coord steps :y :vy))]
                   points))

(defn dimensions [points]
  {:x-max (apply max (map :x points))
   :x-min (apply min (map :x points))
   :y-max (apply max (map :y points))
   :y-min (apply min (map :y points))})

(defn box-size [points]
  (let [{:keys [x-max y-max x-min y-min]} (dimensions points)]
    (+ (- x-max x-min)
       (- y-max y-min))))

(defn shrinking? [points]
  (> (box-size points) (box-size (move-points 1 points))))

(defn minimum? [points]
  (let [left (box-size (move-points -1 points))
        middle (box-size points)
        right (box-size (move-points 1 points))]
    (and (> left middle)
         (< middle right))))

(defn estimation [points]
  (let [a (box-size points)
        b (box-size (move-points 1 points))]
    (- (quot a (- b a)))))

(defn find-smallest-constellation []
  (let [points (parse-input)
        est (estimation points)
        step (if (shrinking? (move-points est points))
               1 -1)]
    (loop [index est]
      (if (minimum? (move-points index points))
        [(dec index) (move-points index points)]
        (recur (+ step index))))))

(defn draw-point [min-x min-y field {:keys [x y]}]
  (setval [(nthpath (- y min-y)) (nthpath (- x min-x))] "#"
          field))

(defn field-string [field]
  (str/join \newline (map str/join field)))

(defn draw-points [points]
  (let [{:keys [x-max y-max x-min y-min]} (dimensions points)
        field (repeat (inc (- y-max y-min))
                      (repeat (inc (- x-max x-min)) " "))]
    (field-string
      (reduce (partial draw-point x-min y-min)
              field
              points))))

(def solve
  (memoize find-smallest-constellation))

(defn solve-1 []
  (draw-points (second (solve))))

(defn solve-2 []
  (inc (first (solve))))

(deftest part-1
         (is (= 1122240692 (hash (solve-1)))))

(deftest part-2
         (is (= (str answer-2)
                (str (solve-2)))))
