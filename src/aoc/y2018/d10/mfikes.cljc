(ns aoc.y2018.d10.mfikes
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format nth' count']]
   [aoc.y2018.d10.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as string]))

(defn read-point [s]
  (zipmap [:p-x :p-y :v-x :v-y] (map read-string (re-seq #"-?\d+" s))))

(defn update-point [point]
  (-> point
    (update :p-x + (:v-x point))
    (update :p-y + (:v-y point))))

(defn raster-geometry [points]
  (let [min-max (juxt #(apply min %) #(apply max %))
        [min-x max-x] (min-max (map :p-x points))
        [min-y max-y] (min-max (map :p-y points))]
    [min-x min-y (- max-x min-x) (- max-y min-y)]))

(defn big? [points]
  (let [[_ _ width height] (raster-geometry points)]
    (or (> width 80) (> height 10))))

(defn rasterize [points]
  (let [[origin-x origin-y width height] (raster-geometry points)]
    (reduce (fn [raster {:keys [p-x p-y]}]
              (assoc-in raster [(- p-y origin-y) (- p-x origin-x)] "#"))
      (vec (repeat (inc height) (vec (repeat (inc width) "."))))
      points)))

(defn print-raster [raster]
  (println (string/join \newline (map string/join raster))))

(defn solve-1 []
  (let [initial-points (map read-point (string/split-lines input))
        points (nth' (eduction (drop-while big?) (iterate #(map update-point %) initial-points)) 0)]
    (print-raster (rasterize points))))

(defn solve-2 []
  (let [initial-points (map read-point (string/split-lines input))]
    (count' (eduction (take-while big?) (iterate #(map update-point %) initial-points)))))

(deftest part-1
  (is (= 204673534 (hash (with-out-str (solve-1))))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
