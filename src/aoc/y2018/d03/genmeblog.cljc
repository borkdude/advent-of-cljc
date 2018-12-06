(ns aoc.y2018.d03.genmeblog
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
   [clojure.set :refer [union]]
   [clojure.string :refer [split-lines]]
   [clojure.test :refer [is testing]]))

;;

(defn claim-parser
  "Parse claims, return rect coordinates."
  [line]
  (let [[id x y w h] (map read-string (rest (re-find #"#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)" line)))]
    [id x y (+ x w) (+ y h)]))

(def claims
  (delay (map claim-parser (split-lines input))))

;;

(defn overlap
  "Find overlaps"
  [[id1 r1x1 r1y1 r1x2 r1y2] [id2 r2x1 r2y1 r2x2 r2y2]]
  (when (not= id1 id2)
    (let [left (max r1x1 r2x1)
          right (min r1x2 r2x2)
          top (max r1y1 r2y1)
          bottom (min r1y2 r2y2)]
      (when (and (< left right) (< top bottom))
        [left top right bottom]))))

(def overlaps-map
  (delay (reduce #(->> (map (partial overlap %2) @claims)
                       (filter some?)
                       (seq)
                       (assoc %1 (first %2))) {} @claims)))

(defn overlap->set
  "Convert common rectangle to set of coordinates"
  [[x1 y1 x2 y2]]
  (set (for [x (range x1 x2)
             y (range y1 y2)]
         [x y])))

(defn inches-overlap []
  (count (->> (vals @overlaps-map)
              (mapcat identity)
              (map overlap->set)
              (reduce union #{}))))

;;

(def solve-1 #(inches-overlap))
(def solve-2 #(ffirst (filter (comp nil? second) @overlaps-map)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
