(ns aoc.y2018.d03.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
    [clojure.set :as set]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(defn parse-numbers [s]
  (map #(u/parse-int %) (re-seq #"\d+" s)))

(defn rectangle [xs]
  (let [[id left top width height] xs]
    {::id     id
     ::left   left
     ::top    top
     ::right  (dec (+ left width))
     ::bottom (dec (+ top height))}))

(defn rectangles [input]
  (map (comp rectangle parse-numbers) input))

(defn points-in-rectangle [{:keys [::left ::top ::right ::bottom]}]
  (for [x (range left (inc right))
        y (range top (inc bottom))]
    [x y]))

(defn has-x-overlap? [rect1 rect2]
  (not (or (> (::left rect1) (::right rect2))
           (< (::right rect1) (::left rect2)))))

(defn has-y-overlap? [rect1 rect2]
  (not (or (> (::top rect1) (::bottom rect2))
           (< (::bottom rect1) (::top rect2)))))

(defn intersect? [rect1 rect2]
  (and (has-x-overlap? rect1 rect2)
       (has-y-overlap? rect1 rect2)))

(defn has-overlap-in? [rect rects]
  (true? (some first
               (for [rhs rects
                     :when (not (= rhs rect))]
                 [(intersect? rect rhs) (::id rect)]))))

(defn overlapping [rects]
  (filter
    #(has-overlap-in? % rects)
    rects))

(defn non-overlapping [rects]
  (remove (into #{} (overlapping rects)) rects))

(defn solve-1 []
  ;; Part 1 - Input of the form "#1215 @ 743,809: 28x27", indicating rectangle
  ;; with:
  ;; ID #1215
  ;; (left, top) (743, 809)
  ;; (width, height) (28, 27)
  ;;
  ;; Note: our coordinate system starts with (0,0) at the top left. Width extends
  ;; towards the right. Height extends towards the bottom.
  ;;
  ;; Find the total area of all the regions in which at least two rectangles
  ;; overlap.

  ;; Create a map of points -> frequency they appear in the rectangles.
  ;; Count up the points that appear more than once.
  (->> (rectangles (str/split-lines input))
       (map points-in-rectangle)
       (map frequencies)
       (apply merge-with +)
       vals
       (filter #(> % 1))
       count))

(defn solve-2 []
  ;; Part 2 - Find the one rectangle that doesn't overlap with any other.
  (::id (first (non-overlapping (rectangles (str/split-lines input))))))


(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
