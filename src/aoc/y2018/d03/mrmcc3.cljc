(ns aoc.y2018.d03.mrmcc3
  (:require
    [aoc.utils :as u :refer [deftest]]
    [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]
    [clojure.set :as set]))

;; parse each claim into a set of coordinates
;; a coordinate is just a string "top,left"

(def regex #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

(defn parse-claim [s]
  (let [[id l t w h]
        (->> (re-find regex s)
             (drop 1)
             (map u/read-string))
        xs     (range l (+ l w))
        ys     (range t (+ t h))
        coords (set (for [x xs y ys] (str x \, y)))]
    (with-meta coords {:id id})))

(def claims
  (delay (map parse-claim (str/split-lines input)))) ;; delay computation

;; find the set of overlapping coordinates for all claims

(def overlapping
  (->> (apply concat @claims)
       (frequencies)
       (filter (comp #(< 1 %) val))
       keys set delay)) ;; delay computation

(defn solve-1 []
  (count @overlapping))

;; any claims set of coordinates that doesn't intersect with
;; the overlapping set is the one we're looking for

(defn solve-2 []
  (->> (map (partial set/intersection @overlapping) @claims)
       (remove seq)
       first meta :id))

;; test stuff

(deftest part-1 (is (= answer-1 (solve-1))))
(deftest part-2 (is (= answer-2 (str (solve-2)))))
