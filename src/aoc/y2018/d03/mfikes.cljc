(ns aoc.y2018.d03.mfikes
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
   [clojure.string :as string]
   [clojure.test :refer [is testing]]))

(def input-lines (string/split-lines input))

(defn read-claim [s]
  (->> (rest (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s))
    (map read-string)
    (zipmap [:id :left :top :width :height])))

(def claims (map read-claim input-lines))

(defn locs [claim]
  (for [row (range (:top claim)  (+ (:top claim)  (:height claim)))
        col (range (:left claim) (+ (:left claim) (:width claim)))]
    [row col]))

(defn add-claim [fabric claim]
  (reduce
    (fn [fabric loc]
      (update fabric loc conj (:id claim)))
    fabric
    (locs claim)))

(defn loc->claims-ids [claims]
  (reduce add-claim {} claims))

(defn overlapping? [loc-claims]
  (> (count loc-claims) 1))

(defn overlapping-loc-claims [claims]
  (filter overlapping? (vals (loc->claims-ids claims))))

(defn solve-1 []
  (count (overlapping-loc-claims claims)))

(defn solve-2 []
  (let [overlapping-claim-ids (reduce into #{} (overlapping-loc-claims claims))
        all-claim-ids         (map :id claims)]
    (first (remove overlapping-claim-ids all-claim-ids))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
