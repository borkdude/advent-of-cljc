(ns aoc.y2018.d03.adamruzicka
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(def data (str/split-lines input))

(defn claim-to-points [claim]
  (for [x (range (claim :width))
        y (range (claim :height))]
    [(+ x (claim :x)) (+ y (claim :y))]))

(defn claim-to-points-with-info [claim]
  (assoc claim :points (claim-to-points claim)))

(defn parse-line [line]
  (let [[_ & captures] (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$" line)]
    (zipmap [:id :x :y :width :height] (map read-string captures))))

(defn unique-claim? [freqs claim]
  (->> (claim :points)
       (map #(= 1 (freqs %)))
       (every? true?)))

(defn find-unique-claim [claims]
  (let [freqs (frequencies (mapcat #(% :points) claims))]
    (->> claims
         (filter #(unique-claim? freqs %))
         (first))))

(defn solve-1 []
  (->> data
       (map parse-line)
       (mapcat claim-to-points)
       (frequencies)
       (vals)
       (filter #(> % 1))
       (count)))

(defn solve-2 []
  (->> data
       (map parse-line)
       (map claim-to-points-with-info)
       (find-unique-claim)
       (#(% :id))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
