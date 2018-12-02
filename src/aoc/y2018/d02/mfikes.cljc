(ns aoc.y2018.d02.mfikes
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d02.data :refer [input answer-1 answer-2] :rename {input input-str}]
   [clojure.string :as string]
   [clojure.test :refer [is testing]]
   [aoc.y2018.d01.mfikes :as day-01]))

(def input (string/split-lines input-str))

(defn count-with-exact [box-ids n]
  (->> box-ids
    (map frequencies)
    (map vals)
    (keep #(some #{n} %))
    count))

(defn solve-1 []
  (* (count-with-exact input 2) (count-with-exact input 3)))

(defn delete [idx s]
  (str (subs s 0 idx) (subs s (inc idx))))

(defn solve-2 []
  (some (fn [idx]
          (->> input
            (map (partial delete idx))
            day-01/first-duplicate))
    (range)))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
