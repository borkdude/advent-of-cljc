(ns aoc.y2017.d05.dandorman
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2017.d05.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(defn steps [maze]
  (loop [maze  maze
         pos   0
         moves 0]
    (if (< -1 pos (count maze))
      (recur (update maze pos inc)
             (+ pos (get maze pos))
             (inc moves))
      moves)))

(defn weird-steps [maze]
  (loop [maze  maze
         pos   0
         moves 0]
    (if (< -1 pos (count maze))
      (recur (update maze pos #(if (<= 3 %) (dec %) (inc %)))
             (+ pos (get maze pos))
             (inc moves))
      moves)))

(def maze (->> (str/split input #"\n")
               (map u/parse-int)
               (into [])))


(deftest part-1
  (is (= answer-1 (steps maze))))

(deftest part-2
  (is (= answer-2 (weird-steps maze))))
