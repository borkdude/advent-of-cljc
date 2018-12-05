(ns aoc.y2017.d05.mfikes
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d05.data :refer [input answer-1 answer-2]]
   [clojure.string :as string]
   [clojure.test :refer [is testing]]))

(def input-lines (string/split-lines input))

(def data (map read-string input-lines))

(defn solve [maze update-fn]
  (reduce (fn [[maze ndx] counter]
            (if-let [offset (get maze ndx)]
              [(update maze ndx update-fn) (+ ndx offset)]
              (reduced counter)))
    [(vec maze) 0]
    (range)))

(defn solve-1 []
  (solve data inc))

(defn solve-2 []
  (solve data (fn [v]
                (if (<= 3 v)
                  (dec v)
                  (inc v)))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
