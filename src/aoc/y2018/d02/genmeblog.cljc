(ns aoc.y2018.d02.genmeblog
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [clojure.string :refer [split-lines]]
   [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

;;

(def ids (delay (split-lines input)))

;; part 1

;; frequencies as list of sets
(defn freqs [] (map #(->> %
                          (frequencies)
                          (group-by second)
                          (keys)
                          (set)) @ids))

(defn how-many
  "How many sets with given value"
  [val]
  (count (filter #(% val) (freqs))))

;; part 2

(defn str-diff
  "Difference between two strings. Returns 0 when strings differ by exactly one character."
  [a b]
  (dec (reduce + (map (fn [a b] (if (= a b) 0 1)) a b))))

(defn correct-boxes []
  (for [a @ids
        b @ids
        :when (zero? (str-diff a b))]
    [a b]))

(defn common-letters []
  (->> (correct-boxes)
       (map (fn [[a b]] (map #(if (= %1 %2) %1 nil) a b)))
       (map (partial apply str))
       (distinct)
       (first)))

(def solve-1 #(* (how-many 2) (how-many 3)))
(def solve-2 #(common-letters))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
