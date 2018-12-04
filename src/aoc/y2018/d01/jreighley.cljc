(ns aoc.y2018.d01.jreighley
  (:require
   [clojure.string :as str]
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

(def inputlist (map u/read-string
                 (str/split-lines input)))

(defn repeating-freq "find the first freqency that happens twice" [freq-history freq inputlist]
  (let [adjustment (first inputlist)
        new-freq (+ freq adjustment)
        remaining-adj (rest inputlist)]
    (if (contains? freq-history new-freq)
        new-freq
        (recur (conj freq-history new-freq) new-freq remaining-adj))))

(defn solve-1 "find the final freqency" []
  (reduce + inputlist))

(defn solve-2 []
  (repeating-freq #{} 0 (cycle inputlist)))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
