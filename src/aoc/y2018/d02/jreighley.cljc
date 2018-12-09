(ns aoc.y2018.d02.jreighley
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :refer [split-lines trim]]))

(def two? #(= 2 %))
(def three? #(= 3 %))
(def double-count #(->> %
                     (vals)
                     (filter two?)
                     (count)
                     (pos?)))
(def triple-count #(->> %
                     (vals)
                     (filter three?)
                     (count)
                     (pos?)))

(def input-items
  (delay
    (->> input
         (split-lines)
         (map trim)
         (sort))))

(def input-freqs
  (->> @input-items
       (map frequencies)))

(defn matching-letters [ str-1 str-2]
    (let [match-seq (for [n   (range (count str-1))]
                      (if (= (nth str-1 n) (nth str-2 n))
                        (nth str-1 n)))
          matching-str (apply str match-seq)
          one-mismatch? (= (dec (count str-1)) (count matching-str))]
      (when one-mismatch? matching-str)))

(defn search-seq [str-seq]
  (when (< 1 (count str-seq))
    (let [match-str (matching-letters (nth str-seq 0)
                                      (nth str-seq 1))]
     (if match-str
       match-str
       (recur (rest str-seq))))))

(defn solve-1 []
  (* (count (filter triple-count input-freqs))
     (count (filter double-count input-freqs))))

(defn solve-2 []
  (search-seq @input-items))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))


