(ns aoc.y2018.d02.borkdude
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(defn correct-ids* []
  (reduce (fn [[twos threes] s]
            (let [m (set/map-invert (frequencies s))]
              [(if-let [new-two (when (get m 2) s)]
                 (conj twos new-two)
                 twos)
               (if-let [new-three (when (get m 3) s)]
                 (conj threes new-three)
                 threes)]))
          [[] []]
          (str/split-lines input)))

(def correct-ids (memoize correct-ids*))

(defn solve-1 []
  (let [[twos threes]
        (correct-ids)]
    (* (count twos) (count threes))))

(defn one-different? [s1 s2]
  (cond (empty? s1)
        false
        (not= (first s1) (first s2))
        (= (rest s1) (rest s2))
        :else
        (recur (rest s1) (rest s2))))

(defn common-letters
  ([s1 s2]
   (common-letters [] s1 s2))
  ([common [f1 & r1 :as s1] [f2 & r2]]
   (cond (empty? s1)
         common
         (not= f1 f2)
         (common-letters common r1 r2)
         :else
         (recur (conj common f1)
                r1 r2))))

(defn solve-2 []
  (let [all-ids (into (set (first (correct-ids)))
                      (second (correct-ids)))
        matching-ids (first (for [id1 all-ids
                                  id2 all-ids
                                  :when (one-different? id1 id2)]
                              [id1 id2]))]
    (apply str
           (apply common-letters matching-ids))))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
