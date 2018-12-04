(ns aoc.y2018.d02.dfornika
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(def data
  (str/split-lines input))

(defn has-n-letters [n s]
  (-> 
   (frequencies s)
   (vals)
   (set)
   (contains? n)))

(defn inc-counts [s counts]
  (cond-> counts
    (has-n-letters 2 s) (update-in [:has-letter-twice] inc)
    (has-n-letters 3 s) (update-in [:has-letter-thrice] inc)))

(defn solve-1 []
  (loop [ids data
         counts {:has-letter-twice 0
                 :has-letter-thrice 0}]
    (if (not (empty? ids))
      (recur (rest ids)
             (inc-counts (first ids) counts))
      (* (counts :has-letter-twice) (counts :has-letter-thrice)))))

(defn one-diff [s1 s2]
  (and (= (count s1) (count s2))
       (->>
        (map = s1 s2)
        (filter not)
        (count)
        (= 1))))

(defn remove-diff [s1 s2]
  (->>
   (map = s1 s2)
   (map #(if %2 %) s1)
   (apply str)))

(defn solve-2 []
  (loop [ids (sort data)]
    (if (one-diff (first ids) (second ids))
      (remove-diff (first ids) (second ids))
      (recur (drop 2 ids)))))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
