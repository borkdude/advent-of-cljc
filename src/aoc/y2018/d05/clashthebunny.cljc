(ns aoc.y2018.d05.clashthebunny
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as s]
   [clojure.core.reducers :as r]))

(defn parse [string]
  (-> string
      s/trim
      seq))

; snagged from mfikes
(defn reacts? [x y]
  (and (not= x y)
       (= (s/lower-case x) (s/lower-case y))))

(defn check-letter
  ([] [])
  ([elim]
   [])
  ([result current-letter]
   (cond
     (= 0 (count result)) [current-letter]
     (reacts? (peek result) current-letter) (pop result)
     :else (conj result current-letter)))
  ([elim result current-letter]
   (cond
     (= elim (s/lower-case current-letter)) result
     :else (check-letter result current-letter))))

(defn react
  ([checked polymer]
   (r/fold 15000 check-letter check-letter polymer))
  ([checked polymer elim]
   (r/fold 15000 (partial check-letter elim) (partial check-letter elim) polymer)))

(defn find-elements [polymer]
 (into #{} (map s/lower-case (into #{} polymer))))

(defn try-elim-elem [polymer]
 (into {} (for [elem (find-elements polymer)]
             {elem (count (react [] polymer elem))})))

(defn solve-1 []
 (count (react [] (parse input))))

(defn solve-2 []
 (val (apply min-key val (try-elim-elem (parse input)))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
