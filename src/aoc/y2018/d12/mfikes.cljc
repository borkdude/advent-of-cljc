(ns aoc.y2018.d12.mfikes
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d12.data :refer [input answer-1 answer-2]]
   [clojure.string :as string]
   [clojure.test :as t :refer [is testing]]))

(defn parse-note [note]
  (when (= "#" (subs note 9))
    (map #{\#} (subs note 0 5))))

(defn parse-state [state]
  (into (sorted-set) (keep-indexed (fn [idx pot]
                                     (when (= \# pot) idx))
                       state)))

(defn parse-input [input]
  (let [[initial-state-line _ & notes] (string/split-lines input)]
    [(parse-state (subs initial-state-line 15))
     (into #{} (keep parse-note notes))]))

(defn generation [notes state]
  (reduce (fn [state' idxs]
            (let [grow? (notes (map #(when (state %) \#) idxs))]
              (cond-> state' grow? (conj (nth idxs 2)))))
    (sorted-set)
    (partition 5 1
      (range (- (first state) 5) (inc (+ (first (rseq state)) 5))))))

(defn states [input]
  (let [[state notes] (parse-input input)]
    (iterate (partial generation notes) state)))

(defn solve [states n]
  (reduce + (nth states n)))

(defn solve-extrapolating [states n]
  (let [shifted? #(apply = (apply map - %))
        idx      (first (keep-indexed #(when (shifted? %2) %1)
                          (map vector states (rest states))))
        base     (solve states idx)
        next     (solve states (inc idx))]
    (+ base (* (- next base) (- n idx)))))

(defn solve-1 []
  (solve (states input) 20))

(defn solve-2 []
  (solve-extrapolating (states input) 50000000000))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
;; borkdude: added comment for retriggering scores
