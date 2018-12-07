(ns aoc.y2018.d07.mfikes
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d07.data :refer [input answer-1 answer-2]]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :refer [is testing]]))

(defn parse-line [s]
  [(first (subs s 36 37)) (first (subs s 5 6))])

(defn input->dep-map [input]
  (->> (string/split-lines input)
    (map parse-line)
    (reduce (fn [m [k v]]
              (update m k (fnil conj #{}) v))
      {})))

(defn eliminate [dep-map completed]
  (reduce-kv (fn [m k v]
               (if-some [v' (not-empty (reduce disj v completed))]
                 (assoc m k v')
                 m))
    {}
    dep-map))

(defn decrement [remaining in-process]
  (reduce-kv (fn [m k v]
               (if (some #{k} in-process)
                 (if (== 1 v)
                   m
                   (assoc m k (dec v)))
                 (assoc m k v)))
    {}
    remaining))

(defn solve [workers f init]
  (loop [acc       init
         dep-map   (input->dep-map input)
         remaining (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (iterate inc 61))]
    (if (seq remaining)
      (let [available  (reduce disj (set (keys remaining)) (keys dep-map))
            in-process (take workers (sort available))]
        (let [completed (filter #(== 1 (remaining %)) in-process)]
          (recur
            (f acc completed)
            (eliminate dep-map completed)
            (decrement remaining in-process))))
      acc)))

(defn solve-1 []
  (solve 1 (partial apply str) ""))

(defn solve-2 []
  (solve 5 (fn [seconds _] (inc seconds)) 0))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
