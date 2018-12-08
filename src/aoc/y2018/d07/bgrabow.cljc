(ns aoc.y2018.d07.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :refer [deftest read-string format]]
    [aoc.y2018.d07.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]
    [clojure.set :as set])
  #?(:clj (:import (clojure.lang PersistentQueue))))

(defn parse [input]
  (map #(vec (re-seq #"\b[A-Z]\b" %)) (str/split-lines input)))

(defn collect-deps [deps]
  (map (fn [[k v]] [k (into #{} (map first v))])
       (group-by second deps)))

(defn solve-1 []
  (let [tasks-remaining (map str (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
        dep-graph (->> (parse input)
                       collect-deps
                       (into {}))]
    (->> (loop [acc []
                tasks-remaining tasks-remaining
                completed-tasks #{}]
           (if (empty? tasks-remaining)
             acc
             (let [has-remaining-deps #(some identity (set/difference (dep-graph %) completed-tasks))
                   [unripe-tasks rem] (split-with has-remaining-deps tasks-remaining)
                   next-task (first rem)]
               (recur (conj acc next-task)
                      (concat unripe-tasks (rest rem))
                      (conj completed-tasks next-task)))))
         (apply str))))

(defn split-set-with [pred s]
  (let [taken (take-while pred s)]
    [taken (set/difference s (set taken))]))

(defn map-vals [pred m]
  (reduce (fn [m k] (update m k pred))
          m
          (keys m)))

(defn blocks-graph [deps]
  (map-vals #(apply sorted-set (map second %)) (group-by first deps)))

(defn blocked-by-graph [deps]
  (map-vals #(apply hash-set (map first %)) (group-by second deps)))

(defn resolve-deps [blocked-by blocks completed-tasks]
  (reduce (fn [[blocked-by uncovered-tasks] t]
            (let [partially-uncovered (blocks t)
                  new-blocked-by (reduce
                                   (fn [bb covered-task]
                                     (update bb covered-task #(disj % t)))
                                   blocked-by partially-uncovered)
                  fully-uncovered (filter #(= 0 (count (new-blocked-by %))) partially-uncovered)]
              [new-blocked-by (concat uncovered-tasks fully-uncovered)]))
          [blocked-by '()] completed-tasks))

(defn solve-2 []
  (let [all-tasks '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
        task-length (zipmap all-tasks (range 61 87))
        x-blocks-y (blocks-graph (parse input))
        x-blocked-by-y (blocked-by-graph (parse input))
        initial-tasks (remove x-blocked-by-y all-tasks)]
    (loop [working (apply sorted-set (zipmap (map task-length initial-tasks) initial-tasks))
           blocked-by x-blocked-by-y
           current-time (first (first working))]
      (let [[c-t still-working] (split-set-with #(= current-time (first %)) working)
            completed-tasks (map second c-t)
            [remaining-blocked-by uncovered-tasks] (resolve-deps blocked-by x-blocks-y completed-tasks)
            new-tasks (map (fn [t] [(+ (task-length t) current-time) t])
                           uncovered-tasks)
            new-working (apply conj still-working new-tasks)]
        (if (empty? new-working)
          current-time
          (recur new-working
                 remaining-blocked-by
                 (first (first new-working))))))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
