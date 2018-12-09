(ns aoc.y2018.d07.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :refer [deftest read-string format]]
    [aoc.y2018.d07.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]
    [clojure.set :as set])
  #?(:clj (:import (clojure.lang PersistentQueue))))

(def parse (memoize
             (fn [input] (map #(vec (re-seq #"\b[A-Z]\b" %)) (str/split-lines input)))))

(defn split-set-with [pred s]
  (let [taken (take-while pred s)]
    [taken (set/difference s (set taken))]))

(defn map-vals [pred m]
  (reduce (fn [m k] (update m k pred))
          m
          (keys m)))

(defn blocks-graph [deps]
  (map-vals #(apply sorted-set (map second %)) (group-by first deps)))
(def mem-blocks-graph (memoize blocks-graph))

(defn blocked-by-graph [deps]
  (map-vals #(apply hash-set (map first %)) (group-by second deps)))
(def mem-blocked-by-graph (memoize blocked-by-graph))

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

; Create an "open-nodes" sorted-set
; Get rid of "completed-tasks" set
; Start on the lexicographically first open node
; Go to the nodes it blocked. Remove each edge from the blocked-by graph.
; Check the new blocked-by graph to see which nodes are open and add them
; to the open-nodes set.
; Take the first node from open-nodes and repeat.
(defn solve-1 []
  (let [all-tasks '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
        deps (parse input)
        x-blocks-y (mem-blocks-graph deps)]
    (loop [acc []
           x-blocked-by-y (mem-blocked-by-graph deps)
           open-tasks (apply sorted-set (remove x-blocked-by-y all-tasks))]
      (if (empty? open-tasks)
        (apply str acc)
        (let [current-task (first open-tasks)
              uncovered (x-blocks-y current-task)
              blocked-by (reduce (fn [deps t]
                                   (update deps t #(disj % current-task)))
                                 x-blocked-by-y uncovered)
              newly-open (remove #(some identity (blocked-by %)) uncovered)]
          (recur (conj acc current-task)
                 blocked-by
                 (-> open-tasks
                     (disj current-task)
                     (#(apply conj % newly-open)))))))))

(defn solve-2 []
  (let [all-tasks '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
        task-length (zipmap all-tasks (range 61 87))
        deps (parse input)
        x-blocks-y (mem-blocks-graph deps)
        x-blocked-by-y (mem-blocked-by-graph deps)
        open-tasks (remove x-blocked-by-y all-tasks)]
    (loop [working (apply sorted-set (zipmap (map task-length open-tasks) open-tasks))
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
