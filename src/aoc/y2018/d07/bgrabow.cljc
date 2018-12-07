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
  (map #(re-seq #"\b[A-Z]\b" %) (str/split-lines input)))

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

(defn has-remaining-deps [dep-graph completed-tasks task]
  (some identity (set/difference (dep-graph task) completed-tasks)))

(defn unblocked-tasks [is-blocked? tasks-remaining]
  (lazy-seq
    (let [[left [next-task & right]] (split-with is-blocked? tasks-remaining)
          remaining (concat left right)]
      (when next-task
        (cons [next-task remaining] (unblocked-tasks is-blocked? remaining))))))

(defn empty-queue []
  #?(:clj  PersistentQueue/EMPTY
     :cljs (.-EMPTY PersistentQueue)))

(defn split-set-with [pred s]
  (let [taken (take-while pred s)]
    [taken (set/difference s (set taken))]))

(defn pop-n [n q]
  (nth (iterate pop q) n))

(defn solve-2 []
  (let [all-tasks '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
        finished-state (into #{} all-tasks)
        task-length (zipmap all-tasks (range 61 87))
        dep-graph (->> (parse input)
                       collect-deps
                       (into {}))]
    (loop [tasks-remaining all-tasks
           completed-tasks #{}
           in-progress (sorted-set)
           backlog (empty-queue)
           current-time 0]
      ;(do
      ;  (println "tasks-remaining" tasks-remaining)
      ;  (println "completed" completed-tasks)
      ;  (println "working" in-progress)
      ;  (println "backlog" (seq backlog) backlog)
      ;  (when (pos? (count backlog)) (println "FOUND---------------------"))
      ;  (println "current time" current-time))

      (if (= completed-tasks finished-state)
        0
        (let [finished? #(= current-time (first %))
              [newly-completed still-working] (split-set-with finished? in-progress)
              all-completed (apply conj completed-tasks (map second newly-completed))
              ;_ (do
              ;    (println "newly-completed" newly-completed)
              ;    (println "completed" all-completed))
              free-workers (- 5 (count still-working))
              ready-tasks (unblocked-tasks (partial has-remaining-deps
                                                    dep-graph
                                                    all-completed)
                                           tasks-remaining)
              new-remaining (or (second (last ready-tasks)) tasks-remaining)
              ready-task-names (map first ready-tasks)
              temp-backlog (apply conj backlog ready-task-names)
              to-start (take free-workers temp-backlog)
              ;_ (do
              ;    (println "to-start" to-start))
              leftover-backlog (pop-n free-workers temp-backlog)
              ;_ (do
              ;    (println "leftover-backlog" (seq leftover-backlog)))
              tasks-with-finish-time (map (fn [t]
                                            [(+ (task-length t) current-time) t])
                                          to-start)
              ;_ (do
              ;    (println "tasks-with-finish-time" tasks-with-finish-time))
              new-in-progress (apply conj still-working tasks-with-finish-time)]
          (if (= all-completed finished-state)
            current-time
            (recur new-remaining
                   all-completed
                   new-in-progress
                   leftover-backlog
                   (first (first new-in-progress)))))))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
