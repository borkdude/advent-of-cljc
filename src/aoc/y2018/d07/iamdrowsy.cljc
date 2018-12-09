(ns aoc.y2018.d07.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d07.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [com.rpl.specter :as s]
    [clojure.string :as str]
    [clojure.set :as set]))

(def re #"Step (\w) must be finished before step (\w) can begin.")

(defn parse-line [line]
  (let [[_ x y] (re-matches re line)]
    [x y]))

(defn parse-input []
  (let [pairs (map parse-line (str/split-lines input))]
    (reduce (fn [m [x y]]
              (s/setval [(s/keypath y) s/NONE-ELEM] x m))
            (zipmap (map first pairs) (repeat #{}))
            pairs)))

(defn possible-steps [solved-set open-steps]
  (s/select [s/ALL (s/selected? s/LAST #(set/subset? % solved-set)) s/FIRST]
            open-steps))

(defn next-step [solved-steps open-steps]
  (let [solved-set (set solved-steps)]
    (first (sort (possible-steps solved-set open-steps)))))

(defn solve-1 []
  (loop [open-steps (parse-input)
         solved-steps []]
    (if (empty? open-steps)
      (str/join solved-steps)
      (let [next (next-step solved-steps open-steps)]
        (recur (dissoc open-steps next)
               (conj solved-steps next))))))

(defn char->int ^long [c]
  #?(:clj (int c)
     :cljs (.charCodeAt c 0)))

(defn task-duration [task]
  (- (char->int (first task)) 4))

(defn build-task [current-time task]
  {:task task
   :end-time (+ current-time (task-duration task))})

(defn solve-2 []
  (loop [open-tasks (parse-input)
         solved-tasks #{}
         open-events [{:task "" :end-time 0}]]
    (if (empty? open-tasks)
      (apply max (map :end-time open-events))
      (let [events-by-time (sort-by :end-time open-events)
            finished-event (first events-by-time)
            finished-task (:task finished-event)
            now-solved (conj solved-tasks finished-task)
            current-time (:end-time finished-event)
            free-workers (- 6 (count open-events))
            next-events (take free-workers (map (partial build-task current-time)
                                                (possible-steps now-solved open-tasks)))
            next-tasks (into #{} (map :task next-events))]
        (recur (s/setval [s/MAP-KEYS next-tasks] s/NONE open-tasks)
               now-solved
               (into (rest events-by-time) next-events))))))


(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
