(ns aoc.y2018.d07.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d07.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]
    [clojure.set :as set]))

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

(defn solve-2 [])
  ;; TODO


(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
