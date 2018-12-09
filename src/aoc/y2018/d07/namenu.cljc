(ns aoc.y2018.d07.namenu
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d07.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as str]))


(defn parse [s]
  (let [[_ x y] (re-find #"Step (\S) must be finished before step (\S) can begin." s)]
    [(get x 0) (get y 0)]))

(def ^:dynamic *num-workers* 5)
(def ^:dynamic *step-duration* 60)

(defn data->graph [data]
  (reduce (fn [g [x y]]
            (update g y (fnil conj #{}) x))
          {}
          data))

(defn update-vals
  "Applies f to all the vals in the map"
  [m f]
  (reduce-kv (fn [m k v] (assoc m k (f v))) {} (or m {})))

(defn remove-node [graph node]
  (update-vals graph (fn [v] (disj v node))))

(defn worker-available? [workers]
  (< (count workers) *num-workers*))

(defn char->int ^long [c]
  #?(:clj (int c)
     :cljs (.charCodeAt c 0)))

(defn schedule [workers task cur-time]
  (let [duration (+ (- (char->int task) 64) *step-duration*)]
    (assoc workers task (+ duration cur-time))))

(defn wait-till-available [workers]
  (let [done (apply min-key val workers)]
    [done (dissoc workers (first done))]))

(defn solve-1 []
  (let [data  (map parse (str/split-lines input))
        nodes (into #{} (flatten data))
        graph (data->graph data)]
    (loop [todo   nodes
           deps   graph
           result ""]
      (if (empty? todo)
        result
        (let [no-deps (filter #(empty? (deps %)) todo)
              task    (first (sort no-deps))]
          (recur (disj todo task)
                 (remove-node deps task)
                 (str result task)))))))

(defn solve-2 []
  (let [data  (map parse (str/split-lines input))
        nodes (into #{} (flatten data))
        graph (data->graph data)]
    (loop [elapsed 0
           workers {}
           todo    nodes
           deps    graph]
      (if (empty? todo)
        ; wait all workers
        (second (apply max-key val workers))
        (let [no-deps (filter #(empty? (deps %)) todo)
              task    (first (sort no-deps))]
          (if (and task (worker-available? workers))
            (recur elapsed (schedule workers task elapsed) (disj todo task) deps)
            (let [[[task time] workers] (wait-till-available workers)]
              (recur time workers todo (remove-node deps task)))))))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
