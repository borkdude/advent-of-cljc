(ns aoc.y2018.d07.borkdude
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d07.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as str]
   [clojure.set :as set]))

(def re #"Step (\w) must be finished before step (\w) can begin\.")

(defn parse-line [s]
  (let [[_ step enables] (re-find re s)]
    {:step (first step)
     :enables (first enables)}))

(def data (map parse-line (str/split-lines input)))

(defn char->int ^long [c]
  #?(:clj (int c)
     :cljs (.charCodeAt c 0)))

(defn min-char [chars]
  (apply min-key char->int chars))

(def step->enablee (delay (group-by :enables data)))
(def enablers (delay (set (map :step data))))
(def enablees (delay (set (map :enables data))))
(def enabled-by (memoize (fn [c] (set (map :step (get @step->enablee c))))))
(def start (delay (min-char (set/difference @enablers @enablees))))
(def start-queue (delay (disj (into @enablers @enablees) @start)))

(defn solve-1 []
  (apply str
         (loop [order [@start]
                queue @start-queue]
           (if (empty? queue)
             order
             (let [expand (first
                           (filter
                            (fn [l]
                              (every? (set order)
                                      (enabled-by l)))
                            (sort queue)))]
               (recur (conj order expand)
                      (disj queue expand)))))))

(defn work [c]
  (let [v (- (char->int c) 4)]
    (assoc (vec (repeat v nil))
           0 c)))

(defn assign [workers letter]
  (let [load (work letter)]
    (loop [processed []
           [w & ws] workers]
      (cond (not w) nil
            (empty? w)
            (into (conj processed load)
                  ws)
            :else (recur (conj processed w)
                         ws)))))

(defn distribute [workers letters]
  (loop [workers workers
         [l & ls :as letters] letters
         processed-letters []]
    (if (not l) [workers letters processed-letters]
        (if-let [ws (assign workers l)]
          (recur ws ls (conj processed-letters l))
          [workers letters processed-letters]))))

(defn first-value [workers]
  (some identity (map peek workers)))

(defn pop* [v]
  (if (empty? v) v (pop v)))

(defn solve-2 []
  (loop [order []
         queue @start-queue
         seconds 0
         workers (first (distribute (vec (repeat 5 [])) [@start]))]
    (if (and (empty? queue)
             (every? empty? workers))
      seconds
      (let [worker-value (first-value workers)
            workers (if worker-value (map pop* workers) workers)
            order (if worker-value (conj order worker-value) order)
            available (filter
                       (fn [l]
                         (every? (set order) (enabled-by l)))
                       (sort queue))
            [workers _ processed] (distribute workers available)]
        (recur order
               (set/difference queue processed)
               (inc seconds)
               (if worker-value workers (map pop* workers)))))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true)
  )
