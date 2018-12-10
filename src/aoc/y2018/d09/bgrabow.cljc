(ns aoc.y2018.d09.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d09.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]])
  (:import [java.util ArrayDeque]))

(def my-input "470 players; last marble is worth 72170 points")

(defn parse [input]
  (let [[n-players last-marble] (map u/parse-int (re-seq #"\d+" input))]
    {:n-players   n-players
     :last-marble last-marble}))

(defn head-right [n ^ArrayDeque dequeue]
  (doall (repeatedly n #(.addLast dequeue (.removeFirst dequeue))))
  dequeue)

(defn head-left [n ^ArrayDeque dequeue]
  (doall (repeatedly n #(.addFirst dequeue (.removeLast dequeue))))
  dequeue)

(defn next-circle-dequeue [^ArrayDeque circle next-scoring-marble]
  (head-right 1 circle)
  (doseq [x (range (- next-scoring-marble 22) next-scoring-marble)]
    (head-right 1 circle)
    (.addLast circle x))
  (head-left 7 circle)
  (let [removed (.removeLast circle)]
    [circle (+ removed next-scoring-marble)]))

(defn scores [circle scoring-marbles]
  (map second (drop 1 (reductions (fn [[circle _] scoring-marble]
                                    (next-circle-dequeue circle scoring-marble)) [circle nil] scoring-marbles))))

(defn circle-after-46-turns [^Integer allocate-size]
  (let [circle (ArrayDeque. allocate-size)]
    (doseq [e [42 4 43 18 44 19 45 2 24 20
               25 10 26 21 27 5 28 22 29 11
               30 1 31 12 32 6 33 13 34 3 35
               14 36 7 37 15 38 0 39 16 40 8 41]]
           (.addLast circle e))
    circle))

(defn all-scoring-turns []
  (concat '(32 63) (scores (circle-after-46-turns 10000000)
                           (iterate #(+ 23 %) 69))))

(defn scoring-players [n-players]
  (drop 1 (iterate #(rem (+ % 23) n-players) 0)))

(defn solve-1 []
  (let [{:keys [n-players last-marble]} (parse my-input)
        n-scoring-marbles (quot last-marble 23)
        player-scores (zipmap (range n-players) (repeat 0))]
    (->> (reduce (fn [p-s [player score]] (update p-s player #(+ % score)))
                 player-scores
                 (map vector (scoring-players n-players) (take n-scoring-marbles (all-scoring-turns))))
         vals
         (apply max))))

(defn solve-2 []
  (let [{:keys [n-players last-marble]} (parse my-input)
        n-scoring-marbles (quot (* 100 last-marble) 23)
        player-scores (zipmap (range n-players) (repeat 0))]
    (->> (reduce (fn [p-s [player score]] (update p-s player #(+ % score)))
                 player-scores
                 (map vector (scoring-players n-players) (take n-scoring-marbles (all-scoring-turns))))
         vals
         (apply max))))

(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest part-2
         (is (= (str answer-2)
                (str (solve-2)))))
