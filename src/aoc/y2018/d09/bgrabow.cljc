(ns aoc.y2018.d09.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d09.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]])
  #?(:clj (:import [java.util ArrayDeque])))

(defn parse [input]
  (let [[n-players last-marble] (map u/parse-int (re-seq #"\d+" input))]
    {:n-players   n-players
     :last-marble last-marble}))

(defn head-right-v [n v]
  (vec (concat (subvec v n)
               (subvec v 0 n))))

(defn head-left-v [n v]
    (vec (concat (subvec v (- (count v) n))
                 (subvec v 0 (- (count v) n)))))

#?(:clj (do
          (defn head-right [n ^ArrayDeque dequeue]
            (doall (repeatedly n #(.addLast dequeue (.removeFirst dequeue))))
            dequeue)

          (defn head-left [n ^ArrayDeque dequeue]
            (doall (repeatedly n #(.addFirst dequeue (.removeLast dequeue))))
            dequeue)))

;(defn next-circle-vector [circle next-scoring-marble]
;  (->> circle
;      (head-right-v 1)
;      (#(reduce (fn [circle marble]
;                  (->> circle
;                       (head-right 1)
;                       (conj marble)))
;                %
;                (range (- next-scoring-marble 22) next-scoring-marble)))))

(defn next-circle-vector [circle next-scoring-marble]
    [(vec (concat (interleave (range (- next-scoring-marble 4) next-scoring-marble) (subvec circle 20 24))
                  (subvec circle 24)
                  (subvec circle 0 1)
                  (interleave (subvec circle 1 19) (range (- next-scoring-marble 22) (- next-scoring-marble 4)))))
     (+ (get circle 19)
        next-scoring-marble)])

(defn next-circle
  #?(:clj ([^ArrayDeque circle next-scoring-marble]
           (head-right 1 circle)
           (doseq [x (range (- next-scoring-marble 22) next-scoring-marble)]
             (head-right 1 circle)
             (.addLast circle x))
           (head-left 7 circle)
           (let [removed (.removeLast circle)]
                [circle (+ removed next-scoring-marble)]))
     :cljs ([circle next-scoring-marble]
            [(vec (concat (interleave (range (- next-scoring-marble 4) next-scoring-marble) (subvec circle 20 24))
                          (subvec circle 24)
                          (subvec circle 0 1)
                          (interleave (subvec circle 1 19) (range (- next-scoring-marble 22) (- next-scoring-marble 4)))))
             (+ (get circle 19)
                next-scoring-marble)])))

(defn scores [circle scoring-marbles]
  (map second (drop 1 (reductions (fn [[circle _] scoring-marble]
                                    (next-circle circle scoring-marble)) [circle nil] scoring-marbles))))

(defn circle-after-46-turns [^Integer allocate-size]
  #?(:clj (let [circle (ArrayDeque. allocate-size)]
            (doseq [e [42 4 43 18 44 19 45 2 24 20
                       25 10 26 21 27 5 28 22 29 11
                       30 1 31 12 32 6 33 13 34 3 35
                       14 36 7 37 15 38 0 39 16 40 8 41]]
                   (.addLast circle e))
            circle)
     :cljs [42 4 43 18 44 19 45 2 24 20
            25 10 26 21 27 5 28 22 29 11
            30 1 31 12 32 6 33 13 34 3 35
            14 36 7 37 15 38 0 39 16 40 8 41]))

(defn circle-vec-after-46-turns []
  [42 4 43 18 44 19 45 2 24 20
   25 10 26 21 27 5 28 22 29 11
   30 1 31 12 32 6 33 13 34 3 35
   14 36 7 37 15 38 0 39 16 40 8 41])

;(println (seq (first (next-circle-dequeue (circle-after-46-turns 100) 69))))
;(println (seq (first (next-circle-vector (circle-vec-after-46-turns) 69))))

(defn all-scoring-turns []
  (concat '(32 63) (scores (circle-after-46-turns 10000000)
                           (iterate #(+ 23 %) 69))))

(defn scoring-players [n-players]
  (drop 1 (iterate #(rem (+ % 23) n-players) 0)))

(defn solve-1 []
  (let [{:keys [n-players last-marble]} (parse input)
        n-scoring-marbles (quot last-marble 23)
        player-scores (zipmap (range n-players) (repeat 0))]
    (->> (reduce (fn [p-s [player score]] (update p-s player #(+ % score)))
                 player-scores
                 (map vector (scoring-players n-players) (take n-scoring-marbles (all-scoring-turns))))
         vals
         (apply max))))

(defn solve-2 []
  (let [{:keys [n-players last-marble]} (parse input)
        n-scoring-marbles (quot (* 100 last-marble) 23)
        player-scores (zipmap (range n-players) (repeat 0))]
    (->> (reduce (fn [p-s [player score]] (update p-s player #(+ % score)))
                 player-scores
                 (map vector (scoring-players n-players) (take n-scoring-marbles (all-scoring-turns))))
         vals
         (apply max))))

(deftest ^:skip-cljs part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest ^:skip-cljs part-2
         (is (= (str answer-2)
                (str (solve-2)))))
