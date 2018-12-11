(ns aoc.y2018.d09.gklijs
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d09.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]))

(defn create-game [players marbles]
  {:next-marble (transient (vec (replicate (inc marbles) 0)))
   :scores      (transient (vec (replicate players 0)))
   :history     (transient [0 0])
   :ctp         (transient [0 0 0])})

(defn play [game value]
  (let [{:keys [next-marble scores history ctp]} game
        turn-counter (inc (get ctp 1))
        player-counter (inc (get ctp 2))
        player-counter (if (= player-counter (count scores)) 0 player-counter)]
    (if
      (= turn-counter 23)
      (do (assoc! scores player-counter (+ (get scores player-counter) (get history 1) value))
          (assoc! next-marble (get history 0) (get next-marble (get history 1)))
          (assoc! ctp 0 (get next-marble (get history 1)))
          (assoc! ctp 1 0)
          (assoc! ctp 2 player-counter)
          game)
      (let [current-marble (get next-marble (get ctp 0))]
        (if (= turn-counter 19)
          (assoc! history 1 current-marble)
          (if (= turn-counter 18) (assoc! history 0 value)))
        (assoc! next-marble value (get next-marble current-marble))
        (assoc! next-marble current-marble value)
        (assoc! ctp 0 value)
        (assoc! ctp 1 turn-counter)
        (assoc! ctp 2 player-counter)
        game))))

(defn solve [players last-marble]
  (let [game (reduce play (create-game players last-marble) (range 1 (inc last-marble)))]
    (apply max (persistent! (:scores game)))))

(defn solve-1 []
  (let [[players last-marble] (map read-string (re-seq #"\d+" input))]
    (solve players last-marble)))

(defn solve-2 []
  (let [[players last-marble] (map read-string (re-seq #"\d+" input))]
    (solve players (* 100 last-marble))))

(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest ^:instrumented part-2
         (is (= (str answer-2)
                (str (solve-2)))))
