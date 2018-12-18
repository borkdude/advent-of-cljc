(ns aoc.y2018.d18.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d18.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [com.rpl.specter :refer [pred= setval NONE MAP-VALS]]
    [clojure.string :as str]))

(defn env-fn [x-dim]
  (fn [board index]
    (map board
         [(- index x-dim 2) (- index x-dim 1) (- index x-dim)
          (- index 1) (+ index 1)
          (+ index x-dim) (+ index x-dim 1) (+ index x-dim 2)])))

(defn single-step [env board middle current]
  (let [env-freqs (frequencies (env board middle))]
    (cond (and (= \. current) (<= 3 (env-freqs \| 0))) \|
          (and (= \| current) (<= 3 (env-freqs \# 0))) \#
          (and (= \# current) (or (nil? (env-freqs \#)) (nil? (env-freqs \|)))) \.
          :else current)))

(defn step [env board]
  (reduce-kv (fn [m k v] (assoc m k (single-step env board k v)))
             {}
             board))

(defn resources [board]
  (let [freqs (frequencies (vals board))]
    (* (freqs \#) (freqs \|))))

(defn solve-1 []
  (let [board-with-linebreaks (zipmap (range) input)
        x-dim (str/index-of input \newline)
        board (setval [MAP-VALS (pred= \newline)] NONE board-with-linebreaks)]
    (resources (nth (iterate (partial step (env-fn x-dim)) board) 10))))

(defn find-repeating-board [env board]
  (loop [cache {}
         index 0
         current board]
    (if-let [first-index (cache current)]
      {:first-index first-index
       :next-index index
       :board current}
      (recur (assoc cache current index)
             (inc index)
             ((partial step env) current)))))

(defn fast-step-to [env init-board index]
  (let [{:keys [first-index next-index board]} (find-repeating-board env init-board)
        repeat-steps (- next-index first-index)
        missing-steps (rem (- index first-index)  repeat-steps)]
    (nth (iterate (partial step env) board) missing-steps)))

(defn solve-2 []
  (let [board-with-linebreaks (zipmap (range) input)
        x-dim (str/index-of input \newline)
        board (setval [MAP-VALS (pred= \newline)] NONE board-with-linebreaks)]
    (resources (fast-step-to (env-fn x-dim) board 1000000000))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest ^:slow ^:skip-cljs part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests))

