(ns aoc.y2017.d19.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d19.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(def left   {:up :left, :left :down, :down :right, :right :up})
(def right  {:up :right, :right :down, :down :left, :left :up})
(def speed  {:up [0 -1] :down [0 1] :left [-1 0] :right [1 0]})
(def alpha? (->> "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (map str) (into #{})))

(defn find-start [pipe-map]
  [(-> pipe-map first (.indexOf "|")) 0])

(defn char-at [s n]
  (.substring s n (inc n)))

(defn travel [pipe-map letters [x y] dir dist]
  (let [c       (-> pipe-map (get y) (char-at x))
        [sx sy] (speed dir)]
    (cond
      (= " " c) [(apply str letters) dist]
      (alpha? c) (recur pipe-map
                        (conj letters c)
                        [(+ x sx) (+ y sy)]
                        dir
                        (inc dist))
      (= "+" c) (let [speedl  (-> dir left speed)
                      speedr  (-> dir right speed)
                      [xl yl] [(+ x (first speedl)) (+ y (second speedl))]
                      [xr yr] [(+ x (first speedr)) (+ y (second speedr))]
                      cl      (-> pipe-map (get yl) (char-at xl))]
                  (if (not= " " cl)
                    (recur pipe-map letters [xl yl] (left dir) (inc dist))
                    (recur pipe-map letters [xr yr] (right dir) (inc dist))))
      :else   (let [[sx sy] (speed dir)]
                (recur pipe-map letters [(+ x sx) (+ y sy)] dir (inc dist))))))

(defn read-input []
  (s/split-lines input))

(defn solve []
  (travel (read-input) [] (find-start (read-input)) :down 0))

(defn solve-1 []
  (first (solve)))

(defn solve-2 []
  (last (solve)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
)
