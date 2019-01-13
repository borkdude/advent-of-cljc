(ns aoc.y2017.d20.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d20.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn parse-line [s]
  (let [[p1 p2 p3 v1 v2 v3 a1 a2 a3] (map read-string (re-seq #"[-]?\d+" s))]
    {:p [p1 p2 p3] :v [v1 v2 v3] :a [a1 a2 a3]}))


(defn index-of-max
  "Returns the index of the value that yields the max value for f(x)"
  [f xs]
  (first
   (apply max-key (comp f second) (map-indexed vector xs))))

(defn read-input []
  (s/split-lines input))

(defn acceleration [{[ax ay az] :a}]
  (+ (Math/abs ax) (Math/abs ay) (Math/abs az)))

(defn min-acceleration [p]
  (* -1 (acceleration p)))

(defn solve-1 []
  (let [input-parts (map parse-line (read-input))]
    (index-of-max min-acceleration input-parts)))

(defn move [{:keys [p v a]}]
  {:p (mapv + p v a)
   :v (mapv + v a)
   :a a})

(defn find-survivors [parts n]
  (if (neg? n)
    (count parts)
    (let [collisions    (->> parts
                             (map :p)
                             frequencies
                             (filter (fn [[k v]] (> v 1)))
                             (map first)
                             (into #{}))
          not-colliding (filter
                         #(not (some collisions [(:p %)]))
                         parts)]
      (recur (map move not-colliding) (dec n)))))

(defn solve-2 []
  (let [input-parts (map parse-line (read-input))]
    (find-survivors input-parts 1000)))

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
