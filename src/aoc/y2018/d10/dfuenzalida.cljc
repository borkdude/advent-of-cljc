(ns aoc.y2018.d10.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d10.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn parse-input [lines]
  (->> lines
       s/split-lines
       (map #(re-seq #".*=<(.*),(.*)> .*=<(.*),(.*)>" %))
       (map first)
       (map rest)
       (map #(map read-string %))))

(defn move [points]
  (for [[x y dx dy] points]
    [(+ x dx) (+ y dy) dx dy]))

(defn graph [points]
  (let [points-set                (set (map (juxt first second) points))
        [min-x max-x min-y max-y] (for [projfn [first second] redfn [min max]]
                                    (apply redfn (map projfn points-set)))
        width                     (- max-x min-x)
        lines                     (->>
                                   (for [y (range min-y (inc max-y))
                                         x (range min-x (inc max-x))
                                         :let [s (if (points-set [x y]) "#" ".")]]
                                     s)
                                   (partition (inc width))
                                   (map #(apply str %)))]
    (apply str (interpose "\n" lines))))

(defn find-extremes [points]
    (let [points-set (set (map (juxt first second) points))]
      (for [projfn [first second] redfn [min max]]
        (apply redfn (map projfn points-set)))))

;; Found that the points group in a smaller are each time until they start expanding again
(defn find-msg [points time]
  (let [[min-x max-x _ _] (find-extremes points)
        curr-width (- max-x min-x)
        [min-x max-x _ _] (find-extremes (move points))
        next-width (- max-x min-x)]
    (if (> next-width curr-width)
      [time (graph points)]
      (recur (move points) (inc time)))))

(defn solve-1 []
  (let [graph (second (find-msg (parse-input input) 0))]
    ;;(println graph) ;; Uncomment if you want to see the message
    (hash graph)))

(defn solve-2 []
  (first (find-msg (parse-input input) 0)))

(deftest part-1
  (is (= "-282693289"
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
)
