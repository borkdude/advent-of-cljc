(ns aoc.y2017.d11.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d11.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn distance [m]
  (let [n (get m "n" 0) ne (get m "ne" 0) nw (get m "nw" 0)
        s (get m "s" 0) se (get m "se" 0) sw (get m "sw" 0)
        vects [(Math/abs (- n s))
               (Math/abs (- ne sw))
               (Math/abs (- nw se))]]
    (reduce + (drop 1 (sort vects))))) ;; sum the 2 largest numbers

(def zero
  {"n" 0 "s" 0 "ne" 0 "se" 0 "nw" 0 "sw" 0})

(defn read-input []
  (s/split input #","))

(defn solve-1 []
  (let [problem-input (read-input)]
    (-> problem-input frequencies distance)))

(defn solve-2 []
  (let [problem-input (read-input)
        moves         (reductions (fn [m k] (update-in m [k] inc)) zero problem-input)]
    (reduce max (map distance moves))))

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
