(ns aoc.y2017.d09.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d09.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn clean [s]
  (-> s
      (s/replace #"!." "")
      (s/replace #"<.*?>" "")))

(defn score [s nesting total]
  (if (seq s)
    (condp = (first s)
      \{ (recur (rest s) (inc nesting) (+ total nesting))
      \} (recur (rest s) (dec nesting) total)
      (recur (rest s) nesting total))
    total))

(defn solve-1 []
  (score (clean input) 1 0))

(defn cancel [s]
  (s/replace s #"!." ""))

(defn garbage-size [s]
  (let [garbage-seq (->> s cancel (re-seq #"<.*?>"))]
    (-
     (reduce +
             (map count garbage-seq))
     (* 2 (count garbage-seq)))))

(defn solve-2 []
  (garbage-size input))

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
