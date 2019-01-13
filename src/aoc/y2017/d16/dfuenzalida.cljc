(ns aoc.y2017.d16.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d16.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn read-input []
  (s/split input #","))

(def programs "abcdefghijklmnop")

(defn spin [^String s ^long n]
  (let [ls (count s)]
    (str (.substring s (- ls n))
         (.substring s 0 (- ls n)))))

(defn char-at [^String s ^long n]
  (.substring s n (inc n)))

(defn exchange [^String s ^long a ^long b]
  (let [table {b (char-at s a) a (char-at s b)}]
    (apply str (map #(get table % (char-at s %)) (range (count s))))))

(defn partner [^String s ^String a ^String b]
  (let [table {(first b) (first a) (first a) (first b)}]
    (apply str (map #(get table % %) s))))

(defn reductor [^String s ^String op]
  (let [operand (char-at op 0)
        params  (into [] (s/split (.substring op 1) #"/"))]
    (condp = operand
      "s" (spin s (read-string (params 0)))
      "x" (exchange s (read-string (params 0)) (read-string (params 1)))
      "p" (partner s (params 0) (params 1))
      s)))

(defn solve-1 []
  (reduce reductor programs (read-input)))

(defn count-steps [^String s i]
  (let [s2 (reduce reductor s (read-input))]
    (if (= programs s2)
      i
      (recur s2 (inc i)))))

(defn solve-2 []
  ;; I suspected iterating would eventually return the same string
  (let [identity-steps (count-steps programs 1) ;; this many steps returns same input
        iterate-steps  (mod 1000000000 identity-steps) ;; steps worth doing
        problem-input  (read-input)]
    (->> (iterate #(reduce reductor % problem-input) programs)
         rest
         (take iterate-steps)
         last)))

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
