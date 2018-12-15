(ns aoc.y2018.d14.mfikes
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d14.data :refer [input answer-1 answer-2]]
   [clojure.string :as string]
   [clojure.test :as t :refer [is testing]]))

(def digits
  (merge
    (zipmap (range 10) (map vector (range 10)))
    (zipmap (map #(+ % 10) (range 9)) (map #(vector 1 %) (range 9)))))

(defn solve-1 [n]
  (loop [idx-1 0 idx-2 1 recipes [3 7]]
    (let [recipe-1 (recipes idx-1)
          recipe-2 (recipes idx-2)
          sum      (+ recipe-1 recipe-2)
          recipes  (apply conj recipes (digits sum))
          c        (count recipes)]
      (if (> c (+ n 10))
        (string/join (subvec recipes n (+ n 10)))
        (recur (mod (+ idx-1 recipe-1 1) c) (mod (+ idx-2 recipe-2 1) c) recipes)))))

(defn solve-part-1 []
  (solve-1 input))

(defn solve-2 [desired]
  (loop [idx-1 0 idx-2 1 recipes [3 7]]
    (let [recipe-1 (recipes idx-1)
          recipe-2 (recipes idx-2)
          sum      (+ recipe-1 recipe-2)
          recipes  (apply conj recipes (digits sum))
          c        (count recipes)]
      (cond
        (< c 10)
        (recur (mod (+ idx-1 recipe-1 1) c) (mod (+ idx-2 recipe-2 1) c) recipes)

        (= desired (subvec recipes (- c (count desired) 1) (dec c)))
        (- c (count desired) 1)

        (= desired (subvec recipes (- c (count desired))))
        (- c (count desired))

        :else
        (recur (mod (+ idx-1 recipe-1 1) c) (mod (+ idx-2 recipe-2 1) c) recipes)))))

(defn digits' [n]
  (if (< n 10)
    [n]
    (conj (digits' (quot n 10)) (rem n 10))))

(defn solve-part-2 []
  (solve-2 (digits' input)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-part-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-part-2)))))
