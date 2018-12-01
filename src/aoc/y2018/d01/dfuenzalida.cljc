(ns aoc.y2018.d01.dfuenzalida
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(def data
  (map (comp u/read-string
             #(str/replace % #"\+" ""))
       (str/split-lines input)))

(defn solve-1 []
  (reduce + 0 data))

(defn conj-or-find [xset xs]
  (when (seq xs)
    (let [x (first xs)]
      (if (xset x)
        x
        (recur (conj xset x) (rest xs))))))

(defn solve-2 []
  (conj-or-find #{} (reductions + (cycle data))))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
