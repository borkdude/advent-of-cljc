(ns aoc.y2018.d01.iamdrowsy
  (:require
    [aoc.utils :as u :refer [deftest]]
    [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [com.rpl.specter :as s]
    [net.cgrand.xforms :as x]))

(def NUMBER
  (s/parser u/read-string str))

(def NUMBERS-IN-LINES
  (s/path (s/regex-nav #"[+-][0-9]*") NUMBER))

(defn duplicates [rf]
  (let [seen (volatile! #{})]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (if (contains? @seen input)
          (rf result input)
          (do (vswap! seen conj input)
              result))))))

(defn solve-1 []
  (reduce + 0 (s/traverse NUMBERS-IN-LINES input)))

(defn solve-2 []
  (->> (reductions + 0 (cycle (s/select NUMBERS-IN-LINES input)))
       (sequence duplicates)
       (first)))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
