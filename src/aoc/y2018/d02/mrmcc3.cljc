(ns aoc.y2018.d02.mrmcc3
  (:require
    [aoc.utils :as u :refer [deftest]]
    [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(def codes
  (str/split-lines input))

(defn solve-1 []
  (let [freqs (map #(-> % frequencies vals set) codes)]
    (* (count (filter #(% 2) freqs))
       (count (filter #(% 3) freqs)))))

;; aoc.y2018.d02.mfikes/solve-2 is clearly superior
(defn solve-2 []
  (first
    (for [a codes b codes
          :let [sim (apply str (map #(if (= %1 %2) %1) a b))]
          :when (= (count sim) (dec (count a)))]
      sim)))

(deftest part-1
         (is (= answer-1 (solve-1))))

(deftest part-2
         (is (= answer-2 (solve-2))))
