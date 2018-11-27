(ns aos.y2017.d02.athos
  (:require
   [aos.utils :as u :refer [deftest]]
   [aos.y2017.d02.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

(defn- line-nums [line]
  (u/read-string (str "[" line "]")))

(defn solve1 [lines]
  (letfn [(line-diff [line]
            (->> (line-nums line)
                 ((juxt #(apply max %) #(apply min %)))
                 (apply -)))]
    (transduce (map line-diff) + 0 lines)))

(deftest part-1
  (is (number? (solve1 [input]))))

(defn solve2 [lines]
  (letfn [(line-div [line]
            (first
             (for [[x & ys] (->> (line-nums line)
                                 (iterate rest)
                                 (take-while seq))
                   y ys
                   :when (or (zero? (mod x y))
                             (zero? (mod y x)))]
               (/ (max x y) (min x y)))))]
    (transduce (map line-div) + 0 lines)))

(deftest part-2
  (is (number? (solve2 [input]))))
