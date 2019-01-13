(ns aoc.y2017.d17.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d17.data :refer [input answer-1 answer-2]]
   [clojure.test :as t :refer [is testing]]))

(defn iter-spinlock [state steps pos n]
  (if (pos? n)
    (let [i         (count state)
          nextpos   (inc (mod (+ pos steps) i))
          nextstate (concat
                     (take nextpos state)
                     [i]
                     (drop nextpos state))]
      (recur nextstate steps nextpos (dec n)))
    state))

(defn solve-1 []
  (let [spin-seq (iter-spinlock [0] input 0 2017)]
    (->> (drop-while #(not= % 2017) spin-seq)
         rest
         first)))

(defn iter-spinlock2 [state steps pos i n]
  (if (pos? n)
    (let [nextpos   (mod (+ pos steps) i)
          nextstate (if (zero? nextpos) i state)]
      (recur nextstate steps (inc nextpos) (inc i) (dec n)))
    state))

(defn solve-2 []
  (iter-spinlock2 [0] input 0 1 50000000))

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
