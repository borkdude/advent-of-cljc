(ns aoc.y2018.d01.genmeblog
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [clojure.string :refer [split-lines]]
   [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

;;

(def frequencies-diff (delay (map read-string (split-lines input))))

(defn freq-dup []
  (let [freqs (reductions + 0 (cycle @frequencies-diff))]
    (reduce (fn [visited freq]
              (if (visited freq)
                (reduced freq)
                (conj visited freq))) #{} freqs)))

;;

(def solve-1 #(reduce + @frequencies-diff))
(def solve-2 #(freq-dup))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
