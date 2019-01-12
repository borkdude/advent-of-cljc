(ns aoc.y2017.d15.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d15.data :refer [input answer-1 answer-2]]
   [clojure.test :as t :refer [is testing]]))

(defn read-input []
  (mapv read-string (re-seq #"\d+" input)))

(defn mk-gen [^long factor ^long modulo]
  (fn [^long n] (mod (* factor n) modulo)))

(defn iterator [gen-a ^long seed-a gen-b ^long seed-b]
  (loop [a       seed-a
         b       seed-b
         matches 0
         n       40000000]
    (if (pos? n)
      (let [val-a (gen-a a)
            val-b (gen-b b)]
        (if (= (bit-and 0xffff val-a) (bit-and 0xffff val-b))
          (recur val-a val-b (inc matches) (dec n))
          (recur val-a val-b matches (dec n))))
      matches)))
  
(defn solve-1 []
  (let [[^long seed-a ^long seed-b] (read-input)]
    (iterator (mk-gen 16807 2147483647) seed-a
              (mk-gen 48271 2147483647) seed-b)))

(defn count-matches [seq-a seq-b ^long matches ^long n]
  (if (pos? n)
    (let [^long a (first seq-a)
          ^long b (first seq-b)
          matches (if (= (bit-and 0xffff a) (bit-and 0xffff b)) (inc matches) matches)]
      (recur (rest seq-a) (rest seq-b) matches (dec n)))
    matches))

(defn solve-2 []
  (let [[^long seed-a ^long seed-b] (read-input)
        seq-a2 (->> (iterate (mk-gen 16807 2147483647) seed-a)
                    (filter #(zero? (bit-and 3 %)))) ;; fast mod 4
        seq-b2 (->> (iterate (mk-gen 48271 2147483647) seed-b)
                    (filter #(zero? (bit-and 7 %))))] ;; fast mod 8
    
    (count-matches seq-a2 seq-b2 0 (* 5 1000 1000))))

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
;; trigger this solution, comment can be removed next time
