(ns aoc.y2018.d05.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(defn anti-pair?
  "Returns true if first and second element of pair
  are case-complements of the same character, false
  otherwise (e.g. if they are identical case of the
  same character, if they are different characters,
  or if either of the first or second elements are
  nil."
  ([pair]
   (anti-pair? (first pair) (second pair)))
  ([x y]
   (and (distinct? x y)
        (= (str/lower-case (str x))
           (str/lower-case (str y))))))

(defn purge-anti-pairs
  ([coll]
   (purge-anti-pairs (take 1 coll)
                     (drop 1 coll)))
  ([left right]
   (if (seq right)
     (if (anti-pair? (first left) (first right))
       (recur (rest left) (rest right))
       (recur (cons (first right) left)
              (rest right)))
     (reverse left))))

(defn char-sets []
  (map (fn [l u] #{l u})
    (map char (range (int \a) (inc (int \z))))
    (map char (range (int \A) (inc (int \Z))))))

(defn solve-1 []
  (count (purge-anti-pairs input)))

(defn solve-2 []
  (let [partially-purged (purge-anti-pairs input)]
    (->> (pmap (fn [cs]
                 (->> partially-purged
                      (remove cs)
                      purge-anti-pairs
                      count))
               (char-sets))
         (apply min))))

(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest part-2
         (is (= (str answer-2)
                (str (solve-2)))))
