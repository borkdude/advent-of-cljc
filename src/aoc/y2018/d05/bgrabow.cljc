(ns aoc.y2018.d05.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(def char-code #?(:clj  (comp int char first str)
                  :cljs #(.charCodeAt % 0)))

(def my-pmap #?(:clj pmap :cljs map))

(def numeric-input
  (memoize #(map char-code (seq %))))

(defn numeric-anti-pair?
  "Returns true if the two args are case-complements
  of the same letter, false otherwise (e.g. if they
  are identical case of the same letter, if they are
  different letter, or if either of the args elements
  are nil."
  [x y]
  (let [diff (- x y)]
    (or (== 32 diff)
        (== -32 diff))))

(defn purge-anti-pairs
  [coll]
  (reduce (fn [left e]
            (if (and (seq left)
                     (numeric-anti-pair? (first left) e))
              (rest left)
              (cons e left)))
          '()
          coll))

(def a 97)
(def z 122)
(def A 65)
(def Z 90)

(defn char-sets []
  (map (fn [l u] #{l u})
       (range a (inc z))
       (range A (inc Z))))

(defn solve-1 []
  (count (purge-anti-pairs (numeric-input input))))

(defn solve-2 []
  (let [partially-purged (purge-anti-pairs (numeric-input input))]
    (->> (my-pmap (fn [cs]
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
