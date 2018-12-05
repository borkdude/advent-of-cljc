(ns aoc.y2018.d05.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(defn anti-pair?
  "Returns true if the two args are case-complements
  of the same letter, false otherwise (e.g. if they
  are identical case of the same letter, if they are
  different letter, or if either of the args elements
  are nil."
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

(def char-code #?(:clj (comp int char first)
                  :cljs #(.charCodeAt % 0)))

(def my-char #?(:clj char
                :cljs #(.fromCharCode js/String %)))

(def my-pmap #?(:clj pmap :cljs map))

(defn char-sets []
  (map (fn [l u] #{l u})
    (map my-char (range (char-code "a") (inc (char-code "z"))))
    (map my-char (range (char-code "A") (inc (char-code "Z"))))))

(defn solve-1 []
  (count (purge-anti-pairs input)))

(defn solve-2 []
  (let [partially-purged (purge-anti-pairs input)]
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
