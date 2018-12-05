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
   (when (seq coll)
     (let [v (into [] (reverse coll))]
       (purge-anti-pairs [(peek v)]
                         (pop v)))))
  ([left right]
   (if (peek right)
     (if (anti-pair? (peek left) (peek right))
       (recur (pop left) (pop right))
       (recur (conj left (peek right))
              (pop right)))
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
