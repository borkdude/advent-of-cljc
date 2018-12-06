(ns aoc.y2018.d05.borkdude
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

(defn char->int [c]
  #?(:clj (int c)
     :cljs (.charCodeAt c 0)))

(defn reacting? [c1 c2]
  (= 32 (Math/abs (- (char->int c1)
                     (char->int c2)))))

(defn react!
  ([chars] (react! [] chars))
  ([acc [c1 c2 & chars]]
   (cond (not c1) acc
         (not c2) (conj acc c1)
         (reacting? c1 c2) (if (seq acc)
                             (recur (pop acc)
                                    (cons (peek acc) chars))
                             (recur acc chars))
         :else (recur (conj acc c1) (cons c2 chars)))))

(defn solve-1 []
  (count (react! input)))

(defn to-upper [c]
  (char (- (char->int c) 32)))

(defn solve-2 []
  (apply min
         (map #(count (react!
                       (remove (hash-set % (to-upper %)) input)))
              "abcdefghijklmnopqrstuvwxyz")))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
