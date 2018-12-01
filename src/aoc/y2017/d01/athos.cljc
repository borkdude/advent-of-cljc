(ns aoc.y2017.d01.athos
  (:require
   [aoc.y2017.d01.data :refer [input answer-1 answer-2]]
   [aoc.utils :as u :refer [deftest]]
   [clojure.test :refer [is testing]]))

(defn char-code [char]
  #?(:clj (int char)
     :cljs (.charCodeAt char 0)))

(defn- cs->nums [cs]
  (map #(- (char-code %) (char-code \0)) cs))

(defn solve1 [cs]
  (let [ns (cs->nums cs)
        c (first ns)]
    (->> (concat ns [c])
         (partition 2 1)
         (filter (fn [[x y]] (= x y)))
         (map first)
         (apply +))))

(defn solve2 [cs]
  (let [len (count cs)
        ns (cycle (cs->nums cs))]
    (->> (map list ns (drop (quot len 2) ns))
         (take len)
         (filter (fn [[x y]] (= x y)))
         (map first)
         (apply +))))

(deftest part-1
  (is (= answer-1 (solve1 input))))

(deftest part-2
  (is (= answer-2 (solve2 input))))
