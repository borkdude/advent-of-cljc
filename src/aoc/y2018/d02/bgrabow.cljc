(ns aoc.y2018.d02.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(defn first-duplicate [coll]
  (loop [seen-values #{}
         remaining-values coll]
    (cond
      (empty? remaining-values) nil
      (seen-values (first remaining-values)) (first remaining-values)
      :else (recur (conj seen-values (first remaining-values)) (rest remaining-values)))))

(defn differences [x y]
  (filter #(apply distinct? %) (map list x y)))

(defn extract-common-chars
  [pair]
  (->> pair
       (apply map list)
       (filter #(apply = %))
       (map first)
       (apply str)))

(defn find-almost-duplicates [ids]
  (for [lhs ids
        rhs ids
        :when (distinct? lhs rhs)]
    (when (-> (differences lhs rhs)
              count
              (= 1))
      [lhs rhs])))

(defn in-twain [s]
  (let [mid (quot (count s) 2)]
    [(subs s 0 mid)
     (subs s mid)]))

(defn group-by-first-and-second-halves [coll]
  (concat (vals (group-by #(first (in-twain %)) coll))
          (vals (group-by #(second (in-twain %)) coll))))

(defn solve-1 []
  ;; Part 1 - Compute a checksum of (* d t) where
  ;; d is the number of IDs with at least one char
  ;; that appears exactly twice, and
  ;; t is the number of IDs with at least one char
  ;; that appears exactly thrice
  (let [char-freqs (map frequencies (str/split-lines input))
        doubles (count (filter #(some #{2} (vals %)) char-freqs))
        triples (count (filter #(some #{3} (vals %)) char-freqs))
        checksum (* doubles triples)]
    checksum))

(defn solve-2 []
  ;; Part 2 - Find a pair of IDs that differ by exactly one char.
  ;; The answer is the list of chars common to the above pair.

  ;; Hybrid approach. Group the words by their first half and group them
  ;; by their second half. Perform a brute force O(n^2) search for
  ;; almost-duplicates in each group. Take the first pair of almost-
  ;; duplicates found and extract the common characters.
  ;;
  ;; Rationale: A pair of almost-duplicates will have their differing
  ;; character in either the first half or the second half, meaning
  ;; either their second half or their first half will be identical.
  ;; Grouping by first half and by second half guarantees that the
  ;; pair will end up in a group together. The grouping step also
  ;; drastically cuts down the number of pairings we need to consider.
  ;; Searching within the group is O(n^2)
  (->> (str/split-lines input)
       group-by-first-and-second-halves
       (map find-almost-duplicates)
       (some first)
       extract-common-chars))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
