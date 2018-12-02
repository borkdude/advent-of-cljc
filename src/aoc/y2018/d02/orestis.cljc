(ns aoc.y2018.d02.orestis
  (:require
   [aoc.utils :as u :refer [deftest]]
   [clojure.string :as str]
   [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

(defn solve-1 []
  (let [freqs (->> input
                   str/split-lines
                   (map frequencies)
                   (map vals)
                   (mapcat distinct)
                   (filter #{2 3})
                   (frequencies)
                   (vals))]
    (apply * freqs)))


(defn diff [i1 i2]
  (let [d (map - i1 i2)
        diffs (remove #{0} d)]
    (when (= 1 (count diffs))
      (let [dd (keep identity (map #(if (= 0 %2) %1 nil) i1 d))]
        (apply str (map char dd))))))

(defn char-to-int [c]
  #?(:clj (int c)
     :cljs (.charCodeAt c 0)))

(defn solve-2 []
  (loop [candidates (mapv #(mapv char-to-int %) (str/split-lines input))]
    (let [current (peek candidates)
          to-check (pop candidates)
          found (keep #(diff current %) to-check)]
      (if (seq found)
        (first found)
        (recur to-check)))))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
