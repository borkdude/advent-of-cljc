(ns aoc.y2018.d02.transducer
  (:require [aoc.utils :as utils :refer [deftest]]
            [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
            [clojure.string :as string]
            [clojure.test :refer [is testing]]))

(def data
  (string/split-lines input))

(defn solve-1 []
  (->> (map frequencies data)
       (reduce (fn [[twos threes] e]
                 (let [freqs (vals e)]
                   [((if (some #{2} freqs) inc identity) twos)
                    ((if (some #{3} freqs) inc identity) threes)]))
               [0 0])
       (apply *)))

(defn solve-2 []
  (letfn [(same-part [word1 word2]
            (->> (map (fn [c1 c2] (when (= c1 c2) c1)) word1 word2)
                 (apply str)))]
    (-> (for [word1 data
              word2 data
              :when (= (count (same-part word1 word2))
                       (dec (count word1)))]
          (same-part word1 word2))
        first)))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
