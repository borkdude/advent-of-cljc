(ns aoc.y2018.d01.adamruzicka
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(def data (map u/read-string (str/split-lines input)))

(defn solve-1 []
  (reduce + 0 data))

(defn solve-2 []
  (let [f (fn [[freq seen] cur]
            (let [new (+ freq cur)]
              (if (seen new)
                (reduced [new seen])
                [new (conj seen new)])))]
    (first (reduce f [0 #{}] (cycle data)))))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
