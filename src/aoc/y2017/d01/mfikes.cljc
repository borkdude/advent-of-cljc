(ns aoc.y2017.d01.mfikes
  (:require
   [aoc.y2017.d01.data :refer [input answer-1 answer-2]]
   [aoc.utils :as u :refer [deftest]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(let [c->d (zipmap "0123456789" (range))]
  (defn str->digits
    [s]
    (map c->d s)))

(def data (-> input str/trim str->digits))

(defn matches [xs ys]
  (->>
   (map vector xs ys)
   (filter (partial apply =))
   (map first)))

(defn solve [pair-up]
  (apply + (matches data (pair-up data))))

(deftest part-1
  (is (= answer-1 (solve #(rest (cycle %))))))

(deftest part-2
  (is (= answer-2 (solve #(nthrest (cycle %) (/ (count %) 2))))))
