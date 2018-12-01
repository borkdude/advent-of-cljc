(ns aoc.y2018.d01.orestis
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(def data (->> input
              (str/split-lines)
              (map u/parse-int)))

(defn solve-1 []
  (reduce + data))

(defn solve-2 []
  (loop [freq 0
         seen (transient #{0})
         changes (cycle data)]
    (let [nfreq (+ freq (first changes))]
      (if (seen nfreq)
        nfreq
        (recur nfreq
               (conj! seen nfreq)
               (rest changes))))))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
