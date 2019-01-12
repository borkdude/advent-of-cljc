(ns aoc.y2017.d13.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d13.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn caught? [[depth range]]
  (zero? (mod depth (* 2 (dec range)))))

(defn severity [xs]
  (->> xs
      (filter caught?)
      (map (fn [[a b]] (* a b)))
      (reduce +)))

(defn read-input []
  (->> input
       s/split-lines
       (mapv #(mapv read-string (re-seq #"\d+" %)))))

(defn solve-1 []
  (severity (read-input)))

(defn caught2? [t [depth range]]
  (zero? (mod (+ t depth) (* 2 (dec range)))))

(defn undetected? [xs t]
  (->> xs
       (filter (partial caught2? t))
       empty?))

(defn solve-2 []
  (let [xs (read-input)]
    (->> (filter (partial undetected? xs) (range))
         first)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
)
