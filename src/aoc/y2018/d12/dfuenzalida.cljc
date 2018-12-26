(ns aoc.y2018.d12.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d12.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn parse-input []
  (let [lines (s/split-lines input)
        state (->> lines first (re-seq #"[#.]+") first)
        rules (into {} (map #(vec (nfirst (re-seq #"(.+) => (.+)" %))) (drop 2 lines)))]
    [state rules]))

(defn simulate [offset state rules iters]
  (if (pos? iters)
    (let [state  (str ".." state "...")
          pieces (map #(apply str %) (partition 5 1 state))]
      (recur offset
             (apply str (map #(get rules % ".") pieces))
             rules
             (dec iters)))
    state))

(defn compute-sum [iters]
  (let [offset        -20
        padding       (apply str (repeat (Math/abs offset) \.))
        [state rules] (parse-input)
        state         (str padding state padding)
        state         (simulate 0 state rules iters)]
    (->> (map vector (range offset (count state)) state)
         (filter (fn [[i c]] (= (str c) "#")))
         (map first)
         (reduce +))))

(defn solve-1 []
  (compute-sum 20))

(defn solve-2 []
  (let [sum200 (compute-sum 200)
        sum300 (compute-sum 300)
        diff   (- sum300 sum200)]
    (-> 50000000000
        (- 200)
        (/ 100)
        (* diff)
        (+ sum200))))

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
