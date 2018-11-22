(ns aos.y2017.d02
  (:require
   [aos.utils :as u]
   [aos.y2017.input :refer [input-d02] :rename {input-d02 input}]
   [clojure.test :refer [deftest is testing]]
   [clojure.string :as str]))

;;;; Solution 001

(defn solution-001-01 []
  (transduce
   (comp
    (map #(str/split % #"\s"))
    (map #(map u/parse-int
               %))
    (map (fn [row]
           [(apply max row)
            (apply min row)]))
    (map (fn [[max min]]
           (- max min))))
   +
   (str/split-lines input)))

(defn find-divisibles [nums]
  (let [desc (sort-by - nums)
        asc  (sort nums)]
    (for [greater desc
          smaller asc
          :while (> greater smaller)
          :when (zero? (mod greater smaller))]
      [greater smaller])))

(defn solution-001-02 []
  (transduce
   (comp
    (map #(str/split % #"\s"))
    (map #(map u/parse-int
               %))
    (map (fn [row]
           (first (find-divisibles row))))
    (map (fn [[greater smaller]]
           (/ greater smaller))))
   +
   (str/split-lines input)))

;;;; Solution 002

;;;; Tests

(deftest aos-y2017-d02-01-test
  (is (= 44887 (solution-001-01))))

(deftest aos-y2017-d02-02-test
  (is (= 242 (solution-001-02))))
