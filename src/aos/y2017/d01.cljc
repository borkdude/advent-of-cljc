(ns aos.y2017.d01
  (:require
   [aos.y2017.input :refer [input-d01] :rename {input-d01 input}]
   [aos.utils :as u]
   [clojure.test :refer [deftest is testing]]
   [clojure.string :as str]))

;;;; Solution 001

(defn solution-001-01 []
  (reduce
   +
   (map
    (fn [a b]
      (if (= a b)
        (u/parse-int (str a)) 0))
    input
    (drop 1 (cycle input)))))

(defn solution-001-02 []
  (let [half (/ (count input) 2)]
    (reduce
     +
     (map
      (fn [a b]
        (if (= a b)
          (u/parse-int (str a)) 0))
      input
      (drop half (cycle input))))))

;;;; Solution 002

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

(defn solution-002-01 []
  (solve #(rest (cycle %))))

(defn solution-002-02 []
  (solve #(nthrest (cycle %) (/ (count %) 2))))

;;;; Tests

(deftest aos-y2017-d01-01-test
  (is (= 995 (solution-001-01)))
  (is (= 995 (solution-002-01))))

(deftest aos-y2017-d01-02-test
  (is (= 1130 (solution-001-02)))
  (is (= 1130 (solution-002-02))))
