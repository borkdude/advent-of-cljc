(ns aos.y2017.d02.mfikes
  (:require
   [aos.utils :as u]
   [aos.y2017.d02.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))


(def data (->> [input]
               (map #(str/split % #"\t"))
               (map #(map u/read-string %))))

(defn solve [f]
  (transduce
   (map f)
   +
   data))

(deftest part-1
  (is (number? (solve #(- (apply max %) (apply min %))))))

(defn divides? [x y]
  (and (not= 0 x y)
       (zero? (mod y x))))

(defn dividing-pairs [xs]
  (for [x1 xs
        x2 xs
        :when (and (distinct? x1 x2)
                   (divides? x1 x2))]
    [x1 x2]))

(defn first-integer-ratio [xs]
  (when-let [[x y] (first (dividing-pairs xs))]
    (/ y x)))

(deftest part-2
  (is (number? (solve first-integer-ratio))))
