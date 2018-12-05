(ns aoc.y2018.d05.mfikes
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
   [clojure.string :as string]
   [clojure.test :refer [is testing]]))

(defn fixed-point-by [g f x]
  (reduce #(if (= (g %1) (g %2))
             (reduced %1)
             %2)
    (iterate f x)))

(defn lower-case [unit]
  #?(:clj  (Character/toLowerCase ^Character unit)
     :cljs (string/lower-case unit)))

(defn reacts? [x y]
  (and (not= x y)
       (= (lower-case x) (lower-case y))))

(defn react [polymer]
  (lazy-seq
    (when (seq polymer)
      (let [[x y & more] polymer]
        (cond
          (nil? y)      [x]
          (reacts? x y) (react more)
          :else         (cons x (react (rest polymer))))))))

(defn fully-react [polymer]
  (fixed-point-by count react polymer))

(defn fully-react-conquer [n polymer]
  (if (< n 512)
    (fully-react polymer)
    (fully-react-conquer (quot n 2)
      (mapcat (partial fully-react-conquer (quot n 2)) (partition-all n polymer)))))

(defn solve-1 []
  (count (fully-react (fully-react-conquer 4096 input))))

(defn remove-units [x polymer]
  (remove (fn [y]
            (or (= x y)
                (reacts? x y)))
    polymer))

(defn solve-2 []
  (->> (into #{} (map lower-case input))
    (map #(count (fully-react (fully-react-conquer 4096 (remove-units % input)))))
    (apply min)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
