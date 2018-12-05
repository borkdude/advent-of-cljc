(ns aoc.y2018.d05.mfikes
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
   [clojure.string :as string]
   [clojure.test :refer [is testing]]))

(defn lower-case [unit]
  #?(:clj  (Character/toLowerCase ^Character unit)
     :cljs (string/lower-case unit)))

(defn reacts? [x y]
  (and (not= x y)
       (= (lower-case x) (lower-case y))))

(defn add-unit [polymer unit]
  (if (some-> (peek polymer) (reacts? unit))
    (pop polymer)
    (conj polymer unit)))

(defn react [polymer]
  (reduce add-unit [] polymer))

(defn solve-1 []
  (count (react input)))

(defn remove-units [x polymer]
  (remove (fn [y]
            (or (= x y)
                (reacts? x y)))
    polymer))

(defn solve-2 []
  (->> (into #{} (map lower-case input))
    (map #(count (react (remove-units % input))))
    (apply min)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
