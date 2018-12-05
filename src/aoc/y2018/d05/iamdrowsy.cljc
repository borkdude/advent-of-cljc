(ns aoc.y2018.d05.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as str]))

(defn reducible-set []
  (into #{} (map hash-set
                 (map char (range 97 123))
                 (map char (range 65 91)))))

(defn reducible?-fn []
  (let [r (reducible-set)]
       (memoize
         (fn [c1 c2]
           (r (hash-set c1 c2))))))

(defn react [input skipable?]
  (let [reducible? (reducible?-fn)]
    (loop [reduced (take 1 input)
           [current & remaining] (rest input)]
      (cond (nil? current) reduced
            (skipable? current) (recur reduced remaining)
            (reducible? (first reduced) current) (recur (rest reduced) remaining)
            :else (recur (cons current reduced) remaining)))))

(defn solve-1 []
  (count (react (str/trim input) #{})))

(defn solve-2 []
  (apply min (map (comp count (partial react (react (str/trim input) #{})))
                  (reducible-set))))


(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
