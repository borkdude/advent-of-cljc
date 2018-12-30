(ns aoc.y2018.d08.transducer
  (:refer-clojure :exclude [read-string format])
  (:require [aoc.utils :refer [deftest read-string format]]
            [aoc.y2018.d08.data :refer [input answer-1 answer-2]]
            [clojure.test :refer [is testing]]
            [clojure.zip :as z]))

(def data
  (->> input (format "[%s]") read-string delay))

(defn root
  "Zips all the way up and returns the zipper at the root."
  [loc]
  (->> (iterate z/up loc) (take-while (complement nil?)) last))

(def tree-with-zipper
  (delay
   (loop [loc              (z/vector-zip [[]])
          [x & xs :as all] @data]
     (if (seq all)
       (let [next-loc                              (z/next loc)
             {:keys [meta-data-cnt] :as next-node} (z/node next-loc)]
         (cond (z/branch? next-loc)
               (recur (-> (iterate #(z/insert-child % []) next-loc)
                          (nth x)
                          (z/insert-child :add-meta-data-cnt))
                      xs)

               (= :add-meta-data-cnt next-node)
               (recur (-> next-loc z/remove (z/append-child {:meta-data-cnt x})) xs)

               (and meta-data-cnt (> meta-data-cnt 0))
               (recur (-> next-loc
                          (z/edit assoc :meta-data-cnt (dec meta-data-cnt))
                          (z/edit update :meta-data conj x)
                          z/prev)
                      xs)

               :else
               (recur next-loc all)))
       (root loc)))))

(defn solve-1 []
  (->> @tree-with-zipper
       z/root
       flatten
       (mapcat :meta-data)
       (apply +)))


(defn count-children
  "Gets children count of loc. Not counting meta data map as a child."
  [loc]
  (try (-> loc z/children count dec)
       (catch #?(:clj Exception :cljs :default) _ 0)))

(defn values-of-loc [loc]
  (if-not loc
    0
    (let [meta-data (-> loc z/node last :meta-data)]
      (if (zero? (count-children loc))
        (apply + meta-data)
        (for [i meta-data]
          (let [child (nth (iterate z/right (z/next loc)) (dec i))]
            (values-of-loc child)))))))

(defn solve-2 []
  (->> @tree-with-zipper
       z/next
       values-of-loc
       flatten
       (apply +)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
