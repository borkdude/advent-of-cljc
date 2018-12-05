(ns aoc.y2018.d04.transducer
  (:refer-clojure :exclude [read-string format])
  (:require [aoc.utils :as utils :refer [deftest read-string format parse-int]]
            [aoc.y2018.d03.transducer :refer [inc-indices]]
            [aoc.y2018.d04.data :refer [input answer-1 answer-2]]
            [clojure.test :refer [is testing]]
            [clojure.string :as string]))

(defn parse [input]
  (->> (sort (string/split-lines input))
       (map #(zipmap
              [:YYYY :MM :DD :hh :mm :type :guard-number]
              (let [[timestamp-data type-data]
                    (split-at 5
                              (rest (re-find #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (\w+) .(\w+)" %)))]
                (concat (map parse-int timestamp-data) type-data))))
       (partition-by #(= (:type %) "Guard"))
       (partition 2)
       (map flatten)))

(def guard->sleep-mask
  (->> (parse input)
       (reduce
        (fn [acc [{:keys [guard-number]} & sleep-data]]
          (let [sleep-periods (partition 2 (keep :mm sleep-data))
                sleep-mask (reduce
                            (fn [mask [start stop]]
                              (inc-indices mask (range start stop)))
                            (vec (repeat 60 0)) ; "bit mask" for sleeping minutes in hour
                            sleep-periods)]
            (conj acc {(parse-int guard-number) sleep-mask})))
        [])
       (apply merge-with #(map + %1 %2))))

(defn solve-1 []
  (let [[guard-number sleep-mask]
        (first
         (into
          (sorted-map-by (fn [k1 k2]
                           (compare
                            (apply + (get guard->sleep-mask k2))
                            (apply + (get guard->sleep-mask k1)))))
          guard->sleep-mask))]
    (* guard-number (.indexOf sleep-mask (apply max sleep-mask)))))

(defn solve-2 []
  (let [[guard-number sleep-mask]
        (first
         (into
          (sorted-map-by (fn [k1 k2]
                           (compare
                            (apply max (get guard->sleep-mask k2))
                            (apply max (get guard->sleep-mask k1)))))
          guard->sleep-mask))]
    (* guard-number (.indexOf sleep-mask (apply max sleep-mask)))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
