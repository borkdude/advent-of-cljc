(ns aoc.y2017.d06.mfikes
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d06.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(def data (as-> input x (str/trim x) (str/split x #"\t") (mapv read-string x)))

(defn redistribute [banks]
  (let [max-val     (apply max (vals banks))
        max-ndx     (apply min (keep (fn [[k v]] (when (= max-val v) k)) banks))
        target-ndxs (map #(mod (+ max-ndx 1 %) (count banks))
                      (range (banks max-ndx)))]
    (merge-with + (assoc banks max-ndx 0) (frequencies target-ndxs))))

(defn solve [banks]
  (reduce (fn [[last-seen banks] steps]
            (if (last-seen banks)
              (reduced [steps (last-seen banks)])
              [(assoc last-seen banks steps) (redistribute banks)]))
    [{} (zipmap (range) banks)]
    (range)))

(defn solve-1 []
  (first (solve data)))

(defn solve-2 []
  (apply - (solve data)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
