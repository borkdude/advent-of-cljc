(ns aoc.y2018.d03.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :refer [is testing]]))

(defn points-for [claim]
  (let [nums (->> claim
                  (re-seq #"#(.+) @ (.+),(.+): (.+)x(.+)")
                  first
                  rest)
        [claim-id left top width height] (map read-string nums)]
    (for [x (range left (+ left width))
          y (range top (+ top height))]
      [x y])))

(defn points-and-claims-for [claim]
  (let [nums (->> claim
                  (re-seq #"#(.+) @ (.+),(.+): (.+)x(.+)")
                  first
                  rest)
        [claim-id left top width height] (map read-string nums)]
    (for [x (range left (+ left width))
          y (range top (+ top height))]
      {[x y] [claim-id]})))

(defn claims-for [claim]
  (->> claim
       (re-seq #"#(.+) @ .*")
       first
       rest
       first
       read-string))

(defn overlapping-claims [input]
  (->> input
       (mapcat points-and-claims-for)
       (apply merge-with into)
       (map second)
       (filter #(> (count %) 1))
       (reduce into #{})))

;; (overlapping-claims example-input)

(defn solve-1 []
  (->> input
       s/split-lines
       (mapcat points-for)
       frequencies
       vals
       (filter #(< 1 %))
       count))

(defn solve-2 []
  (let [input      (s/split-lines input)
        all-claims (set (map claims-for input))
        ovr-claims (overlapping-claims input)]
    (first
     (for [claim all-claims
           :when (not (ovr-claims claim))]
       claim))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
