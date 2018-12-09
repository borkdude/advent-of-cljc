(ns aoc.y2018.d03.akmiller78
  (:refer-clojure :exclude [read-string])
  (:require
   [aoc.utils :as u :refer [deftest read-string]]
   [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as str]))

(defn parse-int [s]
  (read-string (re-find #"\d+" s)))

(defn location-1d
  [width [x y]]
  (+ x (* width y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part One
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-claim-info
  []
  (->> input
       str/split-lines
       (map #(let [[claim-no x1 y1 x2 y2]
                   (->> %
                        (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
                        rest
                        (map parse-int))]
               {:claim claim-no
                :coords [[x1 y1] [(+ x1 x2) (+ y1 y2)]]}))))

(defn get-fabric
  [size]
  (vec (replicate size 0)))

(defn- mark-row
  [fabric start-idx end-idx]
  (reduce #(update %1 %2 inc) fabric (range start-idx end-idx)))

(defn- mark-fabric
  [fabric width [[x1 y1][x2 y2]]]
  (let [start-row y1
        end-row y2]
    (reduce
     (fn [f row]
       (let [start-idx (location-1d width [x1 row])
             end-idx (location-1d width [x2 row])]
         (mark-row f start-idx end-idx)))
     fabric
     (range start-row end-row))))

(defn get-claimed
  [board-width]
  (let [size (* board-width board-width)
        fabric (get-fabric size)
        claims (get-claim-info)]
    (reduce
     (fn [fabric claim]
       (mark-fabric fabric board-width (:coords claim)))
     fabric
     claims)))

(defn get-claimed-square-inches
  [board-width]
  (let [claimed (get-claimed board-width)]
    (->> claimed (filter #(> % 1)) count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part Two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- get-indices
  [v]
  (->> v (keep-indexed #(when (= 1 %2) %1)) set))

(defn- get-claim-indices
  [width claim]
  (let [[[x1 y1][x2 y2]] (:coords claim)
        start-row y1
        end-row y2]
    (reduce
     (fn [indices row]
       (let [start-idx (location-1d width [x1 row])
             end-idx (location-1d width [x2 row])]
         (apply conj indices (range start-idx end-idx))))
     #{}
     (range start-row end-row))))

(defn find-non-repeated-claims
  [board-width]
  (let [size (* board-width board-width)
        fabric (get-fabric size)
        claims (get-claim-info)
        single-claim-indices (get-indices
                              (get-claimed board-width))]
    (->> claims
         (keep (fn [c]
                 (let [indices (get-claim-indices board-width c)]
                   (when (clojure.set/subset? indices single-claim-indices)
                     c)))))))

(defn solve-1 []
  (get-claimed-square-inches 1000))

(defn solve-2 []
  (-> (find-non-repeated-claims 1000) first :claim str))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
