(ns aoc.y2018.d03.dandorman
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(defn parse-claim [line]
  (let [[_ id & dims] (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$" line)
        [left top width height] (map u/parse-int dims)]
    {:id id, :left left, :top top, :width width, :height height}))

(defn claim->map [{:keys [left top width height]}]
  (into {}
        (for [x (range left (+ left width))
              y (range top (+ top height))]
          [[x y] 1])))

(defn merge-claims [input]
  (->> (str/split-lines input)
       (map parse-claim)
       (map claim->map)
       (reduce (fn [m c] (merge-with + m c)) {})))

(defn solve-1 [input]
  (->> input
       merge-claims
       vals
       (filter (partial < 1))
       count))

(defn claim-coords [{:keys [left top width height]}]
  (into #{}
        (for [x (range left (+ left width))
              y (range top (+ top height))]
          [x y])))

(defn index-claims [input]
  (->> input
       str/split-lines
       (map parse-claim)
       (reduce (fn [m c] (assoc! m (:id c) (claim-coords c))) (transient {}))
       persistent!))

(defn index-claims-by-coords [claims]
  (reduce-kv (fn [m id coords]
               (reduce (fn [m coord]
                         (update m coord conj id))
                       m
                       coords))
             {}
             claims))

(defn remove-overlaps [coord->claims]
  (->> coord->claims
       (remove (fn [[_ claims]] (< 1 (count claims))))
       (map first)
       (into #{})))

(defn solve-2 [input]
  (let [claims (index-claims input)
        single-owners (-> claims index-claims-by-coords remove-overlaps)]
    (->> claims
         (filter (fn [[_ coords]] (= coords (set/intersection coords single-owners))))
         ffirst)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1 input)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2 input)))))
