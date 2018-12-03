(ns aoc.y2018.d03.iamdrowsy
  (:require
    [aoc.utils :as u :refer [deftest]]
    [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [com.rpl.specter :as s]
    [clojure.string :as str]
    [clojure.set :as set]))

(def line-re
  #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

(defn parse-line [s]
  (let [parts (rest (re-matches line-re s))]
    (zipmap [:id :x :y :x-size :y-size]
            (map u/parse-int parts))))

(defn parsed-input []
  (map parse-line (str/split-lines input)))

(defn box->used-fields [{:keys [id x y x-size y-size :as box]}]
  (for [i (range x (+ x x-size))
        j (range y (+ y y-size))]
    {:id id
     :coords [i j]}))

(def prepared-coords
  (memoize #(->> (parsed-input)
                 (mapcat box->used-fields)
                 (group-by :coords))))

(def OVERLAPPING-KOORDS
  (s/path s/MAP-VALS (s/selected? (s/view count) (s/pred> 1))))

(defn solve-1 []
  (->> (prepared-coords)
       (s/select OVERLAPPING-KOORDS)
       count))

(defn solve-2 []
  (str (first (drop-while
                (into #{0} (s/select [OVERLAPPING-KOORDS s/ALL :id] (prepared-coords)))
                (range)))))


(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
