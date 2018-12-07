(ns aoc.y2018.d06.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d06.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(def parse
  (memoize (fn [input]
             (map
               #(mapv u/parse-int (re-seq #"\d+" %))
               (str/split-lines input)))))

(defn manhattan-distance [[px py] [qx qy]]
  ; TODO add type hints
  (+ (Math/abs (- px qx))
     (Math/abs (- py qy))))

(defn min-max-by [pred coll]
  (when coll
    (let [pairs (map (fn [x] [x (pred x)]) coll)]
      (map first (reduce (fn [[min-pair max-pair] new-pair]
                          [(min-key second min-pair new-pair)
                           (max-key second max-pair new-pair)])
                         [(first pairs) (first pairs)]
                         (rest pairs))))))

(defn left-of [x p]
  (< (first p) x))

(defn scan-x [x-start y ps]
  (let [sorted-ps (sort-by #(manhattan-distance [x-start y] %) ps)]
    (loop [x x-start
           approaching-ps sorted-ps
           closest-ps '()
           acc (zipmap ps (repeat []))]
      #_(do
          (println "x y" x y)
          (println "approaching" (take 5 approaching-ps))
          (println "closest" closest-ps)
          (println "acc" acc))
      (if (empty? approaching-ps)
        acc
        (let [m-dist #(manhattan-distance [x y] %)
              new-closest-ps (->> approaching-ps
                                  (partition-by m-dist)
                                  first
                                  (concat closest-ps)
                                  (sort-by m-dist)
                                  (partition-by m-dist)
                                  first
                                  dedupe)]
          #_(do
              (println "new-closest" new-closest-ps))
          (recur (inc x)
                 (drop-while #(left-of (inc x) %) approaching-ps)
                 new-closest-ps
                 (if (second new-closest-ps) acc (update acc (first new-closest-ps) #(conj % [x y])))))))))


(defn on-bounding-box [x-bounds y-bounds p]
  (or (some #{(first p)} x-bounds)
      (some #{(second p)} y-bounds)))

(defn bounding-box [[x-min x-max] [y-min y-max]]
  (for [x (range x-min (inc x-max))
        y (range y-min (inc y-max))
        :when (on-bounding-box [x-min x-max] [y-min y-max] [x y])]
    [x y]))

(defn infinite-region? [x-bounds y-bounds [_ territory]]
  (let [group-x-vals (into #{} (map first territory))
        group-y-vals (into #{} (map second territory))]
    (or (some group-x-vals (into #{} x-bounds))
        (some group-y-vals (into #{} y-bounds)))))

; Find the largest non-infinite region by closest-manhattan distance.
(defn solve-1 []
  (let [points (parse input)
        x-bounds (map first (min-max-by first points))
        y-bounds (map second (min-max-by second points))
        closest-point-map (map #(scan-x (first x-bounds) % points)
                               (range (first y-bounds)
                                      (inc (second y-bounds))))]
    (->> closest-point-map
         (apply merge-with concat)
         (remove #(infinite-region? x-bounds y-bounds %))
         (apply max-key (comp count second))
         second
         count)))
;
;(def points (parse input))
;(def x-bounds (map first (min-max-by first points)))
;(def y-bounds (map second (min-max-by second points)))
;(scan-x (first x-bounds) (first y-bounds) points)
;(def x (first x-bounds))
;(def y (first y-bounds))
;(def sorted-ps (sort-by #(manhattan-distance [(first x-bounds) (first y-bounds)] %) points))
;(def m-dist #(manhattan-distance [(first x-bounds) (first y-bounds)] %))
;(m-dist (first sorted-ps))
;(map m-dist sorted-ps)

;(def bbox (into #{} (bounding-box x-bounds y-bounds)))
;(def closest-point-map (filter (comp identity second) (apply merge (map #(scan-x (first x-bounds) % points)))))
;                                                                        (range (first y-bounds) (second y-bounds))))))
;(def points-with-infinite-region (into #{} (map second (filter #(bbox (first %)) closest-point-map))))
;(def remaining-map (remove #(points-with-infinite-region (first %)) closest-point-map))
;(def freqs (frequencies (map second remaining-map)))
;(def answer (apply max-key second freqs))

#_(def points-with-infinite-region (into #{} (map closest-point-map (bounding-box x-bounds y-bounds))))


; Find the number of points within 10000 manhattan distance of each point.

(let [points (parse input)
      x-bounds (map first (min-max-by first points))
      y-bounds (map second (min-max-by second points))]
  )


(defn solve-2 []
  (let [points (parse input)
        x-bounds (map first (min-max-by first points))
        y-bounds (map second (min-max-by second points))]
    (->> (for [x (range (first x-bounds) (inc (second x-bounds)))
               y (range (first y-bounds) (inc (second y-bounds)))]
           (reduce + (map #(manhattan-distance [x y] %) points)))
         (filter #(< % 10000))
         count)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
