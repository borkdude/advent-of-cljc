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

(defn closest
  "Among the points a & b, find the non-nil point
  closest to p."
  [p a b]
  (cond
    (nil? a) b
    (nil? b) a
    :else (let [da (manhattan-distance p a)
                db (manhattan-distance p b)]
            (cond
              (= da db) nil
              (> da db) b
              :else a))))

(defn min-max-by [pred coll]
  (when coll
    (let [pairs (map (fn [x] [x (pred x)]) coll)]
      (map first (reduce (fn [[min-pair max-pair] new-pair]
                          [(min-key second min-pair new-pair)
                           (max-key second max-pair new-pair)])
                         [(first pairs) (first pairs)]
                         (rest pairs))))))

#_(min-max-by first [[0 0] [-1 1] [2 -3]])

(min-max-by first (parse input))
(- 355 46)

(defn scan-x [x-start y-start points]
  (loop [x x-start
         y y-start
         nearest-right-points (sort-by #(manhattan-distance [x y] %) points)
         nearest-left-point nil
         closest-point-tally (zipmap (parse input) (repeat 0))]
    (comment (do
               (println "loop" x y)
               (println "right points" nearest-right-points)
               (println "rest" nearest-left-point closest-point-tally)))
    (if (empty? nearest-right-points)
      closest-point-tally
      (let [closest-point (closest [x y] nearest-left-point (first nearest-right-points))
            [left-points right-points] (split-with #(<= (first %) x) nearest-right-points)]
        (recur (inc x)
               y
               right-points
               (closest [x y] nearest-left-point (first left-points))
               (if closest-point (update closest-point-tally closest-point inc)
                                 closest-point-tally))))))

(defn on-bounding-box [x-bounds y-bounds p]
  (or (some #{(first p)} x-bounds)
      (some #{(second p)} y-bounds)))



#_(let [right '([84 60] [47 117] [116 49] [104 71] [107 123] [46 189] [100 151] [56 209] [227 47] [173 144] [236 88] [119 214] [201 139] [136 210] [222 125] [305 50] [196 163] [180 190] [312 70] [112 275] [110 280] [86 304] [91 300] [255 151] [261 146] [283 130] [144 273] [183 242] [313 124] [149 291] [297 187] [242 250] [203 289] [268 225] [235 265] [308 202] [200 312] [217 300] [174 356] [346 188] [300 249] [329 225] [354 201] [326 237] [243 328] [350 245] [318 293] [322 291] [289 331] [327 317])]
    (split-with #(<= (first %) 46) right))


; Find the largest non-infinite region by closest-manhattan distance.
(defn solve-1 []
  (let [points (parse input)
        x-bounds (map first (min-max-by first points))
        y-bounds (map second (min-max-by second points))]
    (->> (map #(scan-x (first x-bounds) % points)
              (range (first y-bounds) (inc (second y-bounds))))
         (apply merge-with +)
         (remove #(on-bounding-box x-bounds y-bounds (first %)))
         vals
         (apply max))))

; Find the number of points within 10000 manhattan distance of each point.
(defn solve-2 [])
  ;; TODO


(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
