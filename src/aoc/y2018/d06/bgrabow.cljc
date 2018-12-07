(ns aoc.y2018.d06.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d06.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]
    [clojure.set :as set]))

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
  "Find the list of points that are non-nil and
  have the closest manhattan distance to p."
  [p a b]
  (cond
    (some nil? [a b]) (remove nil? (list a b))
    :else (let [da (manhattan-distance p a)
                db (manhattan-distance p b)]
            (cond
              (= da db) (list a b)
              (> da db) (list b)
              :else (list a)))))

(defn min-max-by [pred coll]
  (when coll
    (let [pairs (map (fn [x] [x (pred x)]) coll)]
      (map first (reduce (fn [[min-pair max-pair] new-pair]
                          [(min-key second min-pair new-pair)
                           (max-key second max-pair new-pair)])
                         [(first pairs) (first pairs)]
                         (rest pairs))))))

#_(min-max-by first [[0 0] [-1 1] [2 -3]])

#_(min-max-by first (parse input))
#_(- 355 46)

(defn gather-closest
  "p is the target point
  prev-closest is a list of points that must all have the same distance to p
  sorted-new-candidates - a list of points sorted by their distance to p

  Return the list of points that are tied for closest to p."
  [p prev-closest sorted-new-candidates]
  (if (empty? prev-closest)
    (first (partition-by
             #(manhattan-distance p %)
             sorted-new-candidates))
    (let [closest-distance (manhattan-distance p (first prev-closest))
          next-distance (manhattan-distance p (first sorted-new-candidates))]
      #_(println "dist" closest-distance next-distance)
      (cond
        (< closest-distance next-distance) prev-closest
        (= closest-distance next-distance) (concat prev-closest
                                                   (first (partition-by
                                                            #(manhattan-distance p %)
                                                            sorted-new-candidates)))
        :else (first (partition-by
                       #(manhattan-distance p %)
                       sorted-new-candidates))))))

(defn left-of [x p]
  (< (first p) x))

(def max-distance 1000)

(defn scan-x [x-start y points]
  (let [sorted-points (sort-by #(manhattan-distance [x-start y] %) points)]
    (loop [x x-start
           nearby-receding-points '()
           receding-dist max-distance
           approaching-points sorted-points
           approaching-dist (manhattan-distance [x y] (first approaching-points))
           prev-closest-point nil
           closest-point-map {}]
      (comment (do
                 (println "x y" x y)
                 (println "approaching points" (take 3 approaching-points))
                 (println "rest" nearby-receding-points)
                 (println "rec-dist app-dist" receding-dist approaching-dist)))
      (if (empty? approaching-points)
        closest-point-map
        (cond (< receding-dist approaching-dist)
              ; TODO - Can we fast-forward here (quot (- receding-dist approaching-dist) 2) spaces?
              (let [new-approaching-points (drop-while #(left-of (inc x) %) approaching-points)
                    new-approaching-dist (if (= new-approaching-points approaching-points)
                                           (dec approaching-dist)
                                           (if (seq new-approaching-points)
                                             (dec (manhattan-distance [x y] (first new-approaching-points)))
                                             max-distance))]
                (recur (inc x)
                       nearby-receding-points
                       (inc receding-dist)
                       new-approaching-points
                       new-approaching-dist
                       prev-closest-point
                       (assoc closest-point-map [x y] prev-closest-point)))
              (> receding-dist approaching-dist)
              (let [;_ (println "test here 0")
                    [new-receding-points new-approaching-points]
                    (split-with #(left-of (inc x) %) approaching-points)
                    new-nearby-receding-points (filter #(= (manhattan-distance [x y] %)
                                                           approaching-dist)
                                                       new-receding-points)
                    new-receding-point-dist (inc approaching-dist)
                    ;_ (println "test here 1")
                    new-approaching-dist (if (= new-approaching-points approaching-points)
                                           (dec approaching-dist)
                                           (if (seq new-approaching-points)
                                             (dec (manhattan-distance
                                                    [x y] (first new-approaching-points)))
                                             max-distance))
                    closest-points (take-while #(= (manhattan-distance [x y] %) approaching-dist)
                                               approaching-points)
                    current-closest-point (if (second closest-points) nil (first closest-points))]
                ;(println "test here")
                (recur (inc x)
                       new-nearby-receding-points
                       new-receding-point-dist
                       new-approaching-points
                       new-approaching-dist
                       current-closest-point
                       (assoc closest-point-map [x y] current-closest-point)))
              :else ; (= receding-dist approaching-dist)
              (let [[under right] (split-with #(left-of (inc x) %) approaching-points)
                    new-nearby-receding-points (if (seq under)
                                                 (concat nearby-receding-points
                                                         (take-while #(= (manhattan-distance [x y] %)
                                                                         approaching-dist)
                                                                     under))
                                                 '())
                    new-receding-dist (inc receding-dist)
                    new-approaching-points right
                    new-approaching-dist (if (= new-approaching-points approaching-points)
                                           (dec approaching-dist)
                                           (if (seq new-approaching-points)
                                             (dec (manhattan-distance
                                                    [x y] (first new-approaching-points)))
                                             max-distance))
                    closest-points (concat new-nearby-receding-points
                                           (take-while #(= (manhattan-distance [x y] %)
                                                           approaching-dist)
                                                       new-approaching-points))
                    current-closest-point (if (second closest-points) nil (first closest-points))]
                (recur (inc x)
                       new-nearby-receding-points
                       new-receding-dist
                       new-approaching-points
                       new-approaching-dist
                       current-closest-point
                       (assoc closest-point-map [x y] current-closest-point))))))))








(defn on-bounding-box [x-bounds y-bounds p]
  (or (some #{(first p)} x-bounds)
      (some #{(second p)} y-bounds)))

#_(let [right '([84 60] [47 117] [116 49] [104 71] [107 123] [46 189] [100 151] [56 209] [227 47] [173 144] [236 88] [119 214] [201 139] [136 210] [222 125] [305 50] [196 163] [180 190] [312 70] [112 275] [110 280] [86 304] [91 300] [255 151] [261 146] [283 130] [144 273] [183 242] [313 124] [149 291] [297 187] [242 250] [203 289] [268 225] [235 265] [308 202] [200 312] [217 300] [174 356] [346 188] [300 249] [329 225] [354 201] [326 237] [243 328] [350 245] [318 293] [322 291] [289 331] [327 317])]
    (split-with #(<= (first %) 46) right))

(defn bounding-box [[x-min x-max] [y-min y-max]]
  (for [x (range x-min (inc x-max))
        y (range y-min (inc y-max))
        :when (on-bounding-box [x-min x-max] [y-min y-max] [x y])]
    [x y]))

; Find the largest non-infinite region by closest-manhattan distance.
(defn solve-1 []
  (let [points (parse input)
        x-bounds (map first (min-max-by first points))
        y-bounds (map second (min-max-by second points))
        closest-point-map (map #(scan-x (first x-bounds) % points)
                               (range (first y-bounds)
                                      (inc (second y-bounds))))]
    closest-point-map))
    ;(take 10 (map closest-point-map (bounding-box x-bounds y-bounds)))))
    ;(->> closest-point-map
    ;  (remove #(some points-with-infinite-region (first %)))))
    ;  vals
    ;  frequencies
    ;  (apply max)))

(def points (parse input))
(def x-bounds (map first (min-max-by first points)))
(def y-bounds (map second (min-max-by second points)))
(def closest-point-map (apply merge (map #(scan-x (first x-bounds) % points)
                                         (range (first y-bounds) (second y-bounds)))))
(def points-with-infinite-region (map second (filter #(bbox (first %)) closest-point-map)))
(def remaining-map (remove #(points-with-infinite-region (first %)) closest-point-map))
(def freqs (frequencies (vals remaining-map)))
(def answer (apply max-key second freqs))

#_(def points-with-infinite-region (into #{} (map closest-point-map (bounding-box x-bounds y-bounds))))


; Find the number of points within 10000 manhattan distance of each point.
(defn solve-2 [])
  ;; TODO

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
