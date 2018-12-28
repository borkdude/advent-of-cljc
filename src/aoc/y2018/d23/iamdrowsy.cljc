(ns aoc.y2018.d23.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d23.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]
    [com.rpl.specter :refer [transform ALL multi-path ALL select]]))

(def my-pmap #?(:clj pmap
                :cljs map))

(defn parse-line [line]
  (let [[x y z r] (map u/read-string (re-seq #"-?\d+" line))]
    {:pos [x y z]
     :range r}))

(defn parse-input [input]
  (mapv parse-line (str/split-lines input)))

(defn distance [x y]
  (+ (Math/abs (- ^long (x 0) ^long (y 0)))
     (Math/abs (- ^long (x 1) ^long (y 1)))
     (Math/abs (- ^long (x 2) ^long (y 2)))))

(defn in-range? [drone1 pos]
  (<= ^long (distance (:pos drone1) pos) ^long (:range drone1)))

(defn drones-in-range [drones point]
  (count (filter #(in-range? % point) drones)))

(defn dist-from-0 [point]
  (distance [0 0 0] point))

(defn point-sort-fn [drones]
  (fn [p]
    [(drones-in-range drones p) (- ^long (dist-from-0 p)) p]))

(defn scale-bots [bots ^long factor]
  (transform [ALL (multi-path [:pos ALL] :range)] #(quot ^long % factor) bots))

(defn solve-scaled [bots factor [^long x-min ^long x-max ^long y-min ^long y-max ^long z-min ^long z-max]]
  (->> (for [x (range x-min (inc x-max))
             y (range y-min (inc y-max))
             z (range z-min (inc z-max))]
         [x y z])
       (my-pmap (point-sort-fn (scale-bots bots factor)))
       (sort)
       (last)
       (last)))

(defn scale-min-max [min-max ^long factor]
  (transform ALL #(quot ^long % factor) min-max))

(def s 4)
(def hs (quot s 2))

(defn next-min-max [[^long x ^long y ^long z]]
  [(- (* s x) hs) (+ (* s x) hs)
   (- (* s y) hs) (+ (* s y) hs)
   (- (* s z) hs) (+ (* s z) hs)])

(defn solve [bots]
  (loop [factor 16777216
         min-max (scale-min-max (for [i [0 1 2]
                                      op [min max]]
                                  (apply op (select [ALL :pos i] bots)))
                                factor)]
    (if (< factor s)
      (reduce + (solve-scaled bots factor min-max))
      (recur (quot factor s)
             (next-min-max (solve-scaled bots factor min-max))))))

(defn solve-1 []
  (let [drones (parse-input input)
        high-range-drone (last (sort-by :range drones))]
    (count (filter (partial in-range? high-range-drone) (map :pos drones)))))

(defn solve-2 []
  (solve (parse-input input)))

(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest part-2
         (is (= (str answer-2)
                (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests))
