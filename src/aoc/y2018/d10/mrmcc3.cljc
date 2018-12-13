(ns aoc.y2018.d10.mrmcc3
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d10.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]))

(defn parse-line [line]
  (let [in (-> (str/replace line #"<" "[")
               (str/replace #">" "]")
               (str/replace #"position=|velocity=" ""))]
    (u/read-string (str "[" in "]"))))

;; project the location of all points at time t

(defn project [data t]
  (for [[[x y] [vx vy]] data]
    [(+ x (* vx t)) (+ y (* vy t))]))

;; calculate the height of the bounding box around all points.
;; because all velocities are constant this will reduce linearly
;; until a minimum then increase linearly from that time.

(defn height [points]
  (- (apply max (map second points))
     (apply min (map second points))))

;; if we know a time t' after the minimum is reached we can
;; compute the final time and final state directly. To find
;; t' we keep doubling t until the bounding height increases.

(defn overshoot [data h t']
  (let [h' (height (project data t'))]
    (if (< h h') (reduced [t' h']) h')))

(defn minimize-height [data]
  (let [h0 (height (project data 0))
        m  (- h0 (height (project data 1)))
        ts (map #(bit-shift-left 1 %) (range))
        [t1 h1] (reduce (partial overshoot data) h0 ts)]
    (/ (+ h0 (- h1) (* m t1)) (* 2 m))))

(defn draw [points]
  (let [xs      (map first points)
        ys      (map second points)
        min-x   (apply min xs)
        min-y   (apply min ys)
        max-x   (apply max xs)
        max-y   (apply max ys)
        width   (inc (- max-x min-x))
        height  (inc (- max-y min-y))
        ps      (set points)
        squares (for [y (range height) x (range width)]
                  (if (contains? ps [(+ x min-x) (+ y min-y)]) "#" "."))]
    (->> (partition-all width squares)
         (map #(apply str %))
         (str/join "\n"))))

(def data
  (delay (map parse-line (str/split-lines input))))

(def final-time
  (delay (minimize-height @data)))

(defn solve-1 []
  (println (draw (project @data @final-time))))

(defn solve-2 []
  @final-time)

(deftest part-1
         (is (= 204673534 (hash (with-out-str (solve-1))))))

(deftest part-2
         (is (= (str answer-2) (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
  (solve-1)
  (solve-2)
  )
