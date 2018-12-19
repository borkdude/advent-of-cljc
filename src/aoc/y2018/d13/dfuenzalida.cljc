(ns aoc.y2018.d13.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d13.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(def cart-direction
  {"<" :left, ">" :right, "^" :up, "v" :down})

(defn char-at [s i]
  (.substring s i (inc i))) ;; works in both CLJ and CLJS!

(defn find-carts [tube-map]
  (for [y (range (count tube-map))
        x (range (count (first tube-map)))
        :let [piece (get (mapv str (get tube-map y)) x)]
        :when (#{"<" ">" "^" "v"} piece)]
    [x y (cart-direction piece) (cycle [:left :straight :right])]))

;; (find-carts example-input) ;; => ([2 0 :right] [9 3 :down])
                        
(def left  {:up :left, :left :down, :down :right, :right :up})
(def right {:up :right, :right :down, :down :left, :left :up})
(def speed {:up [0 -1] :down [0 1] :left [-1 0] :right [1 0]})

(defn crash? [carts] ;; [[x y :dir turn-cycles] ... ]
  (->> carts
       (map (juxt first second))
       frequencies
       (filter (fn [[k v]] (> v 1)))
       seq))

;; "when going :right and enter a '\' we turn right (and end facing :down)
(def right-turns
  #{[:right "\\"] [:down "/"] [:left "\\"] [:up "/"]})

(def left-turns
  #{[:right "/"] [:down "\\"] [:left "/"] [:up "\\"]})

(defn cart-order [[x y & _]]
  (+ (* 10000 y) x))

(defn move-cart [tube-map [x y dir turns]]
  (let [c       (char-at (nth tube-map y) x)]
    (cond
      (= "+" c) (let [turning (first turns)
                      dir2    (get {:left (left dir) :right (right dir)} turning dir)
                      speed2  (get speed dir2)
                      [x2 y2] [(+ x (first speed2)) (+ y (second speed2))]]
                  [x2 y2 dir2 (rest turns)])

      :else (let [nextdir-fn (cond (right-turns [dir c]) right
                                   (left-turns  [dir c]) left
                                   :else identity)
                  dir2       (nextdir-fn dir)
                  [dx dy]    (speed dir2)
                  [x2 y2]    [(+ x dx) (+ y dy)]]
              [x2 y2 dir2 turns]))))

(defn solve-1 []
  (let [tube-map (s/split-lines input)]
    (loop [ticks 0, carts (find-carts tube-map), moved-carts []]
      (let [crashed (crash? (concat carts moved-carts))]
        (if (or crashed (< 1000 ticks))
          (let [[x y] (ffirst crashed)]
            (str x "," y))
          (if (seq carts)
            (recur ticks (rest carts) (conj moved-carts
                                            (move-cart tube-map (first carts))))
            (recur (inc ticks) (sort-by cart-order moved-carts) [])))))))

(defn solve-2 []
  (let [tube-map (s/split-lines input)]
    (loop [ticks 0, carts (find-carts tube-map), moved-carts []]
      (let [crashed (crash? (concat carts moved-carts))]
        (if crashed
          (let [;; _ (println "ticks" ticks "crashed" crashed) ;; DEBUG
                crashsites  (set (map first crashed))
                carts       (remove (fn [[x y & _]] (crashsites [x y])) carts)
                moved-carts (remove (fn [[x y & _]] (crashsites [x y])) moved-carts)]
            (if (<= (+ (count carts) (count moved-carts)) 1)
              (let [[x y & _] (first (concat carts moved-carts))]
                (str x "," y))
              (recur (inc ticks) carts moved-carts)))
          (if (seq carts)
            (recur ticks (rest carts) (conj moved-carts
                                            (move-cart tube-map (first carts))))
            (recur (inc ticks) (sort-by cart-order moved-carts) [])))))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
)
