(ns aoc.y2018.d18.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d18.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn char-at [^String s ^long i]
  (.substring s i (inc i)))

(defn adjacents [terrain ^long x ^long y]
  (merge
   {"." 0 "|" 0 "#" 0} ;; defaults
   (let [width  (count (first terrain))
         height (count terrain)]
     (frequencies
      (for [dx (range -1 2)
            dy (range -1 2)
            :let [x2 (+ x dx)
                  y2 (+ y dy)]
            :when (and (not= [0 0] [dx dy])
                       (<= 0 x2) (< x2 width)
                       (<= 0 y2) (< y2 width))]
        (let [c (char-at (nth terrain y2) x2)]
          c))))))

(defn iter-acre [terrain ^long x ^long y]
  (let [acre (char-at (nth terrain y) x)
        adjs (adjacents terrain x y)]
    (cond
      (= "." acre) (let [trees (get adjs "|")]
                     (if (>= trees 3) "|" "."))

      (= "|" acre) (let [lumbs (get adjs "#")]
                     (if (>= lumbs 3) "#" "|"))

      (= "#" acre) (let [lumbs (get adjs "#")
                         trees (get adjs "|")]
                     (if (and (pos? lumbs) (pos? trees)) "#" "."))

      :else "?")))

(defn step-terrain [terrain]
  (into
   []
   (let [width  (count (first terrain))
         height (count terrain)]
     (for [y (range height)]
       (apply str
              (for [x (range width)]
                (iter-acre terrain x y)))))))

(defn iter-terrain [terrain]
  (iterate step-terrain terrain))

(defn solve-1 []
  (let [input   (s/split-lines input)
        iters   10
        terrain (apply str (last (take (inc iters) (iter-terrain input))))
        freqs   (frequencies terrain)
        trees   (get freqs \| 0)
        lumbs   (get freqs \# 0)]
    (* trees lumbs)))

(defn find-fixed-terrain [terrain-iterator]
  (let [tuples (map-indexed vector (take 1000 terrain-iterator))]
    (loop [tuples tuples, seen {}]
      (let [[i terr] (first tuples)]
        (if (seen terr)
          [i (seen terr)] ;; [current-index index-of-previously-seen-terrain]
          (recur (rest tuples) (assoc seen terr i)))))))

(defn solve-2 []
  (let [input  (s/split-lines input)
        terr-i (iter-terrain input)
        [b a]  (find-fixed-terrain terr-i)
        period (- b a)
        index  (+ a (mod (- 1000000000 a) period))
        terr   (first (drop index terr-i))
        freqs  (frequencies (apply str terr))]
    (* (get freqs \| 0) (get freqs \# 0))))

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
