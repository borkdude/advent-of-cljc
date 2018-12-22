(ns aoc.y2018.d22.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d22.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.set :as set]
    [com.rpl.specter :refer [transform MAP-VALS]]))

(defn parse-input [input]
  (let [[d x y] (re-seq #"\d+" input)]
    {:depth (u/parse-int d)
     :target [(u/parse-int x) (u/parse-int y)]}))

(defn erosion-level [depth cave [target-x target-y] [x y]]
  (letfn [(e [m1 m2] (mod (+ depth (* m1 m2)) 20183))]
    (cond (and (zero? x) (zero? y)) (e 0 0)
          (and (= target-x x) (= target-y y)) (e 0 0)
          (zero? x) (e y 48271)
          (zero? y) (e x 16807)
          :else (e (cave [(dec x) y]) (cave [x (dec y)])))))

(defn erosion-map [depth target [max-x max-y]]
  (reduce (fn [cave coords]
            (assoc cave coords (erosion-level depth cave target coords)))
          {}
          (for [x (range (inc max-x))
                y (range (inc max-y))]
            [x y])))

(defn type-map [erosion-map]
  (transform [MAP-VALS] #(mod % 3) erosion-map))

(defn danger-level [type-map]
  (reduce + (vals type-map)))

(defn solve-1 [input]
  (let [{:keys [depth target]} (parse-input input)]
    (danger-level (type-map (erosion-map depth target target)))))

(defn all-valid [type-map]
    (reduce-kv (fn [all [x y] t]
                 (case t 0 (conj all [x y :torch][x y :climbing])
                         1 (conj all [x y :none] [x y :climbing])
                         2 (conj all [x y :none] [x y :torch])))
               #{}
               type-map))

(defn switch-tool [[x y t]]
  (map (fn [t] [x y t]) (disj #{:none :torch :climbing} t)))

(defn move-single [[x y t]]
  #{[(inc x) y t] [(dec x) y t] [x (inc y) t] [x (dec y) t]})

(defn move [current]
  (reduce set/union (map move-single current)))

(defn solve-2 [input]
  (let [{:keys [depth target]} (parse-input input)
        ;; (* 7 %) is an heuristic to generate a cave big enough we would never leave it
        type-map (type-map (erosion-map depth target (map #(* 7 %) target)))
        t (conj target :torch)
        all-valid (all-valid type-map)]
    (loop [time 1
           not-visited all-valid
           reachable {0 [[0 0 :torch]]}]
      (let [next (set/intersection (into (move (reachable (dec time) []))
                                         (mapcat switch-tool (reachable (- time 7) [])))
                                   not-visited)]
        (if (next t)
          time
          (recur (inc time) (set/difference not-visited next) (assoc reachable time next)))))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1 input)))))

(deftest ^:slow part-2
  (is (= (str answer-2)
         (str (solve-2 input)))))

;;;; Scratch

(comment
  (t/run-tests))

