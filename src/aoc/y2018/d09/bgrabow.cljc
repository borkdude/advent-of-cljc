(ns aoc.y2018.d09.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d09.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]))

(def my-input "470 players; last marble is worth 72170 points")

(defn parse [input]
  (let [[n-players last-marble] (map u/parse-int (re-seq #"\d+" input))]
    {:n-players   n-players
     :last-marble last-marble}))

(parse my-input)

(def test-input {:n-players   9
                 :last-marble 25})

(defn cw-2 [coll]
  (concat (drop 2 coll) (take 2 coll)))

(defn add-marble [n coll]
  (cons n (cw-2 coll)))

(defn ccw-7 [coll]
  (concat (take-last 7 coll) (drop-last 7 coll)))

(defn collect-points [acc next-marble player]
  ;(println (take-last 10 (:circle acc)))
  (let [rotated (ccw-7 (:circle acc))]
    (-> acc
        (assoc :circle (rest rotated))
        (update-in [:scores player] #(+ % next-marble (first rotated))))))

(cons 1 (cw-2 '(0)))

(defn solve-1 []
  (let [{:keys [n-players last-marble]} (parse my-input)]
    (->> (reduce (fn [acc [marble player]]
                   (try
                     ;(println player marble (:circle acc))
                     (if (zero? (rem marble 23))
                       (collect-points acc marble player)
                       (update acc :circle (partial add-marble marble)))
                     (catch Exception e (println player (:circle acc)))))
                 {:scores (zipmap (range n-players) (repeat 0))
                  :circle '(0)}
                 (map vector (range 1 last-marble) (cycle (range n-players))))
         :scores
         vals
         (apply max))))

(let [{:keys [n-players last-marble]} {:n-players 9 :last-marble 69}]
  (->> (reduce (fn [acc [marble player]]
                 (try
                   ;(println player marble (:circle acc))
                   (if (zero? (rem marble 23))
                     (collect-points acc marble player)
                     (update acc :circle (partial add-marble marble)))
                   (catch Exception e (println player (:circle acc)))))
               {:scores (zipmap (range n-players) (repeat 0))
                :circle '(0)}
               (map vector (range 1 (inc last-marble)) (cycle (range n-players))))
       :circle
       println))

(defn next-circle [old-circle next-scoring-marble]
  (concat (interleave (range (- next-scoring-marble 4) next-scoring-marble) (subvec old-circle 20 24))
          (subvec old-circle 24)
          (subvec old-circle 0 1)
          (interleave (subvec old-circle 1 20) (range (- next-scoring-marble 22) (- next-scoring-marble 4)))))

(defn next-score [old-circle next-scoring-marble]
  (+ next-scoring-marble
     (get old-circle 19)))

(defn solve-2 [])
;; TODO


(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest part-2
         (is (= (str answer-2)
                (str (solve-2)))))
