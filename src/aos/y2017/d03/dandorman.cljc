(ns aos.y2017.d03.dandorman
  (:require
   [aos.utils :as u :refer [deftest]]
   [aos.y2017.d03.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is]]))

(defn pow [x y]
  #?(:clj  (Math/pow x y)
     :cljs (js/Math.pow x y)))

(defn spiral-memory [n]
  (let [rings (iterate #(+ % 2) 1)
        idx+ring (map vector (range) rings)
        [idx ring] (first (drop-while (fn [[_ r]] (< (pow r 2) n)) idx+ring))
        max-offset (quot ring 2)
        offsets (if (zero? max-offset)
                  (repeat 0)
                  (cycle (concat (range max-offset 0 -1) (range max-offset))))
        prev-ring (- ring 2)
        distance-from-prev-ring (- n (pow prev-ring 2))]
    (+ idx (nth offsets distance-from-prev-ring))))

(defn sum-neighbors [grid [x y]]
  (reduce +
          (for [dx (range (dec x) (+ x 2))
                dy (range (dec y) (+ y 2))
                :when (not (and (= dx x) (= dy y)))]
            (get grid [dx dy] 0))))

(defn move [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn spiral-step [{{:keys [ring steps pos dir dirs]} :cursor, :keys [grid]}]
  (let [added (sum-neighbors grid pos)
        next-ring (if (and (zero? steps) (empty? dirs))
                    (+ ring 2)
                    ring)
        next-steps (if (zero? steps)
                     (- next-ring 2)
                     (dec steps))
        next-dir (cond
                   (and (zero? steps) (empty? dirs)) [0 1]
                   (zero? steps) (first dirs)
                   :else dir)
        next-dirs (cond
                    (and (zero? steps) (empty? dirs)) [[-1 0] [0 -1] [1 0]]
                    (zero? steps) (rest dirs)
                    :else dirs)
        next-pos (cond
                   (and (zero? steps) (empty? dirs)) (move pos dir)
                   (zero? steps) (move pos next-dir)
                   :else (move pos dir))]
    {:added added
     :grid (assoc grid pos added)
     :cursor {:ring next-ring
              :steps next-steps
              :dir next-dir
              :dirs next-dirs
              :pos next-pos}}))

(defn walk-spiral
  ([] (walk-spiral {:added 1
                    :grid {[0 0] 1}
                    :cursor {:ring  3
                             :steps 1
                             :pos   [1 0]
                             :dir   [0 1]
                             :dirs  [[-1 0] [0 -1] [1 0]]}}))
  ([state]
   (lazy-seq (cons state (walk-spiral (spiral-step state))))))


(deftest part-1
  (is (= answer-1 (spiral-memory input))))

(deftest part-2
  (is (= answer-2 (->> (walk-spiral)
                       (map :added)
                       (filter #(< input %))
                       first))))
