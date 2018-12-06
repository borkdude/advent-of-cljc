(ns aoc.y2018.d05.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :refer [is testing]]))

(defn react? [a b]
  (and (not= a b)
       (= (s/upper-case (str a b)) (s/upper-case (str b a)))))

(def react2? (memoize react?))

(defn reduce-polimer2 [^String prob-input]
  (loop [s prob-input, i 0]
    (if (>= i (dec (count s)))
      s
      (let [a (.substring s i (inc i))
            b (.substring s (inc i) (+ i 2))]
        (if (react2? a b)
          (recur (str (.substring s 0 i) (.substring s (+ i 2))) (max 0 (+ -2 i)))
          (recur s (inc i)))))))

(defn smallest-reduction [^String prob-input]
  (let [tinput   (s/trim prob-input)
        letters (->> tinput s/upper-case (map str) set)
        lengths (for [letter letters]
                  {letter
                   (count
                    (reduce-polimer2 (-> prob-input
                                         (s/replace letter "")
                                         (s/replace (s/lower-case letter) ""))))})
        len-map (reduce merge {} lengths)]
    (->> len-map (map second) (apply min))))

(defn solve-1 []
  (count (reduce-polimer2 input)))

(defn solve-2 []
  (smallest-reduction input))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
