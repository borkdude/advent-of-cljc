(ns aoc.y2017.d05.borkdude
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2017.d05.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as str]))

(defn solve-1 []
  (loop [maze (into []
                    (map #(u/parse-int %))
                    (str/split-lines input))
         cur-pos 0
         steps 0]
    (if-let [cur-val (get maze cur-pos)]
      (let [next-maze (assoc maze cur-pos (inc cur-val))
            next-pos (+ cur-pos cur-val)]
        (recur next-maze next-pos (inc steps)))
      steps)))

(defn solve-2 []
  (let [^ints maze
        (->>
         (into []
               (map #(u/parse-int %))
               (str/split-lines input))
         (into-array #?(:clj Integer/TYPE)))
        length ^int (alength maze)]
    (loop [cur-pos 0
           steps 0]
      (if (< cur-pos length)
        (let [cur-val (aget maze cur-pos)]
          (if (zero? cur-val)
            (do
              (doto maze
                (aset cur-pos 2))
              (recur
               (inc cur-pos)
               (+ steps 2)))
            (do (doto maze
                  (aset
                   cur-pos
                   (if (>= cur-val 3)
                     (dec cur-val)
                     (inc cur-val))))
                (recur
                 (+ cur-pos cur-val)
                 (inc steps)))))
        steps))))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
