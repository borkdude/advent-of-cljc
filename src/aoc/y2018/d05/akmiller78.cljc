(ns aoc.y2018.d05.akmiller78
  (:require
   [aoc.utils :refer [deftest]]
   [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

(defn char->int [c]
  #?(:clj (int c)
     :cljs (.charCodeAt c 0)))

(def data (map char->int input))

(defn opposite-polarity? [b1 b2]
  (if (or (nil? b1) (nil? b2))
    false
    (and (not= b1 b2)
         (= (bit-clear b1 5) (bit-clear b2 5)))))

(defn react-polymer
  ([input] (react-polymer input nil))
  ([input exclusions]
   (reduce #(let [last-unit (peek %1)]
              (if (and (not (empty? exclusions)) (contains? exclusions %2))
                %1
                (if (opposite-polarity? last-unit %2)
                  (pop %1)
                  (conj %1 %2)))) [] input)))

(defn solve-1 []
  (count (react-polymer data)))

(defn solve-2 []
  (reduce (fn [cur-min drop-unit]
            (let [exclusions #{drop-unit (bit-flip drop-unit 5)}
                  cnt (count (react-polymer data exclusions))]
              (if (nil? cur-min)
                cnt
                (min cur-min cnt))))
          nil
          (range 65 91)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
