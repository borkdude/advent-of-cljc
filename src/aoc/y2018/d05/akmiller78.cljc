(ns aoc.y2018.d05.akmiller78
  (:require
   [aoc.utils :refer [deftest]]
   [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

#?(:cljs
   (def pmap map))

(defn char->int [c]
  #?(:clj (int c)
     :cljs (.charCodeAt c 0)))

(def data (map char->int input))

(defn opposite-polarity? [b1 b2]
  (if (nil? b1)
    false
    (and (not= b1 b2)
         (== (bit-clear b1 5) (bit-clear b2 5)))))

(defn react-polymer
  [input]
  (reduce #(let [last-unit (peek %1)]
             (if (opposite-polarity? last-unit %2)
               (pop %1)
               (conj %1 %2))) [] input))

(defn solve-1 []
  (count (react-polymer data)))

(defn solve-2 []
  (apply min (pmap (fn [char]
                     (let [input (remove #(or (= % char)
                                              (= % (+ char 32))) data)]
                       (count (react-polymer input))))
                   (range 65 91))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
