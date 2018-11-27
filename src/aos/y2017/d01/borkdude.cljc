(ns aos.y2017.d01.borkdude
  (:require
   [aos.y2017.d01.data :refer [input answer-1 answer-2]]
   [aos.utils :as u :refer [deftest]]
   [clojure.test :refer [is testing]]))

(deftest part-1
  (is (= answer-1
         (reduce
          +
          (map
           (fn [a b]
             (if (= a b)
               (u/parse-int (str a)) 0))
           input
           (drop 1 (cycle input)))))))

(deftest part-2
  (is (= answer-2
         (let [half (/ (count input) 2)]
           (reduce
            +
            (map
             (fn [a b]
               (if (= a b)
                 (u/parse-int (str a)) 0))
             input
             (drop half (cycle input))))))))
