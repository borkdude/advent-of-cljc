(ns aos.y2017.d01.dandorman
  (:require
   [aos.y2017.d01.data :refer [input answer-1 answer-2]]
   [aos.utils :as u :refer [deftest]]
   [clojure.test :refer [is testing]]))

(defn inverse-captcha [pairs]
  (transduce
   (comp (filter (partial apply =))
         (map first)
         (map #?(:clj int, :cljs #(.charCodeAt % 0)))
         (map #(- % 48)))
   +
   pairs))

(defn solve-1 [s]
  (let [pairs (->> (take (inc (count s)) (cycle s))
                   (partition 2 1))]
    (inverse-captcha pairs)))

(defn solve-2 [s]
  (let [pairs (->> (split-at (/ (count s) 2) s)
                   cycle
                   (partition 2 1)
                   (take 2)
                   (mapcat (partial apply map vector)))]
    (inverse-captcha pairs)))


(deftest part-1
  (is (= answer-1 (solve-1 input))))

(deftest part-2
  (is (= answer-2 (solve-2 input))))
