(ns aos.y2017.d02.dandorman
  (:require
   [aos.utils :as u :refer [deftest]]
   [aos.y2017.d02.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(defn line->nums [line]
  (->> (str/split line #"\s+")
       (map u/parse-int)))

(defn bounds [nums]
  (reduce (fn [[mn mx :as mn-mx] n]
            (cond-> mn-mx
                    (< mx n) (assoc 1 n)
                    (< n mn) (assoc 0 n)))
          #?(:clj  [Integer/MAX_VALUE Integer/MIN_VALUE]
             :cljs [js/Number.MAX_SAFE_INTEGER js/Number.MIN_SAFE_INTEGER])
          nums))

(defn diff [nums]
  (let [[mn mx] (bounds nums)]
    (- mx mn)))

(defn corruption-checksum [lines]
  (transduce
    (comp
      (map line->nums)
      (map diff))
    +
    lines))

(defn quot? [a b]
  (when (zero? (mod a b))
    (quot a b)))

(defn quotient [nums]
  (reduce
    (fn [seen num]
      (if-let [q (->> seen
                      (mapcat #(vector (quot? % num)
                                       (quot? num %)))
                      (some identity))]
        (reduced q)
        (conj seen num)))
    []
    nums))

(defn evenly-divisible [lines]
  (transduce
    (comp
      (map line->nums)
      (map quotient))
    +
    lines))


(deftest part-1
  (is (= answer-1 (corruption-checksum (str/split input #"\n")))))

(deftest part-2
  (is (= answer-2 (evenly-divisible (str/split input #"\n")))))
