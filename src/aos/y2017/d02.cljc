(ns aos.y2017.d02
  (:require
   [aos.utils :as u]
   [aos.y2017.input :refer [input-d02] :rename {input-d02 input}]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

;;;; Solution 002

;; TODO: Rename back to data after moved to its own namespace.
(def data-002 (->> [input]
                   (map #(str/split % #"\t"))
                   (map #(map u/read-string %))))

(defn solve [f]
  (transduce
   (map f)
   +
   data-002))

(defn solution-7c76c00d-p1 []
  (solve #(- (apply max %) (apply min %))))

(defn divides? [x y]
  (and (not= 0 x y)
       (zero? (mod y x))))

(defn dividing-pairs [xs]
  (for [x1 xs
        x2 xs
        :when (and (distinct? x1 x2)
                   (divides? x1 x2))]
    [x1 x2]))

(defn first-integer-ratio [xs]
  (when-let [[x y] (first (dividing-pairs xs))]
    (/ y x)))

(defn solution-7c76c00d-p2 []
  (solve first-integer-ratio))

;;;; Solution 003

(defn- line-nums [line]
  (u/read-string (str "[" line "]")))

(defn solve1 [lines]
  (letfn [(line-diff [line]
            (->> (line-nums line)
                 ((juxt #(apply max %) #(apply min %)))
                 (apply -)))]
    (transduce (map line-diff) + 0 lines)))

(defn solution-31051433-p1 []
  (solve1 [input]))

(defn solve2 [lines]
  (letfn [(line-div [line]
            (first
             (for [[x & ys] (->> (line-nums line)
                                 (iterate rest)
                                 (take-while seq))
                   y ys
                   :when (or (zero? (mod x y))
                             (zero? (mod y x)))]
               (/ (max x y) (min x y)))))]
    (transduce (map line-div) + 0 lines)))

(defn solution-31051433-p2 []
  (solve2 [input]))

;;;; Tests

(deftest aos-y2017-d02-01-test
  (is (number? (solution-7c76c00d-p1)))
  (is (number? (solution-31051433-p1)))
  )

(deftest aos-y2017-d02-02-test
  (is (number? (solution-7c76c00d-p2)))
  (is (number? (solution-31051433-p2)))
  )

(deftest ^:instrumented sanity-check
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                  :cljs ExceptionInfo)
               (merge 1))))

;;;; Scratch

(comment
  )
