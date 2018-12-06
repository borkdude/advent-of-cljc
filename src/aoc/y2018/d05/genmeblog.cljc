(ns aoc.y2018.d05.genmeblog
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

#?(:clj (set! *unchecked-math* :warn-on-boxed))

;;

(defn char->int [c]
  #?(:clj (int c)
     :cljs (.charCodeAt c 0)))

(def mapper #?(:clj pmap :cljs map))
(def polymer (delay (map char->int input)))

;;

(defn react
  ([] (react nil @polymer))
  ([back poly]
   (if-let [c (first poly)]
     (if (and (seq back) (== 32 (Math/abs (- (int (first back)) (int c)))))
       (recur (rest back) (rest poly))
       (recur (cons c back) (rest poly)))
     back)))

(defn removed-unit-reactions []
  (mapper #(let [npolymer (remove #{%1 %2} @polymer)]
             (count (react nil npolymer))) (range 97 123) (range 65 91)))

;;

(def solve-1 #(count (react)))
(def solve-2 #(apply min (removed-unit-reactions)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
