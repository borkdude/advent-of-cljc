(ns aoc.y2017.d10.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d10.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn update-vec [v v2 offset i]
  (if (seq v2)
    (recur (assoc-in v [(mod (+ i offset) (count v))] (first v2))
           (rest v2) offset (inc i))
    v))

(defn iter1 [elems lengths pos skip]
  (if (seq lengths)
    (let [fstlen (first lengths)
          l      (count elems)
          revd   (->> elems cycle (drop pos) (take fstlen) reverse vec)
          uptd   (update-vec elems revd pos 0)]
      (recur uptd
             (rest lengths)
             (mod (+ pos fstlen skip) l)
             (inc skip)))
    elems))

(defn read-input []
  (read-string (str "[" input "]")))

(defn solve-1 []
  (let [nums (iter1 (into [] (range 256)) (read-input) 0 0)]
    (* (first nums) (second nums))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ascii [c]
  #?(:clj (int c)
     :cljs (.charCodeAt c 0)))

(defn str-to-len [s]
  (into (mapv ascii s) [17, 31, 73, 47, 23]))

(defn iter2 [elems lengths pos skip]
  (if (seq lengths)
    (let [fstlen (first lengths)
          l      (count elems)
          revd   (->> elems cycle (drop pos) (take fstlen) reverse vec)
          uptd   (update-vec elems revd pos 0)]
      (recur uptd
             (rest lengths)
             (mod (+ pos fstlen skip) l)
             (inc skip)))
    [elems pos skip]))

(defn run-rounds [elems lengths]
  (loop [e elems
         p 0
         s 0
         rounds 64]
    (if (pos? rounds)
      (let [[e2 p2 s2] (iter2 e lengths p s)]
        (recur e2 p2 s2 (dec rounds)))
      e)))

(defn dense-hash [xs]
  (let [seqs (partition 16 xs)]
    (map #(reduce bit-xor %) seqs)))

(defn to-hex [n]
  (str
   (nth "0123456789abcdef" (int (/ n 16)))
   (nth "0123456789abcdef" (rem n 16))))

(defn knot-hash [s]
  (let [lengths (str-to-len s)
        elems   (run-rounds (into [] (range 256)) lengths)
        densed  (dense-hash elems)]
    (apply str (map to-hex densed))))

(defn solve-2 []
  (knot-hash input))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
)
