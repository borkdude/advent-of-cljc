(ns aoc.y2018.d08.mrmcc3
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d08.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(def count-vals (map (comp count :vals)))

(defn node-value [subs meta]
  (if (seq subs)
    (let [value-fn #(:value (nth subs (dec %) {:value 0}))]
      (transduce (map value-fn) + meta))
    (reduce + meta)))

(defn children [vals]
  (lazy-seq
    (let [c     (nth vals 0)
          m     (nth vals 1)
          subs  (vec (take c (children (subvec vals 2))))
          sub-l (transduce count-vals + subs)
          l     (+ 2 sub-l m)
          meta  (subvec vals (+ 2 sub-l) l)]
      (cons {:vals  (subvec vals 0 l)
             :subs  subs
             :meta  meta
             :value (node-value subs meta)}
            (children (subvec vals l))))))

(def root
  (->> (str/split input #" ") (mapv u/parse-int) children first delay)) ;; delay

(defn solve-1 []
  (->> @root
       (tree-seq (comp seq :subs) :subs)
       (mapcat :meta)
       (reduce +)))

(defn solve-2 []
  (:value @root))

(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest part-2
         (is (= (str answer-2)
                (str (solve-2)))))
