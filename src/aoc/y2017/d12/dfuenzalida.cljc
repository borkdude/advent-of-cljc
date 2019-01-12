(ns aoc.y2017.d12.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d12.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn parse-line [s]
  (let [[_ from to] (first (re-seq #"(\d+) <-> (.*)" s))]
    (into #{from} (s/split to #", "))))

(defn grow-set [s ss]
  (reduce (fn [a b] (if (or (some b a) (some a b)) (into a b) a)) s ss))

(defn grow [s ss]
  (let [new-set (grow-set s ss)]
    (if (not= s new-set)
      (recur new-set ss)
      s)))

(defn read-input []
  (s/split input #"\n"))

(defn solve-1 []
  (let [problem-input (read-input)
        zero-set      (grow #{"0"} (map parse-line problem-input))]
    (count zero-set)))

(defn parse-from [s]
  (let [[_ from] (first (re-seq #"(\d+) .*" s))]
    #{from}))

(defn all-sets [froms problem-input]
  (let [input-sets (map parse-line problem-input)]
    (loop [from-sets (reduce into froms)
           sets      #{}]
      (if (empty? from-sets)
        sets
        (let [ffrom (first from-sets)
              gfrom (grow #{ffrom} input-sets)]
          ;; now we remove the contents of gfrom from from-sets
          (recur
           (reduce disj from-sets gfrom)
           (conj sets gfrom)))))))

(defn solve-2 []
  (let [problem-input (read-input)
        froms         (map parse-from problem-input)]
    (count (all-sets froms problem-input))))

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
