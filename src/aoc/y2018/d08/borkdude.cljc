(ns aoc.y2018.d08.borkdude
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d08.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(def data (map u/parse-int (str/split input #" ")))

(defn parse-nodes
  [[n-children n-meta & nums]]
  (loop [n n-children
         nums nums
         children []]
    (if (zero? n)
      (let [[meta rest] (split-at n-meta nums)]
        [{:meta (vec meta)
          :children children} rest])
      (let [[new-child new-nums] (parse-nodes nums)]
        (recur (dec n) new-nums (conj children new-child))))))

(defn node-sum [node]
  (+ (reduce + (:meta node))
     (reduce #(+ %1 (node-sum %2)) 0 (:children node))))

(def tree (delay (first (parse-nodes data))))

(defn solve-1 []
  (node-sum @tree))

(defn node-value [{:keys [:children :meta] :as node}]
  (cond (not node) 0
        (empty? children) (reduce + meta)
        :else (reduce
               #(+ %1 (node-value (get children (dec %2))))
               0 meta)))

(defn solve-2 []
  (node-value @tree))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
