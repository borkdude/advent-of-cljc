(ns aoc.y2018.d08.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d08.data :refer [input answer-1 answer-2]]
   [clojure.test :as t :refer [is testing]]))

(defn parse [xs num-nodes] ;; returns [{:children [{...}] :meta [1 2 3]}]
  (loop [xs xs, num-nodes num-nodes, nodes []] 
    (if (zero? num-nodes)
      [nodes xs]
      (let [[[num-children num-meta] xs] (split-at 2 xs)
            [children xs] (if (pos? num-children) (parse xs num-children) [[] xs])
            [meta xs]     (split-at num-meta xs)]
        (recur xs
               (dec num-nodes)
               (conj nodes {:children children :meta meta}))))))

(defn read-input []
  (read-string (str "[" input "]")))

(defn solve-1 []
  (let [tree (ffirst (parse (read-input) 1))]
    (->> (tree-seq :children :children tree)
         (mapcat :meta)
         (reduce +))))

(defn eval-tree [{:keys [children meta]}]
  (if (empty? children)
    (reduce + meta) ;; childless node? sum of its meta entries
    (let [num-children (count children)
          indexes      (->> meta (filter pos?) (map dec) (filter #(< % num-children)))]
      (reduce + (map #(eval-tree (get children %)) indexes)))))

(defn solve-2 []
  (let [tree (ffirst (parse (read-input) 1))]
    (eval-tree tree)))

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
