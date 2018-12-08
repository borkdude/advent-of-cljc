(ns aoc.y2018.d08.mfikes
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d08.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

(defn parse-license [input]
  (letfn [(parse-node [[child-count metadata-count & more]]
            (let [[children more] (nth (iterate parse-child [[] more]) child-count)
                  [metadata more] (split-at metadata-count more)]
              [{:children children, :metadata metadata} more]))
          (parse-child [[children more]]
            (let [[child more] (parse-node more)]
              [(conj children child) more]))]
    (first (parse-node (map read-string (re-seq #"\d+" input))))))

(defn solve-1 []
  (->> (tree-seq (comp seq :children) :children (parse-license input))
    (mapcat :metadata)
    (reduce +)))

(defn solve-2 []
  (let [node-value (fn node-value [{:keys [children metadata]}]
                     (if (empty? children)
                       (reduce + metadata)
                       (->> metadata
                         (keep #(get children (dec %)))
                         (map node-value)
                         (reduce +))))]
    (node-value (parse-license input))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
