(ns aoc.y2018.d07.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d07.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn load-graph [input]
  (let [lines     (s/split-lines input)
        pairs     (->> lines
                       (map #(re-seq #"Step (\w) .* (\w) can begin." %))
                       (map first)
                       (map rest))
        nodes     (set (into (map first pairs) (map second pairs)))
        defaults  (reduce into {} (map (fn [s] {s []}) nodes))]
    (merge-with into defaults (->> pairs
                                   (map (fn [[k v]] {k [v]}))
                                   (apply merge-with into)))))

(defn solve-1 []
  (loop [graph (load-graph input)
         plan  []]
    (if (empty? graph)
      (apply str plan)
      (let [parents    (set (keys graph))
            dependents (set (mapcat second graph))
            next-task  (first (sort (for [parent parents
                                          :when (not (dependents parent))]
                                      parent)))]
        (recur (dissoc graph next-task) (conj plan next-task))))))

(defn to-ascii [s]
  #?(:clj  (int (first s))
     :cljs (.charCodeAt s 0)))

(defn duration [s base]
  (let [dur (-> s s/upper-case to-ascii (- (to-ascii "A")) (+ 1 base))]
    [s dur]))

(defn simulate [input num-workers base-duration]
  (loop [graph   (load-graph input)
         working [] ;; [[name duration] ... ]
         ts      -1]
    (if (or (every? empty? [graph working]) (> ts 1000))
      ts
      (let [worked     (mapv #(update-in % [1] dec) working)
            worked-ns  (->> worked (map first) set)
            working-2  (filter (comp pos? second) worked)
            finished   (->> worked (filter (comp zero? second)) (map first))
            graph-2    (reduce dissoc graph finished)
            parents    (set (keys graph-2))
            dependents (set (mapcat second graph-2))
            next-tasks (->> (for [parent parents
                                  :when (and (not (dependents parent))
                                             (not (worked-ns parent)))]
                              parent)
                            sort
                            (take (- num-workers (count working-2)))
                            (map #(duration % base-duration)))]

        (recur graph-2 (into working-2 next-tasks) (inc ts))))))

(defn solve-2 []
  (simulate input 5 60))

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
