(ns aoc.y2018.d08.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d08.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(defn parse [input]
  (map u/parse-int (re-seq #"\d+" input)))

(defn node-score
  ([data]
   (node-score 0 data))
  ([prev-score [n-children n-meta & data]]
   (let [subtree (take (inc n-children) (iterate #(apply node-score %) [prev-score data]))
         last-child (last subtree)
         meta+remaining (second last-child)
         [metadata remaining] (split-at n-meta meta+remaining)]
     [(+ (first last-child) (reduce + metadata)) remaining])))

#_(defn sum-all [{:keys [children metadata]}]
    (apply + (concat metadata (map first children))))

(defn sum-all [{:keys [child-scores metadata]}]
  (reduce + (concat metadata child-scores)))

(defn by-child-weight [{:keys [child-scores metadata]}]
  (if (seq child-scores)
    (let [score-map (zipmap (range 1 (inc (count child-scores))) child-scores)]
      (->> metadata
           (map score-map)
           (filter identity)
           (reduce +)))
    (reduce + metadata)))

(defn node-score-by [{:keys [score-fns input-data] :as args}]
      ;(println "data" (take 10 input-data))
  (let [[n-children n-meta & after-header] input-data
        children (drop 1 (take (inc n-children)
                               (iterate node-score-by {:score-fns score-fns
                                                       :input-data after-header})))
        ;_ (println "node" n-children n-meta (take 10 after-header))
        leftovers (or (:input-data (last children)) after-header)
        [metadata after-meta] (split-at n-meta leftovers)
        scores (zipmap score-fns (map (fn [f]
                                        (f {:child-scores (map #(get-in % [:scores f]) children)
                                            :metadata metadata})) score-fns))]
    ;(println "node" n-children n-meta children)
    ;(println "node-score" n-children n-meta metadata (vals scores) (map :scores children))
    (-> args
        (assoc :input-data after-meta)
        (assoc :scores scores))))

(def solve-both
  (memoize
    #(node-score-by {:score-fns [sum-all by-child-weight] :input-data (parse input)})))

(defn solve-1 []
  (get-in (solve-both) [:scores sum-all]))

(defn solve-2 []
  (get-in (solve-both) [:scores by-child-weight]))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
