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

#_(defn sum-all [{:keys [children metadata]}]
    (reduce + (concat metadata (map #(or (:score %) 0) children))))

(defn by-child-weight [{:keys [children metadata]}]
  (if (> (count children) 1)
    (let [child-scores (zipmap (range) (map :score children))]
      (reduce
        (fn [acc index]
          (+ acc
             (or (child-scores index)
                 0)))
        0 metadata))
    (reduce + metadata)))

(defn node-score-by [{:keys [score-fn input-data] :as args}]
  ;(println "data" input-data)
  (let [[n-children n-meta & after-header] input-data
        ;_ (println "args" args)
        new-args (assoc args :input-data after-header)
        ;_ (println "new-args" new-args)
        children (take (inc n-children)
                       (iterate node-score-by new-args))
        ;_ (println "node" n-children n-meta after-header)
        last-child (last children)
        leftovers (:input-data last-child)
        [metadata after-meta] (split-at n-meta leftovers)
        context {:children children
                 :metadata metadata}
        score (score-fn context)]
    ;(println "node" n-children n-meta children)
    ;(println "node-score" score)
    (-> args
        (assoc :input-data after-meta)
        (assoc :score score))))

(defn solve-1 []
  (first (node-score (parse input)))
  #_(:score (node-score-by {:score-fn sum-all :input-data (parse input)})))

(defn solve-2 []
  (:score (node-score-by {:score-fn by-child-weight :input-data (parse input)})))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
