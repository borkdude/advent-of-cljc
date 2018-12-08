(ns aoc.y2018.d08.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d08.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [com.rpl.specter :as s]))

(defn num-seq [input]
  (map u/parse-int (re-seq #"\d+" input)))

(defn parse-tree [remaining-input]
  (let [[sub# meta# & more] remaining-input]
    (if (zero? sub#)
      {:meta            (take meta# more)
       :remaining-input (drop meta# more)}
      (let [sub-nodes (vec (take sub# (drop 1 (iterate #(parse-tree (:remaining-input %))
                                                       {:remaining-input more}))))
            meta (take meta# (:remaining-input (last sub-nodes)))]
        {:meta meta
         :children sub-nodes
         :remaining-input (drop meta# (:remaining-input (last sub-nodes)))}))))

(def NODES
  (s/recursive-path [] node
     (s/if-path map?
        (s/stay-then-continue
          [:children s/ALL node])
        s/STAY)))

(defn solve-1 []
  (reduce +
          (s/select [NODES :meta s/ALL]
                    (parse-tree (num-seq input)))))

(defn node-value [{:keys [meta children]}]
  (if (empty? children)
    (reduce + meta)
    (let [cost-by-index (frequencies meta)]
      (reduce + (map *
                     (map #(node-value (get children (dec %) 0))
                          (keys cost-by-index))
                     (vals cost-by-index))))))

(defn solve-2 []
  (node-value (parse-tree (num-seq input))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
