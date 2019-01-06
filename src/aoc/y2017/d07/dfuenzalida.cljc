(ns aoc.y2017.d07.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d07.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn parents-in [[k vs]]
  (reduce merge {}
          (map #(hash-map % k) vs)))

(defn parents-map [m]
  (reduce merge (map parents-in m)))

(defn parse-line [s]
  (let [[_ parent weight] (first (re-seq #"(\w+) \((\d+)\)" s))
        [_ s2]            (first (re-seq #".* -> (.*)" s))
        children          (when s2 (s/split s2 #", "))]
    [parent (read-string weight) children]))

(defn lines-to-children-map [lines]
  (->> lines
       (map parse-line)
       (map (juxt first last)) ;; name and children
       (filter last)           ;; leave only nodes with children
       (into {})
       parents-map))

(defn find-root [lines]
  (let [data-map (lines-to-children-map lines)]
    (first
     (filter #(nil? (data-map %))
             (vals data-map)))))

(defn read-input []
  (s/split-lines input))

(defn solve-1 []
  (find-root (read-input)))

(defn root-name []
  (find-root (read-input)))

(defn load-map [lines f]
  (->> lines
       (map parse-line)
       (map f)
       (filter last) ;; we don't want nil values in the hashmap
       (into {})))

(defn weight [weight-map children-map node]
  (+ (weight-map node)
     (reduce +
             (map (partial weight weight-map children-map)
                  (children-map node)))))

(defn unbalanced [weight-map children-map node]
  (if (<= (count (into #{} (map (partial weight weight-map children-map)
                                (children-map node)))) 1)
    node ;; the current node is not balanced, all children have same weight
    
    (let [unb-child (->> (map (juxt identity
                                    (partial weight weight-map children-map))
                              (children-map node))
                         (group-by second)
                         (filter #(= 1 (count (second %))))
                         first second first first)]
      (unbalanced weight-map children-map unb-child))))

(defn solve-2 []
  (let [input              (read-input)
        large-weight-map   (load-map input (juxt first second))
        large-children-map (load-map input (juxt first last))
        large-unbalanced   (unbalanced large-weight-map large-children-map (root-name))
        large-parent-map   (lines-to-children-map input)
        large-desired-tree (->> large-unbalanced
                                large-parent-map
                                large-children-map
                                (filter #(not= large-unbalanced %))
                                first
                                (weight large-weight-map large-children-map))]
    (+ (large-weight-map large-unbalanced)
       (- large-desired-tree
          (weight large-weight-map large-children-map large-unbalanced)))))

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
