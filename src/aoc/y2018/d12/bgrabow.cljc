(ns aoc.y2018.d12.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d12.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]
    [clojure.set :as set]))

(def parse-plant {\# true \. nil})
(def print-plant (set/map-invert parse-plant))

(defn parse-rule [rule-str]
  (let [rule-vec (vec (seq rule-str))]
    [(->> rule-vec
          (take 5)
          (mapv parse-plant))
     (-> rule-vec
         (get 9)
         parse-plant)]))

(defn parse [input]
  (let [[initial-state-str _ & rule-strs] (str/split-lines input)
        initial-state (->> (re-find #"\#.*" initial-state-str)
                           seq
                           (map parse-plant)
                           (zipmap (range))
                           (into (sorted-map)))
        rules (->> rule-strs
                   (map parse-rule)
                   (into {}))]
    {:initial-state initial-state
     :rules         rules}))

(defn trim-until-some [m range]
  (reduce (fn [m k]
            (if (m k)
              (reduced m)
              (dissoc m k)))
          m range))

(defn trim-ends [lb ub m]
  (-> m
      (trim-until-some (iterate dec ub))
      (trim-until-some (iterate inc lb))))

(defn step [rules state]
  (let [lower-bound (- (first (first state)) 2)
        upper-bound (+ (first (last state)) 2)]
    (->> (reduce (fn [[m a b c d] k]
                   (let [e (state (+ k 2))]
                     [(assoc m k (rules [a b c d e]))
                      b
                      c
                      d
                      e]))
                 [(sorted-map)
                  (state (- lower-bound 2))
                  (state (- lower-bound 1))
                  (state lower-bound)
                  (state (+ lower-bound 1))]
                 (range lower-bound (inc upper-bound)))
         first
         (trim-ends lower-bound upper-bound))))

(defn first-consecutive-duplicate-by [pred coll]
  (reduce (fn [px y]
            (let [py (pred y)]
              (if (= px py)
                (reduced y)
                py)))
          (pred (first coll))
          (rest coll)))

(defn plants-score [state]
  (->> state
       (filter second)
       keys
       (reduce +)))

(defn solve-1 []
  (let [{:keys [initial-state rules]} (parse input)
        step' (partial step rules)]
    (->> (nth (iterate step' initial-state) 20)
         plants-score)))

(defn solve-2 []
  (let [{:keys [initial-state rules]} (parse input)
        step' (partial step rules)
        [n quasi-stable-state] (->> (take 1000 (iterate step' initial-state))
                                    (map vector (range))
                                    (first-consecutive-duplicate-by (comp vals second)))
        score (plants-score quasi-stable-state)
        qss' (step' quasi-stable-state)
        score' (plants-score qss')]
    (+ score (* (- 50000000000 n) (- score' score)))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment

  (defn rand-bool []
    ({0 nil 1 true} (rand-int 2)))

  (defn random-initial-state [n]
    (->> (map vector (range n) (repeatedly rand-bool))
         (into (sorted-map))))

  (defn print-plants [plants]
    (apply str (map print-plant (vals plants))))

  (print-plants (random-initial-state 100))

  (time (doall (repeatedly 10000
                           #(let [{:keys [initial-state rules]} (parse my-input)
                                  rand-state (random-initial-state 100)
                                  step' (partial step rules)
                                  ;_ (println "Searching for QSS in " (print-plants rand-state))
                                  [n quasi-stable-state] (->> (take 1000 (iterate step' rand-state))
                                                              (map vector (range))
                                                              (first-consecutive-duplicate-by (comp vals second)))
                                  ;_ (println "Qss at" n)
                                  ;_ (println "QSS: " (print-plants quasi-stable-state))
                                  score (plants-score quasi-stable-state)
                                  qss' (step' quasi-stable-state)
                                  score' (plants-score qss')]
                              (+ score (* (- 50000000000 n) (- score' score)))))))
  (t/run-tests))
