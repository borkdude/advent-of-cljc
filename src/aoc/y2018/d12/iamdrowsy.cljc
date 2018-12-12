(ns aoc.y2018.d12.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d12.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]
    [com.rpl.specter :refer [setval MAP-VALS pred= transform MAP-KEYS NONE]]))

(defn parse-rule [rule]
  (let [[a b c d e _ _ _ _ r] (seq rule)]
    [[a b c d e] r]))

(defn remove-empty [pots]
  (setval [MAP-VALS (pred= \.)] NONE pots))

(defn parse-input []
  (let [[pots _ & rules] (str/split-lines input)]
    {:rules (into {} (map parse-rule rules))
     :pots (remove-empty (zipmap (range) (drop 15 pots)))}))

(defn env-indices [c]
  [(- c 2) (dec c) c (inc c) (+ c 2)])

(defn step [{:keys [rules pots] :as state} index]
  (let [env (mapv #(get pots % \.) (env-indices index))]
    [index (rules env \.)]))

(defn next-gen [{:keys [rules pots] :as state}]
  (let [min-index (- (apply min (keys pots)) 2)
        max-index (+ (apply max (keys pots)) 3)]
    (assoc state
      :pots
      (remove-empty
        (into {} (map (partial step state)
                      (range min-index max-index)))))))

(defn solve-1 []
  (->> (drop 20 (iterate next-gen (parse-input)))
      (first)
      (:pots)
      (keys)
      (reduce +)))

(defn min-index [pots]
  (apply min (keys pots)))

(defn finger-print [pots]
  (let [mi (min-index pots)]
    (map #(- % mi) (sort (keys pots)))))

(defn find-first-recurrence [state]
  (loop [index 0
         current state
         cache {}]
    (if-let [recurrence (cache (finger-print (:pots current)))]
      (assoc recurrence
        :next-index index
        :next-pots (:pots current))
      (recur (inc index)
             (next-gen current)
             (assoc cache (finger-print (:pots current))
                          {:pots (:pots current)
                           :index index})))))

(defn solve-2 []
  (let [state (parse-input)
        {:keys [index next-index pots next-pots]} (find-first-recurrence state)
        index-diff (- next-index index)
        index-shift (- (min-index next-pots) (min-index pots))
        known-fast-forward-steps (quot (- 50000000000 index) index-diff)
        known-fast-forward-shift (* index-shift known-fast-forward-steps)
        missing-steps (- 50000000000 known-fast-forward-steps index)
        pot-after-fast-forward (transform [MAP-KEYS] #(+ known-fast-forward-shift %)
                                          pots)
        final-pots (:pots (first (drop missing-steps
                                       (iterate next-gen
                                                (assoc state
                                                  :pots pot-after-fast-forward)))))]
     (reduce + (keys final-pots))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch
(comment
  (t/run-tests))

