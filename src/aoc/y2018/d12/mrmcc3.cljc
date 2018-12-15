(ns aoc.y2018.d12.mrmcc3
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d12.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]))

(defn add-note [m s]
  (assoc m (subs s 0 5) (subs s (dec (count s)))))

(def initial-state
  (delay
    (let [[init _ & rest] (str/split-lines input)]
      {:pots  (subs init 15) ;; assume init state starts and ends with #
       :start 0
       :cases (reduce add-note {} rest)})))

(defn plant-numbers [start]
  (comp (filter (comp #{\#} first))
        (map second)
        (map #(+ start %))))

(defn sum-plants [{:keys [pots start]}]
  (transduce (plant-numbers start) +
             (map vector pots (range))))

(defn trim [s]
  (let [start (str/index-of s \#)
        end   (str/last-index-of s \#)]
    (subs s start (inc end))))

(defn next-pot [cases s i]
  (let [i- (- i 2) i+ (+ i 3)]
    (cond
      (neg? i-) \.
      (< (count s) i+) \.
      :else (cases (subs s i- i+)))))

(defn next-gen [{:keys [pots cases start] :as state} i]
  (let [padded  (str "..." pots "...")
        pot-fn  (partial next-pot cases padded)
        pot-idx (range (count padded))
        padded' (apply str (map pot-fn pot-idx))
        shift   (- (str/index-of padded' \#)
                   (str/index-of padded \#))
        pots'   (trim padded')]
    (if (= pots pots')
      (reduced (assoc state :shift shift :gen i))
      (assoc state :pots pots' :start (+ start shift)))))

;; note padding with (str "..." pots "...") works because
;; ..... => .
;; ....# => .
;; #.... => .

(defn solve-1 []
  (sum-plants (reduce next-gen @initial-state (range 20))))

(defn solve-2 []
  (let [{:keys [shift gen] :as state}
        (reduce next-gen @initial-state (range))
        shift' (* shift (- 50000000000 gen))]
    (sum-plants (update state :start + shift'))))

(deftest part-1 (is (= (str answer-1) (str (solve-1)))))
(deftest part-2 (is (= (str answer-2) (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
  (time (solve-1))
  (time (solve-2))
  )
