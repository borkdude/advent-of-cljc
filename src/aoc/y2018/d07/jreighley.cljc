(ns aoc.y2018.d07.jreighley
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d07.data :refer [input answer-1 answer-2]]
   [clojure.string  :refer [split-lines]]
   [clojure.test :refer [is testing]]))

(def sched
  (delay
    (let [raw (split-lines input)
          precedent-keys (sort-by first (map #(vector (subs % 5 6) (subs % 36 37 ))raw))]
       precedent-keys)))

(def all-nodes (sort (reduce into #{} @sched)))

(def remaining-nodes #(remove (set %) all-nodes))

(def jobtime (delay (zipmap all-nodes (range 61 (+ 60 27)))))

(defn find-parents [job unsat]
  (->> (filter #(= (last %) job) unsat)
    (map #(first %))))

(defn reduced-instructions [acc]
      (remove #((set acc) (first %)) @sched))

(defn undependant [acc]
  (filter #(empty? (find-parents % (reduced-instructions acc))) (remaining-nodes acc)))

(defn do-jobs [acc]
  (let [new-acc (conj acc (first (undependant acc)))]
   (if (= (count all-nodes) (count acc))
     (apply str acc)
     (recur new-acc))))

(defn solve-1 []
  (do-jobs []))

;;  PART 2
(defn sched-times [done ip tics]
  (let [free-workers (- 5 (count ip))
        wip (set (map first ip))
        av-work  (remove wip (undependant done))
        new-work (->> (take free-workers av-work))
        work-timer (vec (for [job new-work]
                          [job (+ tics (@jobtime job))]))
        interum-ip (reduce conj ip work-timer)
        next-tic (when (not-empty interum-ip) (apply min (map last interum-ip)))
        next-done (reduce conj done (map first (filter #(= next-tic (last %)) interum-ip)))
        next-ip (vec (remove #(= next-tic (last %)) interum-ip))]
   (if (= (count done) 26)
     tics
     (recur next-done next-ip next-tic))))

(defn solve-2 []
  (sched-times [] [] 0))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
