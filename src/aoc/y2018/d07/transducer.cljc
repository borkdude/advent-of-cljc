(ns aoc.y2018.d07.transducer
  (:refer-clojure :exclude [read-string format])
  (:require [aoc.utils :refer [deftest read-string format]]
            [aoc.y2018.d07.data :refer [input answer-1 answer-2]]
            [clojure.set :refer [difference union]]
            [clojure.string :as string]
            [clojure.test :refer [is testing]]))

(def step->dependencies
  (memoize
   #(->> input
         string/split-lines
         (map (fn [line] (re-seq #"[A-Z]" (string/replace-first line #"S" ""))))
         (group-by second)
         (map (fn [[k v]] [k (into #{} (map first v))]))
         (into {}))))

(defn available? [step->dependencies step]
  (empty? (step->dependencies step)))

(defn available-steps-in-deps [step->dependencies]
  (->> (keys step->dependencies)
       (apply disj (apply union (vals step->dependencies)))))

(defn execute [step->deps step]
  (->> step->deps
       (map (fn [[st deps]] [st (disj deps step)]))
       (into {})
       (#(dissoc % step))))


(defn find-and-do-steps [step->deps acc]
  (if (empty? step->deps)
    acc
    (when-let [steps (->> (keys step->deps)
                          (filter #(available? step->deps %))
                          (union (available-steps-in-deps step->deps))
                          (into (sorted-set)))]
      (for [step steps]
        (find-and-do-steps (execute step->deps step)
                           (str acc step))))))

(defn steps-in-progress [s]
  (->> (vals s)
       (map :step)
       (remove nil?)
       set))

(defn to-seconds [step]
  #?(:clj (- (int (first step)) 4)
     :cljs (- (.charCodeAt step) 4)))

(defn assign-worker [workers step]
  (let [free-worker (->> workers
                         (filter (fn [[w {:keys [seconds step]}]]
                                   (zero? seconds)))
                         ffirst)]
    (assoc workers free-worker {:seconds (dec (to-seconds step)) :step step})))

(defn dec-worker-time [workers]
  (->> workers
       (map (fn [[w {:keys [seconds step]}]]
              (if (zero? seconds)
                [w {:seconds 0 :step step}]
                [w {:seconds (dec seconds) :step step}])))
       (into {})))

(defn free-worker-count [workers]
  (->> workers
       (filter (fn [[w {:keys [seconds]}]] (zero? seconds)))
       count))

(defn execute-finished-steps [step->deps workers]
  (let [finished-workers (->> workers
                              (filter (fn [[w {:keys [seconds step]}]]
                                        (and (zero? seconds) step))))
        finished-steps (->> finished-workers
                            (map (fn [[k {:keys [step]}]] step)))]
    (reduce
     (fn [s->d step]
       (execute s->d step))
     step->deps
     finished-steps)))

(defn find-and-do-steps-with-workers [step->deps steps-stack seconds workers]
  (let [step->deps (execute-finished-steps step->deps workers)]
    (if (empty? step->deps)
      seconds
      (if (seq steps-stack)
        (recur step->deps (pop steps-stack) seconds (assign-worker workers (peek steps-stack)))
        (let [steps (->> (keys step->deps)
                         (filter #(available? step->deps %))
                         (set)
                         (union (available-steps-in-deps step->deps))
                         (#(difference % (steps-in-progress workers)))
                         (into (sorted-set)))
              steps-now (vec (take (free-worker-count workers) steps))]
          (if (empty? steps-now)
            (recur step->deps [] (inc seconds) (dec-worker-time workers))
            (recur step->deps steps-now (inc seconds) (dec-worker-time workers))))))))

(defn solve-1 []
  (first (flatten (find-and-do-steps (step->dependencies) ""))))

(defn solve-2 []
  (let [workers
        {:w1 {:seconds 0 :step nil}
         :w2 {:seconds 0 :step nil}
         :w3 {:seconds 0 :step nil}
         :w4 {:seconds 0 :step nil}
         :w5 {:seconds 0 :step nil}}]
    (find-and-do-steps-with-workers (step->dependencies) [] 0 workers)))

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
