(ns aoc.y2018.d07.ClashTheBunny
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d07.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as s]
   [loom.graph :as lg]
   [loom.alg :as la]))

(defn char-range
 ([start end]
  (map char (range (int start) (inc (int end)))))
 ([start]
  (map char (range (int start) 256)))) 

(def dest-weight
  (into {} (map hash-map
            (map keyword (s/split (apply str (char-range \A \Z)) #""))
            (range 0 27))))

(defn parse [string]
  (->> string
       s/split-lines
       (map #(s/split %1 #" "))
       (map #(vector (keyword (nth % 1)) (keyword (nth % 7)) ((keyword (nth % 7)) dest-weight)))))

(defn set-of-nodes [graph]
  (:nodeset graph))

(defn get-blocked-nodes [graph]
  (into #{} (keys (filter #(not= 0 ((comp count val) %)) (:in graph)))))

(defn get-available-nodes [graph open-tasks available-tasks]
  (apply sorted-set (clojure.set/difference
                      (set-of-nodes graph)
                      (get-blocked-nodes graph)
                      (into #{} (keys open-tasks))
                      (into #{} available-tasks))))

(defn tick-open-tasks [open-tasks]
  (into {} (map #(update-in % [1] dec)) open-tasks))

(defn some-tasks-complete? [open-tasks]
   (some #(= 0 (second %)) open-tasks))
(defn complete-open-tasks [open-tasks]
  (keys (filter #(= 0 (second %)) open-tasks)))
(defn incomplete-open-tasks [open-tasks]
  (filter #(not= 0 (second %)) open-tasks))

(defn refil-open-tasks [task-cost workers open-tasks available-tasks]
   (into {} (reduce #(assoc %1 %2 (+ task-cost (dest-weight %2)))
                    open-tasks
                    (take (- workers (count open-tasks)) available-tasks))))

(defn get-remaining-tasks [workers open-tasks available-tasks]
  (into [] (drop (- workers (count open-tasks)) available-tasks)))

(defn done? [graph available-tasks open-tasks]
  (= 0 (count (:nodeset graph)) (count available-tasks) (count open-tasks) (count (:adj graph))))

(defn tasks-complete [{:keys [visited graph open-tasks available-tasks timer workers task-cost] :as the-data}]
  (let [complete-open-tasks (complete-open-tasks open-tasks)
        incomplete-open-tasks (incomplete-open-tasks open-tasks)]
    {:visited (apply conj visited complete-open-tasks)
     :graph (apply lg/remove-nodes graph (apply conj visited complete-open-tasks))
     :open-tasks (into {} (tick-open-tasks incomplete-open-tasks))
     :available-tasks (get-available-nodes graph open-tasks available-tasks)
     :timer (inc timer)
     :workers workers :task-cost task-cost}))

(defn reload-workers-with-tasks [{:keys [visited graph open-tasks available-tasks timer workers task-cost] :as the-data}]
   {:visited visited :timer timer :workers workers :task-cost task-cost :graph graph
    :open-tasks (refil-open-tasks task-cost workers open-tasks available-tasks)
    :available-tasks (get-remaining-tasks workers open-tasks available-tasks)})

(defn refil-available-tasks [{:keys [visited graph open-tasks available-tasks timer workers task-cost] :as the-data}]
  {:visited visited :timer timer :workers workers :task-cost task-cost :open-tasks open-tasks :graph graph
   :available-tasks (into available-tasks (get-available-nodes graph open-tasks available-tasks))})

(defn solve-2 [input workers task-cost]
 (let [graph (apply lg/digraph (parse input))]
  (loop [{:keys [visited graph open-tasks available-tasks timer workers task-cost] :as the-data}
         (refil-available-tasks
           {:graph graph :available-tasks []
            :open-tasks {} :visited [] :workers workers
            :timer 0 :task-cost task-cost})]
    (if (done? graph available-tasks open-tasks)
      {:string (s/join (map name visited))
       :timer  timer}
      (recur ((comp tasks-complete reload-workers-with-tasks refil-available-tasks)
              {:visited visited :graph graph :open-tasks open-tasks :available-tasks available-tasks
               :timer timer :workers workers :task-cost task-cost}))))))

(deftest part-1
  (is (= (str answer-1)
         (str (:string (solve-2 input 1 0))))))

(deftest part-2
  (is (= (str answer-2)
         (str (:timer (solve-2 input 5 60))))))
