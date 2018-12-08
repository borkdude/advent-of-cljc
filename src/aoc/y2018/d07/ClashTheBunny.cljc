(ns aoc.y2018.d07.ClashTheBunny
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d07.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as s]
   [clojure.set :as st]))

(defn char->int [character]
  #?(:clj (int character)
     :cljs (.charCodeAt character 0)))

(defn char-range
 ([start end]
  (map char (range (char->int start) (inc (char->int end)))))
 ([start]
  (map char (range (char->int start) #?(:clj Double/POSITIVE_INFINITY
                                        :cljs js/Infinity)))))

(def dest-weight
  (into {} (map hash-map
            (map keyword (filter #(not= "" %) (s/split (apply str (char-range \A \Z)) #"")))
            (range 0 27))))

(defn parse [string]
  (->> string
       s/split-lines
       (map #(s/split %1 #" "))
       (map #(vector (keyword (nth % 1)) (keyword (nth % 7)) ((keyword (nth % 7)) dest-weight)))
       (into [])))

(defn set-of-nodes [graph]
  (:nodeset graph))

(defn get-blocked-nodes [graph]
  (into #{} (keys (filter #(not= 0 ((comp count val) %)) (:in graph)))))

(defn get-available-nodes [graph open-tasks available-tasks]
  (apply sorted-set (st/difference
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

(defn refil-open-tasks [task-cost workers available-tasks open-tasks]
   (into {} (reduce #(assoc %1 %2 (+ task-cost (dest-weight %2)))
                    open-tasks
                    (take (- workers (count open-tasks)) available-tasks))))

(defn get-remaining-tasks [workers open-tasks available-tasks]
  (into [] (drop (- workers (count open-tasks)) available-tasks)))

(defn done? [graph available-tasks open-tasks]
  (= 0 (count (:nodeset graph)) (count available-tasks) (count open-tasks) (count (:adj graph))))

(defn remove-node-from-map-of-sets [nodes]
  (comp (partial into {})
        (partial map #(vector (first %) (apply disj (second %) nodes)))
        (partial filter (comp (partial (comp not contains?) nodes) first))))

(defn remove-node [graph & nodes]
  (let [nodes (into #{} nodes)]
   (-> graph
    (update :nodeset #(apply disj % nodes))
    (update :in (remove-node-from-map-of-sets nodes))
    (update :adj (remove-node-from-map-of-sets nodes)))))

(defn digraph [g & edges]
   (reduce
      (fn [g [n1 n2]]
        (-> g
            (update-in [:nodeset] (fnil conj #{}) n1 n2)
            (update-in [:adj n1] (fnil conj #{}) n2)
            (update-in [:in n2] (fnil conj #{}) n1)))
      g edges))

(defn tasks-complete [{:keys [visited graph open-tasks available-tasks timer workers task-cost]
                       :or {visited [] available-tasks [] open-tasks {} timer 0} :as the-data}]
  (let [visited (apply conj visited (complete-open-tasks open-tasks))]
    (-> the-data
      (assoc :visited visited)
      (update :graph #(apply remove-node % visited))
      (update :open-tasks #(into {} (tick-open-tasks (incomplete-open-tasks %))))
      (update :available-tasks (partial get-available-nodes graph open-tasks))
      (update :timer (fnil inc 0)))))

(defn reload-workers-with-tasks [{:keys [visited graph open-tasks available-tasks timer workers task-cost]
                                  :or {visited [] available-tasks [] open-tasks {} timer 0} :as the-data}]
  (-> the-data
      (update :available-tasks (partial get-remaining-tasks workers open-tasks))
      (update :open-tasks (partial refil-open-tasks task-cost workers available-tasks))))

(defn refil-available-tasks [{:keys [visited graph open-tasks available-tasks timer workers task-cost]
                              :or {visited [] available-tasks [] open-tasks {} timer 0} :as the-data}]
  (-> the-data
      (update :available-tasks #(into available-tasks (get-available-nodes graph open-tasks %)))))

(defn solve [input workers task-cost]
 (let [graph (apply (partial digraph {}) (parse input))]
  (loop [{:keys [visited graph open-tasks available-tasks timer workers task-cost] :as the-data}
         (refil-available-tasks {:graph graph :workers workers :task-cost task-cost})]
    (if (done? graph available-tasks open-tasks)
      {:string (s/join (map name visited))
       :timer  timer}
      (recur ((comp tasks-complete reload-workers-with-tasks refil-available-tasks) the-data))))))

(deftest part-1
  (is (= (str answer-1)
         (str (:string (solve input 1 0))))))

(deftest part-2
  (is (= (str answer-2)
         (str (:timer (solve input 5 60))))))
