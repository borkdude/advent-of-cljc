(ns aoc.y2018.d04.mfikes
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d04.data :refer [input answer-1 answer-2]]
   [clojure.string :as string]
   [clojure.test :refer [is testing]]))

(def input-lines (string/split-lines input))

(defn parse-int [s]
  #?(:clj (Integer/parseInt s) :cljs (js/parseInt s)))

(defn map-vals [f m]
  (reduce-kv (fn [m k v] (assoc m k (f v))) {} m))

(defn mode [coll]
  (apply max-key val (frequencies coll)))

(defn nap-log [input-lines]
  (:log (reduce (fn [acc input-line]
                  (let [minute (parse-int (subs input-line 15 17))]
                    (case (subs input-line 19 24)
                      "Guard" (assoc acc :guard-id (read-string (subs input-line 26)))
                      "falls" (assoc acc :start minute)
                      "wakes" (update acc :log conj (-> (select-keys acc [:guard-id :start])
                                                      (assoc :end minute))))))
          {:log []} (sort input-lines))))

(defn add-minutes [guard-id->minutes log-entry]
  (update guard-id->minutes (:guard-id log-entry)
    into (range (:start log-entry) (:end log-entry))))

(defn solve-1 []
  (let [guard-id->minutes  (->> input-lines nap-log (reduce add-minutes {}))
        [guard-id minutes] (apply max-key (comp count val) guard-id->minutes)
        [minute]           (mode minutes)]
    (* guard-id minute)))

(defn solve-2 []
  (let [guard-id->minutes   (->> input-lines nap-log (reduce add-minutes {}))
        guard-id->mode      (map-vals mode guard-id->minutes)
        [guard-id [minute]] (apply max-key (comp val val) guard-id->mode)]
    (* guard-id minute)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
