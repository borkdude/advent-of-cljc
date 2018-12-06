(ns aoc.y2018.d04.mrmcc3
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d04.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(defn parse-event [s]
  (let [[_ id] (re-find #"#(\d+)" s)
        ts (str "#inst \"" (subs s 1 11) "T" (subs s 12 17) "\"")]
    (cond->
      {:time (read-string ts)
       :min  (u/parse-int (subs s 15 17))}
      id (assoc :id (read-string id))
      (str/includes? s "asleep") (assoc :sleep true)
      (str/includes? s "wakes") (assoc :wake true))))

(defn process-event
  [{:keys [last-id last-sleep] :as state}
   {:keys [min id sleep wake]}]
  (cond
    id (assoc state :last-id id)
    sleep (assoc state :last-sleep min)
    wake (update-in state [:sleep-map last-id] into
                    (range last-sleep min))))

;; map of guard id -> seq of minutes asleep
(def sleep-map
  (->> (str/split-lines input)
       (map parse-event)
       (sort-by :time)
       (reduce process-event {})
       :sleep-map delay)) ;; delay computation

(defn solve-1 []
  (let [id (key (apply max-key (comp count val) @sleep-map))
        zz (key (apply max-key val (frequencies (@sleep-map id))))]
    (* id zz)))

(defn max-freq [{:keys [best-freq] :as state} id mins]
  (let [[min freq] (apply max-key val (frequencies mins))]
    (if (< best-freq freq)
      (assoc state :best-id id :best-min min :best-freq freq)
      state)))

(defn solve-2 []
  (let [{:keys [best-id best-min]}
        (reduce-kv max-freq {:best-freq 0} @sleep-map)]
    (* best-id best-min)))

(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest part-2
         (is (= (str answer-2)
                (str (solve-2)))))