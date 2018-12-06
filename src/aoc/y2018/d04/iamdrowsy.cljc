(ns aoc.y2018.d04.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d04.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as str]
   [com.rpl.specter :as s]))


(def re #"[^:]:(\d+)[^#]*#?(\d+)?.*")

(defn parse-line [s]
  (let [[min guard] (rest (re-find re s))]
    ;because sleeping and waking up aways swap, we don't need the type of event
    (if guard
      {:guard guard}
      {:event (u/parse-int min)})))

(defn add-to-timevec [timevec [start end]]
  (s/setval [(s/srange start end) s/ALL] 1 timevec))

(defn event-list->timevec [events]
  (reduce add-to-timevec
          (into [] (repeat 60 0))
          (partition 2 events)))

(defn partition-days [input]
  (let [guard-num (volatile! 0)
        next-guard? (fn [x]
                      (if (:guard x)
                        (vswap! guard-num inc)
                        @guard-num))]
    (partition-by next-guard? input)))

(defn entries->timevec [[guard & events]]
  {:id (:guard guard)
   :timevec (event-list->timevec (map :event events))})

(def timetable
  (memoize
    #(->> (str/split-lines input)
          (sort)
          (map parse-line)
          (partition-days)
          (map entries->timevec)
          (group-by :id))))

(defn sleep-duration [entries]
  (reduce + (s/select [s/ALL :timevec s/ALL] entries)))

(defn sleepiest-guard+sleeptime []
  (apply (partial max-key val)
         (s/transform [s/MAP-VALS]
                      sleep-duration
                      (timetable))))

(defn highest-index [coll]
  (first (last (sort-by second (s/select [s/INDEXED-VALS] coll)))))

(defn sleepiest-minute+sleep-duration [guard]
  (let [added-sleeps (reduce #(map + %1 %2)
                             (s/select [s/ALL :timevec]
                                       ((timetable) guard)))]
    [(highest-index added-sleeps) (apply max added-sleeps)]))

(defn solve-1 []
  (let [[guard _] (sleepiest-guard+sleeptime)
        [sleepiest-min _] (sleepiest-minute+sleep-duration guard)]
    (* (u/parse-int guard) sleepiest-min)))


(defn solve-2 []
  (let [guards (keys (timetable))
        data (map sleepiest-minute+sleep-duration guards)
        guard-index (highest-index (map second data))]
    (* (u/parse-int (nth guards guard-index))
       (nth (map first data) guard-index))))


(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
