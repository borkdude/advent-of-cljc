(ns aoc.y2018.d04.genmeblog
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format parse-int]]
   [aoc.y2018.d04.data :refer [input answer-1 answer-2]]
   #?(:clj [clojure.instant :refer [read-instant-date]])
   [clojure.string :refer [split-lines]]
   [clojure.test :refer [is testing]]))

;;

(defn date-parser [dt]
  #?(:clj (read-instant-date dt)
     :cljs (js/Date.parse dt)))

(defn log-parser
  "Parse input"
  [line]
  (let [[ydm h m op] (rest (re-find #"\[(\d+-\d+-\d+)\s(\d+):(\d+)\].*(#\d+|wakes|falls)" line))]
    {:date (date-parser (str ydm "T" h ":" m))
     :minute (parse-int m)
     :guard-id (when (= \# (first op))
                 (read-string (subs op 1)))}))

(defn pack-events
  "Reorganize input"
  [[id acc] {:keys [guard-id minute]}]
  (if (nil? guard-id)
    [id (update acc id conj minute)]
    [guard-id acc]))

;; read whole log into a map
(def log
  (->> (split-lines input)
       (map log-parser)
       (sort-by :date)
       (reduce pack-events [-1 {}])
       (second)
       (delay)))

(def time-stats
  (delay (for [[k v] @log
               :let [minutes (->> (reverse v)
                                  (partition 2)
                                  (mapcat #(apply range %)))
                     [minute how-many] (->> minutes
                                            (frequencies)
                                            (sort-by val >)
                                            (first))]]
           [k how-many minute (count minutes)])))

(defn id-with-selector
  "Sort and calculate id using selected value from stats"
  [selector]
  (let [[id _ m] (first (sort-by selector > @time-stats))]
    (* id m)))

;;

(def solve-1 #(id-with-selector last))
(def solve-2 #(id-with-selector second))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
