(ns aoc.y2018.d04.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d04.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(defn get-minutes [inst]
  #?(:clj (.getMinute ^java.time.LocalDateTime inst)
     :cljs (.getMinutes (js/Date. inst))))

(defn instant [s]
  #?(:clj (java.time.LocalDateTime/parse s)
     :cljs (js/Date.parse s)))

(defn input-lines [] (str/split-lines input))

(defn parse-inst [s]
  (let [[date-str time-str]
        (str/split (second (re-find #"\[(.+)\]" s)) #" ")
        inst-str (str date-str "T" time-str)]
    (instant inst-str)))

(defn parse-event
  "Examples (input =>
             output):

  \"[1518-08-16 00:04] Guard #2963 begins shift\" =>
  #:advent.day-04{:guard-id \"2963\"}

  \"[1518-05-30 00:48] falls asleep\" =>
  :advent.day-04/falls-asleep

  \"[1518-09-09 00:43] wakes up\"
  :advent.day-04/wakes-up
  "
  [s]
  (let [event (second (re-find #"] (.+)" s))]
    (if-let [guard-id (second (re-find #"Guard #(\d+) begins shift" event))]
      {::guard-id guard-id}
      (get {"wakes up"     ::wakes-up
            "falls asleep" ::falls-asleep}
           event))))

(defn parsed-lines [event-strings]
  (map
    (fn [s] [(parse-inst s) (parse-event s)])
    event-strings))

(defn group-by-shift [sorted-events]
  (when (seq sorted-events)
    (let [guard-id (get-in (first sorted-events) [1 ::guard-id])
          [events-in-shift remaining-events]
          (split-with
            #(#{::falls-asleep ::wakes-up} (second %))
            (rest sorted-events))]
      (cons {guard-id (partition 2 events-in-shift)}
            (lazy-seq (group-by-shift remaining-events))))))

(defn interval-length [t1 t2]
  (- (get-minutes t2) (get-minutes t1)))

(defn time-asleep [nap]
  (->> nap
       (map first)
       (apply interval-length)))

(def prep-data
  "Take a seq of input strings, parse the data in them, then
  organize the data into a map of guard-id => list of all the
  sleep/wake event pairs across all the guard's shifts, sorted by
  timestamp."
  (memoize #(->> %
                 str/split-lines
                 parsed-lines
                 (sort-by first)
                 group-by-shift
                 (apply merge-with concat))))

;; Part 1 - Find the sleepiest guard (by total time asleep) then find
;; the minute of the hour on which the guard is most often asleep.
(defn sleepiest-guard [guards-naps]
  (->> guards-naps
       (map (fn [[guard-id naps]]
              [guard-id (->> naps
                             (map time-asleep)
                             (reduce +))]))
       (apply max-key second)))

(defn sleepiest-minute [guards-naps]
  (when (seq guards-naps)
    (let [nap-intervals (map (fn [[asleep awake]]
                               (range (get-minutes (first asleep))
                                      (get-minutes (first awake))))
                             guards-naps)
          sleepiest-minute (->> nap-intervals
                                flatten
                                frequencies
                                (apply max-key second))]
      sleepiest-minute)))

(defn solve-1 []
  (let [all-guards-naps (prep-data input)
        sleepy-guard (first (sleepiest-guard all-guards-naps))
        one-guards-naps (get all-guards-naps sleepy-guard)
        [sleepiest-minute _] (sleepiest-minute one-guards-naps)
        answer (* (u/parse-int sleepy-guard) sleepiest-minute)]
    answer))

;; Part 2 - Find the sleepiest minute for each guard
(defn solve-2 []
  (let [[guard-id [sleepiest-minute _]]
        (->> input
             prep-data
             (map (fn [[guard naps]]
                    [guard (sleepiest-minute naps)]))
             (filter second)
             (apply max-key #(get-in % [1 1])))
        answer (* (u/parse-int guard-id) sleepiest-minute)]
    answer))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
