(ns aoc.y2018.d04.borkdude
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d04.data :refer [input answer-1 answer-2]] ;; :reload
   [clojure.test :refer [is testing]]
   [clojure.string :as str]))

(def re #"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (Guard #\d+ )?(.*)")

(defn parse-line [s]
  (let [[year month day hour minute guard? action]
        (rest (re-find re s))]
    {:year (u/parse-int year)
     :month (u/parse-int month)
     :day (u/parse-int day)
     :hour (u/parse-int hour)
     :minute (u/parse-int minute)
     :guard (when guard?
              (u/parse-int (re-find #"\d+" guard?)))
     :action action}))

(def data
  (memoize #(map parse-line (str/split-lines input))))

(defn complete-line [{:keys [action] :as m}
                     guard]
  (assoc m
         :guard guard
         :sleeping? (= "falls asleep" action)))

(defn copy-lines [record from until]
  (map #(assoc record :minute %)
       (range from until)))

(def process-data
  (memoize
   #(let [sorted (sort-by (juxt :year :month :day :hour :minute) (data))]
      (first
       (reduce
        (fn [[acc prev-guard prev-record]
             {:keys [:action :guard] :as n}]
          (let [guard (or guard prev-guard)
                record (complete-line n guard)
                copies (if (= "wakes up" action)
                         (copy-lines prev-record
                                     (inc (:minute prev-record))
                                     (:minute record))
                         [])
                new (conj (into acc copies) record)]
            [new guard record]))
        [[] nil nil]
        sorted)))))

(defn max-frequency [vals]
  (first (apply max-key val
                (frequencies vals))))

(def sleeping
  (memoize
   #(keep (fn [m]
            (when (:sleeping? m)
              (select-keys m [:guard :minute]))) (process-data))))

(defn solve-1 []
  (let [guards (map :guard (sleeping))
        most-sleeping-guard (max-frequency guards)
        only-most-sleeping-guard
        (filter #(= most-sleeping-guard (:guard %))
                (sleeping))
        most-frequent-minute (:minute
                              (max-frequency only-most-sleeping-guard))]
    (* most-sleeping-guard most-frequent-minute)))

(defn solve-2 []
  (let [{:keys [:guard :minute]} (max-frequency (sleeping))]
    (* guard minute)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;; What I (re-)learned today:
;; max-key takes a comparison function. a more appropriate name might be max-by
;; (sort-by (comp - val) {:a 1 :b 2}) can be written as (sort-by val > {:a 1 :b 2})
