(ns aoc.y2018.d04.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d04.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :refer [is testing]]))

(defn process-input [input]
  (->> input
       (re-seq #"(.*)")
       (map first)
       (filter seq)
       sort))

(defn format-digit [n]
  (str (if (< n 10) "0" "") n))

(defn format-hh-mm [hh mm]
  (str (format-digit hh) ":" (format-digit mm)))

(defn inc-hh-mm [s]
  (let [[hh mm] (->> (re-seq #"(.*):(.*)" s) first rest (map #(str "1" %)) (map u/parse-int) (map #(mod % 100)))
        inc-hh  (if (>= mm 59) 1 0)]
    (format-hh-mm (mod (+ hh inc-hh) 24) (mod (inc mm) 60))))

(defn hhmm-desc [s]
  (->> s (re-seq #"\[(.*) (.*)\] (.*)") first (drop 2)))

(defn guard-from [s]
  (->> s (re-seq #"Guard #(\d*) (.*)") first second u/parse-int))

(defn sleep-minutes [events]
  (loop [events events
         zzz-entries []
         guard -1
         hhmm  "23:00"
         asleep false]
    (if (seq events)
      (let [[evt-hhmm evt-desc] (hhmm-desc (first events))
            next-hh-mm          (inc-hh-mm hhmm)]
        ;; (println hhmm "->" evt-hhmm "-" evt-desc)
        (cond

          (and (= hhmm evt-hhmm) (s/starts-with? evt-desc "Guard"))
          (let [new-guard (guard-from evt-desc)]
            (recur
             (rest events) zzz-entries new-guard next-hh-mm false))

          (= [hhmm "falls asleep"] [evt-hhmm evt-desc])
          (recur
           (rest events) (conj zzz-entries [guard hhmm]) guard next-hh-mm true)

          (= [hhmm "wakes up"] [evt-hhmm evt-desc])
          (recur
           (rest events) zzz-entries guard next-hh-mm false)

          :otherwise
          (recur
           events (if asleep (conj zzz-entries [guard hhmm]) zzz-entries) guard next-hh-mm asleep)))

      zzz-entries)))

(defn solve-1 []
  (let [sleep-events (sleep-minutes (process-input input))

        ;; find the guard that sleeps the most
        sleepy-guard (->> sleep-events (map first) frequencies (sort-by second) last first)

        ;; find the hour that the guard above was more frequently sleeping
        sleepy-freq (->> sleep-events
                         (filter (fn [[g e]] (= g sleepy-guard)))
                         (map second)
                         frequencies
                         (sort-by second)
                         last
                         first)

        sleepy-minute (->> sleepy-freq
                           (re-seq #"..:(.*)")
                           first
                           last
                           u/parse-int)]

    (* sleepy-guard sleepy-minute)))

(defn solve-2 []
  (let [sleep-events (sleep-minutes (process-input input))
        [guard ts]   (->> sleep-events
                          frequencies
                          (sort-by second)
                          last
                          first)
        minute       (->> ts
                          (re-seq #"..:(.*)")
                          first
                          last
                          u/parse-int)]
    (* guard minute)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
