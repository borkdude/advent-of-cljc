(ns aoc.y2018.d02.iamdrowsy
  (:require
    [aoc.utils :as u :refer [deftest]]
    [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]
    [com.rpl.specter :as s]))

(defn letter-frequencies [s]
  (into #{} (vals (frequencies s))))

(defn solve-1 []
  (->> (str/split-lines input)
       (mapcat letter-frequencies)
       (frequencies)
       (#(* (% 2) (% 3)))))

(def STRING-SEQ
  (s/parser seq #(apply str %)))

(defn different-letters [base-word]
  (let [index+char-in-base (set (s/select s/INDEXED-VALS base-word))
        differs? (fn [index+char] (not (index+char-in-base index+char)))]
    (s/path STRING-SEQ s/INDEXED-VALS differs? s/LAST)))

(defn distance [s1 s2]
  (count (s/select (different-letters s1) s2)))

(defn remove-different [s1 s2]
  (s/setval (different-letters s1) s/NONE s2))

(defn solve-2 []
  (first
    (let [codes (into [] (str/split-lines input))]
      (for [i (range (count codes))
            j (range (inc i) (count codes))
            :let [d (distance (codes i) (codes j))]
            :when (= d 1)]
        (remove-different (codes i) (codes j))))))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
