(ns aoc.y2018.d05.mrmcc3
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(defn reacts? [a b]
  (and a b (not= a b) (= (str/lower-case a) (str/lower-case b))))

(defn react [stack char]
  (if (reacts? (peek stack) char)
    (pop stack)
    (conj stack char)))

(defn solve-1 []
  (count (reduce react [] input)))

(defn remove+react [polymer char]
  (let [re (re-pattern (str "(?i)" char))]
    (count (reduce react [] (str/replace polymer re "")))))

(defn solve-2 []
  (let [alphabet "abcdefghijklmnopqrstuvwxyz"
        map'     #?(:clj pmap :cljs map)]
    (apply min (map' (partial remove+react input) alphabet))))

(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest part-2
         (is (= (str answer-2)
                (str (solve-2)))))

