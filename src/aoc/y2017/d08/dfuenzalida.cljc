(ns aoc.y2017.d08.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d08.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn parse-line [s]
  (let [[reg op amt _ reg2 cmp cmp-val] (s/split s #" ")]
    [reg op (read-string amt) reg2 cmp (read-string cmp-val)]))

(def fns
  {"inc" + "dec" - "<" < "<=" <= "==" = "!=" not= ">=" >= ">" >})

(defn process [lines m]
  (if (seq lines)
    (let [[reg op amt reg2 cmp cmp-val] (parse-line (first lines))]
      (if ((fns cmp) (get m reg2 0) cmp-val)
        (recur (rest lines)
               (assoc m reg ((fns op) (get m reg 0) amt)))
        (recur (rest lines) m)))
    m))

(defn read-input []
  (s/split-lines input))

(defn solve-1 []
  (apply max (vals (process (read-input) {}))))

(defn process2 [lines m max-val]
  (if (seq lines)
    (let [[reg op amt reg2 cmp cmp-val] (parse-line (first lines))]
      (if ((fns cmp) (get m reg2 0) cmp-val)
        (let [new-val ((fns op) (get m reg 0) amt)]
          (recur (rest lines)
                 (assoc m reg new-val)
                 (max new-val max-val)))
        (recur (rest lines) m max-val)))
    [m max-val]))

(defn solve-2 []
  (last (process2 (read-input) {} 0)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
)
