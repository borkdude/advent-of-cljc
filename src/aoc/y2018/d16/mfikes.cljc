(ns aoc.y2018.d16.mfikes
  (:refer-clojure :exclude [read-string format defmacro])
  (:require
   [aoc.utils :as u :refer [deftest read-string format map-vals fixed-point]]
   [aoc.y2018.d16.data :refer [input answer-1 answer-2]]
   [chivorcam.core :refer [defmacro]]
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.test :as t :refer [is testing]]))

(defn oprr [op registers a b c]
  (assoc registers c (op (registers a) (registers b))))

(defn opri [op registers a b c]
  (assoc registers c (op (registers a) b)))

(defn opir [op registers a b c]
  (assoc registers c (op a (registers b))))

(defmacro defmicrocode [opcode n op]
  `(defmethod ~'microcode ~opcode ~'[_ registers a b c]
     (~(case (take-last n (name opcode))
         [\r]    `oprr
         [\i]    `opri
         [\r \r] `oprr
         [\r \i] `opri
         [\i \r] `opir)
      ~op ~'registers ~'a ~'b ~'c)))

(defmulti microcode (fn [opcode registers a b c] opcode))

(defn bool->num [b]
  (if b 1 0))

(defmicrocode :addr 1 +)
(defmicrocode :addi 1 +)
(defmicrocode :mulr 1 *)
(defmicrocode :muli 1 *)
(defmicrocode :banr 1 bit-and)
(defmicrocode :bani 1 bit-and)
(defmicrocode :borr 1 bit-or)
(defmicrocode :bori 1 bit-or)
(defmicrocode :setr 1 (fn [a b] a))
(defmicrocode :setir 2 (fn [a b] a))
(defmicrocode :gtir 2 (comp bool->num >))
(defmicrocode :gtri 2 (comp bool->num >))
(defmicrocode :gtrr 2 (comp bool->num >))
(defmicrocode :eqir 2 (comp bool->num =))
(defmicrocode :eqri 2 (comp bool->num =))
(defmicrocode :eqrr 2 (comp bool->num =))

(def opcodes (keys (methods microcode)))

(defn parse-examples [input]
  (->> (re-seq #"(?m)Before: \[(\d), (\d), (\d), (\d)\]\n(\d+) (\d) (\d) (\d)\nAfter:  \[(\d), (\d), (\d), (\d)\]\n" input)
    (map #(map read-string (rest %)))
    (map #(zipmap [:before :instr :after] (partition 4 %)))))

(defn opcode-num+matching-opcodes [{:keys [before instr after]}]
  (let [[opcode-num a b c] instr]
    [opcode-num (set (filter (fn [opcode]
                               (= after (microcode opcode (vec before) a b c)))
                       opcodes))]))

(defn solve-1 []
  (->> (parse-examples input)
    (map opcode-num+matching-opcodes)
    (filter #(<= 3 (count (second %))))
    count))

(defn subtract-singletons [m]
  (let [singleton?     #(= 1 (count %))
        singleton-vals (into #{} (map first (filter singleton? (vals m))))]
    (reduce-kv (fn [m k v]
                 (if (singleton? v)
                   (assoc m k v)
                   (assoc m k (set/difference v singleton-vals))))
      {}
      m)))

(defn solve-2 []
  (let [examples            (parse-examples input)
        opcode-num->opcodes (->> examples
                              (map opcode-num+matching-opcodes)
                              (map #(apply hash-map %))
                              (apply merge-with set/intersection))
        opcode-num->opcode  (->> opcode-num->opcodes
                              (fixed-point subtract-singletons)
                              (map-vals first))
        program             (->> (string/split-lines input)
                              (drop (+ (* 4 (count examples)) 2))
                              (map #(map read-string (re-seq #"\d+" %))))]
    (first (reduce (fn [registers [numeric-opcode a b c]]
                     (microcode (opcode-num->opcode numeric-opcode) registers a b c))
             [0 0 0 0]
             program))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
