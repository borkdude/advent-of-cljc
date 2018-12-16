(ns aoc.y2018.d16.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d16.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]
    [com.rpl.specter :refer [setval transform ALL compact ALL MAP-KEYS MAP-VALS select not-selected? NONE]]))

(defn parse-instruction [inst]
  (mapv u/parse-int (re-seq #"\d+" inst)))

(defn parse-example [[before inst after]]
  {:before (read-string (subs before 8))
   :instruction (parse-instruction inst)
   :after (read-string (subs after 7))})

(defn parse-input [input]
  (->> (str/split-lines input)
       (partition-by str/blank?)
       (setval [ALL (compact ALL) str/blank?] NONE)
       (group-by count)
       (transform [MAP-KEYS] #(if (= 3 %) :examples :programm))
       (transform [:programm] first)
       (transform [:examples ALL] parse-example)
       (transform [:programm ALL] parse-instruction)))

(defn riop [f]
  (fn [reg [op a b c]]
    (setval [c] (f (reg a) b) reg)))

(defn irop [f]
  (fn [reg [op a b c]]
    (setval [c] (f a (reg b)) reg)))

(defn rop [f]
  (fn [reg [op a b c]]
    (setval [c] (f (reg a) (reg b)) reg)))

(def all-ops
  (letfn [(set-op [a b] a)
          (norm [x] (if x 1 0))
          (eq [a b] (norm (= a b)))
          (gt [a b] (norm (> a b)))]
         [(riop +) (rop +) (riop *) (rop *)
          (riop bit-and) (rop bit-and) (riop bit-or) (rop bit-or)
          (irop set-op) (rop set-op)
          (riop gt) (irop gt) (rop gt)
          (riop eq) (irop eq) (rop eq)]))

(defn fullfills-example? [{:keys [before instruction after]} op]
  (= after (op before instruction)))

(defn three-or-more? [example]
  (<= 3 (count (filter (partial fullfills-example? example) all-ops))))

(defn solve-1 []
  (count (select [:examples ALL three-or-more?] (parse-input input))))

(def op-codes
  (zipmap (range 16) (repeat all-ops)))

(defn test-example [op-codes example]
  (let [op-code (first (:instruction example))]
    (setval [op-code ALL (not-selected? (partial fullfills-example? example))] NONE op-codes)))

(defn shrink-opp-codes-step [op-codes]
  (let [known-opcodes (setval [MAP-VALS #(< 1 (count %))] NONE op-codes)
        known-ops (into #{} (select [MAP-VALS ALL] known-opcodes))]
    (merge (setval [MAP-VALS ALL known-ops] NONE op-codes)
           known-opcodes)))

(defn first-stable [seq]
  (first (first (drop-while (fn [[a b]] (not= a b)) (partition 2 1 seq)))))

(defn shrink-opp-codes [op-codes]
  (first-stable (iterate shrink-opp-codes-step op-codes)))

(defn execute-programm [op-codes programm]
  (reduce (fn [reg instruction]
            (let [op-code (first instruction)]
              ((op-codes op-code) reg instruction)))
          [0 0 0 0]
          programm))

(defn solve-2 []
  (let [{:keys [examples programm]} (parse-input input)
        op-codes (transform [MAP-VALS] first (shrink-opp-codes (reduce test-example op-codes examples)))]
    (first (execute-programm op-codes programm))))


(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests))

