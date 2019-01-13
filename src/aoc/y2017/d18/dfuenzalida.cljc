(ns aoc.y2017.d18.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d18.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn read-input []
  (s/split-lines input))

(defn long-or-str [s] ;; either parses a number or returns the input
  (if-let [numlike (re-seq #"[-]?\d+" s)]
    (->> numlike first read-string)
    s))

(defn parse-line [s]
  (let [[c r v] (into [] (s/split s #" "))]
    [c (long-or-str r) (when v (long-or-str v))]))

(defn run-program [prog i state freq]
  (if (or (neg? i) (>= i (count prog) i))
    [freq state]
    (let [[cmd reg val] (get prog i)]
      (condp = cmd
        "snd" (let [f (get state reg 0)
                    f (if (zero? f) freq f)]
                (recur prog (inc i) state f))
        "set" (recur prog (inc i)
                     (assoc state reg (get state val val)) freq)
        "add" (recur prog (inc i)
                     (assoc state reg (+ (get state reg 0) (get state val val))) freq)
        "mul" (recur prog (inc i)
                     (assoc state reg (* (get state reg 0) (get state val val))) freq)
        "mod" (recur prog (inc i)
                     (assoc state reg (mod (get state reg) (get state val val))) freq)
        "jgz" (let [regv (get state reg 0)]
                (recur prog (+ i (if (pos? regv) (get state val val) 1)) state freq))
        "rcv" (let [f (get state reg 0)]
                (if (zero? f)
                  (recur prog (inc i) state freq)
                  [freq state]))
        (recur prog (inc i) state freq)))))

(defn solve-1 []
  (first (run-program (mapv parse-line (read-input)) 0 {} 0)))

(defn next-state [state reg val f]
  (assoc state reg (f (get state reg 0)
                      (get state val val))))
  
(defn step-program [prog ip state queue] ;; returns [ip state sent queue running?]
  (if (or (neg? ip) (>= ip (count prog)))
    [ip state nil queue false]
    (let [[op reg val] (get prog ip)]
      (condp = op
        "set" [(inc ip) (next-state state reg val (comp second vector)) nil queue true]
        "add" [(inc ip) (next-state state reg val +) nil queue true]
        "mul" [(inc ip) (next-state state reg val *) nil queue true]
        "mod" [(inc ip) (next-state state reg val mod) nil queue true]
        "jgz" (let [regv (get state reg reg)]
                [(+ ip (if (pos? regv) (get state val val) 1)) state nil queue true])

        "snd" [(inc ip) state (get state reg reg) queue true]

        "rcv" (if (empty? queue)
                [ip state nil [] false]
                [(inc ip)
                 (assoc state reg (first queue))
                 nil
                 (into [] (rest queue)) true])))))

(defn run-programs [prog s0 i0 q0 s1 i1 q1 nsends0 nsends1]
  (let [[i0' st0' sent0 q0' run0] (step-program prog i0 s0 q0)
        [i1' st1' sent1 q1' run1] (step-program prog i1 s1 q1)]
    (if (= [false false] [run0 run1])
      nsends1 ;; "how many times did program 1 send a value?"
      (recur
       prog
       st0' i0' (if sent1 (conj q0' sent1) q0')
       st1' i1' (if sent0 (conj q1' sent0) q1')
       (if sent0 (inc nsends0) nsends0)
       (if sent1 (inc nsends1) nsends1)))))

(defn solve-2 []
  (let [input-program (mapv parse-line (read-input))]
    (run-programs input-program {"p" 0 :p 0} 0 [] {"p" 1 :p 1} 0 [] 0 0)))

(deftest part-1
  (is (= (str answer-1) ;; => 4601
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
)
