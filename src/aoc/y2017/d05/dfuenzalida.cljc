(ns aoc.y2017.d05.dfuenzalida
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2017.d05.data :refer [input answer-1 answer-2]]
   [clojure.test :as t :refer [is testing]]))

(defn iter-offsets [pos offsets steps]
  (if (>= pos (count offsets))
    steps ;; we escaped!
    (let [curr    (offsets pos)
          new-pos (+ pos curr)]
      (recur new-pos
             (assoc-in offsets [pos] (inc curr))
             (inc steps)))))

(defn read-input []
  (read-string (str "[" input "]")))

(defn solve-1 []
  (iter-offsets 0 (read-input) 0))

(defn iter-offsets2 [pos offsets steps]
  (if (>= pos (count offsets))
    steps ;; we escaped!
    (let [curr    (offsets pos)
          new-pos (+ pos curr)
          change  (if (>= curr 3) dec inc)]
      (recur new-pos
             (assoc-in offsets [pos] (change curr))
             (inc steps)))))

(defn solve-2 []
  (iter-offsets2 0 (read-input) 0))

(deftest ^:skip part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest ^:skip part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
  )
