(ns aoc.y2018.d14.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d14.data :refer [input answer-1 answer-2]]
   [clojure.test :as t :refer [is testing]])
  #?(:cljs (:require-macros [clojure.core :refer [definline]])))

(defn parse-char [c]
  (- (int c) 48))

(definline conj-board [board new-recipe]
  `(if (< 9 ~new-recipe)
     (-> ~board
         (conj! (quot ~new-recipe 10))
         (conj! (rem ~new-recipe 10)))
     (conj! ~board ~new-recipe)))

(defn solve-1 [input]
  (let [target-size (+ ^long input 10)]
    (loop [elf1 0 elf2 1 board (transient [3 7])]
      (let [^long elf1-recipe (board elf1)
            ^long elf2-recipe (board elf2)
            new-recipe (+ elf1-recipe elf2-recipe)
            new-board (conj-board board new-recipe)
            c (count new-board)]
        (if (> c target-size)
          (apply str (take 10 (drop input (persistent! new-board))))
          (recur (mod (+ 1 elf1 elf1-recipe) c)
                 (mod (+ 1 elf2 elf2-recipe) c)
                 new-board))))))

(definline check [board search start input-length]
  `(let [end# (+ ~input-length ^long ~start)]
     (loop [index# ^long ~start
            [x# & r#] ~search]
       (cond (= index# end#) true
             (not= (~board index#) x#) false
             :else (recur (inc index#) r#)))))

(defn solve-2 [input]
  (let [search-vec (map parse-char (str input))
        input-length (count search-vec)]
    (loop [unchecked-index 0
           elf1 0
           elf2 1
           board (transient [3 7])
           to-check '()]
      (if (not (empty? to-check))
        (if (check board search-vec (first to-check) input-length)
          (first to-check)
          (recur (inc unchecked-index) elf1 elf2 board (rest to-check)))
        (let [^long elf1-recipe (board elf1)
              ^long elf2-recipe (board elf2)
              new-recipe (+ elf1-recipe elf2-recipe)
              new-board (conj-board board new-recipe)
              c (count new-board)]
           (recur unchecked-index
                  (mod (+ 1 elf1 elf1-recipe) c)
                  (mod (+ 1 elf2 elf2-recipe) c)
                  new-board
                  (range unchecked-index (- (count new-board) input-length))))))))


(deftest part-1
  (is (= (str answer-1)
         (str (solve-1 input)))))

(deftest ^:skip-cljs ^:slow part-2
  (is (= (str answer-2)
         (str (solve-2 (str input))))))

;;;; Scratch

(comment
  (t/run-tests))

