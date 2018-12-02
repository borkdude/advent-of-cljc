(ns aoc.y2018.d01.ericstewart
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   #?(:cljs [cljs.reader :as reader]))
  )

(defn read-input []
  (->> (clojure.string/split-lines input)
       #?(:clj (mapv read-string)
          :cljs (mapv reader/read-string))))

(defn solve-1 []
(->> (read-input) 
     (reduce + 0)))

(defn solve-2 []
(->> (read-input) 
     cycle
     (reduce (fn [c e]
               (let [nextval (+ e (:total c))]
                 (if (get-in c [:seen nextval])
                   (reduced nextval)
                  {:total nextval
                   :seen (conj (:seen c) nextval)}))) 
             {:total 0
              :seen #{}})))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
