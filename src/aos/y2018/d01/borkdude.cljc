(ns aos.y2018.d01.borkdude
  (:require
   [aos.utils :as u :refer [deftest]]
   [aos.y2018.d01.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(def data
  (map (comp u/read-string
             #(str/replace % #"\+" ""))
       (str/split-lines input)))

(defn solve-1 []
  (apply + data))

(defn solve-2 []
  (reduce
   (fn [[seen sum] n]
     (let [new (+ sum n)]
       (if (seen new)
         (reduced new)
         [(conj seen new) new])))
   [#{0} 0] (cycle data)))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
