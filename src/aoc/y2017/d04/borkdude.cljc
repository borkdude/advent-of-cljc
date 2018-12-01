(ns aoc.y2017.d04.borkdude
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2017.d04.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :as t :refer [is testing]]
   [net.cgrand.xforms :as x]))

(defn valid-passphrase?
  [phrase]
  (apply distinct? phrase))

(deftest part-1
  (is (= answer-1
         (x/count
          (comp
           (map #(str/split % #"\s"))
           (filter valid-passphrase?))
          (str/split-lines input)))))

(deftest part-2
  (is (= answer-2
         (x/count
          (comp
           (map #(str/split % #"\s"))
           (map #(map sort %))
           (filter valid-passphrase?))
          (str/split-lines input)))))
