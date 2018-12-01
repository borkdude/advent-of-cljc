(ns aoc.y2018.d01.dandorman
  (:require
   [aoc.utils :refer [deftest]]
   [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
   [#?(:clj clojure.edn, :cljs cljs.tools.reader.edn) :as edn]
   [clojure.test :refer [is testing]]))

(defn input->vec [input]
  (edn/read-string (str "[" input "]")))

(def solve-1 (partial reduce +))

(defn solve-2 [numbers]
  (reduce (fn [[freqs freq] n]
            (let [new-freq (+ freq n)]
              (if (contains? freqs new-freq)
                (reduced new-freq)
                [(conj! freqs new-freq) new-freq])))
          [(transient #{}) 0]
          (cycle numbers)))

(deftest part-1
  (is (= answer-1 (solve-1 (input->vec input)))))

(deftest part-2
  (is (= answer-2 (solve-2 (input->vec input)))))
