(ns aos.y2017.d01
  (:require
   [aos.y2017.input :refer [input-d01] :rename {input-d01 input}]
   [aos.utils :as u]
   [clojure.test :refer [deftest is testing]]
   [clojure.string :as str]))

;;;;

(defn solution-cf1083da-p1 []
  (reduce
   +
   (map
    (fn [a b]
      (if (= a b)
        (u/parse-int (str a)) 0))
    input
    (drop 1 (cycle input)))))

(defn solution-cf1083da-p2 []
  (let [half (/ (count input) 2)]
    (reduce
     +
     (map
      (fn [a b]
        (if (= a b)
          (u/parse-int (str a)) 0))
      input
      (drop half (cycle input))))))

;;;;

(let [c->d (zipmap "0123456789" (range))]
  (defn str->digits
    [s]
    (map c->d s)))

(def data (-> input str/trim str->digits))

(defn matches [xs ys]
  (->>
   (map vector xs ys)
   (filter (partial apply =))
   (map first)))

(defn solve [pair-up]
  (apply + (matches data (pair-up data))))

(defn solution-c45758a0-p1 []
  (solve #(rest (cycle %))))

(defn solution-c45758a0-p2 []
  (solve #(nthrest (cycle %) (/ (count %) 2))))

;;;;

(defn char-code [char]
  #?(:clj (int char)
     :cljs (.charCodeAt char 0)))

(defn- cs->nums [cs]
  (map #(- (char-code %) (char-code \0)) cs))

(defn solution-cc6e3478-p1 []
  (let [cs input
        ns (cs->nums cs)
        c (first ns)]
    (->> (concat ns [c])
         (partition 2 1)
         (filter (fn [[x y]] (= x y)))
         (map first)
         (apply +))))

(defn solution-cc6e3478-p2 []
  (let [cs input
        len (count cs)
        ns (cycle (cs->nums cs))]
    (->> (map list ns (drop (quot len 2) ns))
         (take len)
         (filter (fn [[x y]] (= x y)))
         (map first)
         (apply +))))

;;;; Tests

(deftest aos-y2017-d01-01-test
  (is (= 995 (solution-cf1083da-p1)))
  (is (= 995 (solution-c45758a0-p1)))
  (is (= 995 (solution-cc6e3478-p1))))

(deftest aos-y2017-d01-02-test
  (is (= 1130 (solution-cf1083da-p2)))
  (is (= 1130 (solution-c45758a0-p2)))
  (is (= 1130 (solution-cc6e3478-p2))))
