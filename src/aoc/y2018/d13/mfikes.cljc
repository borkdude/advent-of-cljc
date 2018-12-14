(ns aoc.y2018.d13.mfikes
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format some']]
   [aoc.y2018.d13.data :refer [input answer-1 answer-2]]
   [clojure.string :as string]
   [clojure.test :as t :refer [is testing]]))

(def tracks #{\/ \\ \+})

(def dirs #{\v \^ \< \>})

(defn parse-lines [kind lines]
  (into {} (mapcat (fn [y line]
                     (keep-indexed (fn [x c]
                                     (when (kind c)
                                       [[x y] c]))
                       line))
             (range) lines)))

(def turns (cycle [:left :straight :right]))

(defn init-carts [loc->dir]
  (reduce-kv (fn [m loc dir]
               (assoc m loc [dir turns]))
    (sorted-map)
    loc->dir))

(defn update-loc [[x y] dir]
  (case dir
    \< [(dec x) y]
    \> [(inc x) y]
    \^ [x (dec y)]
    \v [x (inc y)]))

(defn move-cart [[_ [dir] :as cart]]
  (update cart 0 update-loc dir))

(defn rotate [dir turn]
  (case [dir turn]
    [\< :left] \v
    [\v :left] \>
    [\> :left] \^
    [\^ :left] \<
    [\< :right] \^
    [\v :right] \<
    [\> :right] \v
    [\^ :right] \>
    dir))

(defn update-dir+turns [[dir [turn & more :as turns]] track]
  (case [dir track]
    ([\> \\] [\< \/]) [\v turns]
    ([\> \/] [\< \\]) [\^ turns]
    ([\^ \\] [\v \/]) [\< turns]
    ([\^ \/] [\v \\]) [\> turns]
    ([\> \+] [\< \+] [\^ \+] [\v \+]) [(rotate dir turn) more]
    [dir turns]))

(defn turn-cart [loc->track [loc :as cart]]
  (update cart 1 update-dir+turns (loc->track loc)))

(defn update-carts-1 [loc->track carts]
  (reduce (fn [carts [old-loc :as cart]]
            (let [[new-loc :as cart] (turn-cart loc->track (move-cart cart))]
              (if (contains? carts new-loc)
                (reduced new-loc)
                (-> carts
                  (conj cart)
                  (dissoc old-loc)))))
    carts
    carts))

(defn solve [extract some-pred advance]
  (let [loc->track (parse-lines tracks (string/split-lines input))
        carts      (init-carts (parse-lines dirs (string/split-lines input)))]
    (string/join "," (extract (some' some-pred (iterate (partial advance loc->track) carts))))))

(defn solve-1 []
  (solve identity #(when (vector? %) %) update-carts-1))

(defn update-carts-2 [loc->track carts]
  (second (reduce (fn [[removed carts] [old-loc :as cart]]
                    (let [[new-loc :as cart] (turn-cart loc->track (move-cart cart))]
                      (cond
                        (removed old-loc)
                        [removed carts]

                        (contains? carts new-loc)
                        [(conj removed new-loc)
                         (-> carts
                           (dissoc new-loc)
                           (dissoc old-loc))]

                        :else
                        [removed
                         (-> carts
                           (conj cart)
                           (dissoc old-loc))])))
            [#{} carts]
            carts)))

(defn solve-2 []
  (solve ffirst #(when (= 1 (count %)) %) update-carts-2))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
;; comment added to retrigger build. can be removed next time
