(ns aos.y2017.d06.borkdude
  (:require
   [aos.utils :as u :refer [deftest]]
   [aos.y2017.d06.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as str]))

(defn data []
  (as-> input $
    (str/split $ #"\s")
    (mapv #(u/parse-int %) $)))

(defn first-max-pos
  [nums]
  (reduce (fn [[_ max-val :as m]
               [_ cur-val :as c]]
            (if (> cur-val max-val)
              c m))
          (map vector (range) nums)))

(defn next-pos
  [state cur-pos]
  (mod (inc cur-pos) (count state)))

(defn next-state
  [state]
  (let [[max-pos max-val] (first-max-pos state)
        dist-size (->
                   (/ max-val (count state))
                   Math/ceil
                   int)]
    (loop [state state
           pos (next-pos state max-pos)]
      (let [left (get state max-pos)]
        (if (or
             (zero? left)
             (= pos max-pos))
          state
          (recur
           (->
            state
            (update pos + dist-size)
            (update max-pos - dist-size))
           (next-pos state pos)))))))

(defn solve
  [data]
  (loop [states #{data}
         state data
         n 1]
    (let [state' (next-state state)]
      (if (contains? states state')
        [state' n]
        (recur (conj states state')
               state'
               (inc n))))))

(def solve* (solve (data)))

(defn solve-1 []
  (second solve*))

(defn solve-2 []
  (second (solve (first solve*))))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
