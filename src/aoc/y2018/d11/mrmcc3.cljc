(ns aoc.y2018.d11.mrmcc3
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d11.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]))

(defn digit [x]
  (mod (quot x 100) 10))

(defn power [subs [l t s :as square]]
  [square
   (cond

     ;; for size 1 squares calculate power directly
     (= s 1)
     (let [rack-id (+ l 10)]
       (-> (* rack-id t) (+ input) (* rack-id) (digit) (- 5)))

     ;; even sized square is covered by 4 sub squares
     ;; of half the size (previously computed)
     (even? s)
     (let [m  (bit-shift-right s 1)
           lm (+ l m) tm (+ t m)]
       (+ (subs [l t m]) (subs [lm t m])
          (subs [l tm m]) (subs [lm tm m])))

     ;; odd square is covered by 4 sub squares (previously computed)
     ;; minus the overlapping (size 1) square in the centre
     :else
     (let [m  (bit-shift-right s 1) m' (inc m)
           lm (+ l m) tm (+ t m)]
       (+ (subs [l t m'])
          (subs [lm tm m'])
          (subs [l (+ t m') m])
          (subs [(+ l m') t m])
          (- (subs [lm tm 1])))))])

(def pmap' #?(:clj pmap :cljs map))

(defn compute-stage [{:keys [hi stage] :as state} s]
  (let [i       (map inc (range (- 300 (dec s))))
        squares (for [x i y i] [x y s])
        powers  (pmap' (partial power state) squares)
        [hi-key' hi'] (apply max-key second powers)]
    (cond-> (into state powers)
            (and (or (nil? stage) (= stage s))
                 (< hi hi'))
            (assoc :hi hi' :hi-key hi-key'))))

(defn solve-1 []
  (->> (reduce
         compute-stage
         {:hi -10000 :stage 3}
         (map inc (range 3)))
       :hi-key
       (take 2)
       (str/join ",")))

(defn solve-2 []
  (->> (reduce
         compute-stage
         {:hi -10000}
         (map inc (range 300)))
       :hi-key
       (str/join ",")))

(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest ^:skip-cljs ^:slow part-2
         (is (= (str answer-2)
                (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
  (time (solve-1))
  (time (solve-2))
  )
