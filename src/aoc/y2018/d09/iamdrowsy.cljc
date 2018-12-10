(ns aoc.y2018.d09.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d09.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

#?(:clj (set! *warn-on-reflection* true))
#?(:clj (set! *unchecked-math* :warn-on-boxed))

(defn parse-input []
  (zipmap [:players :marbles]
          (re-seq #"\d+" input)))

(defn circle []
  (transient
    {:current 0
     :left    '()
     :right   '()}))

(defn shift-left [{:keys [left right current] :as circle}]
  (if (empty? left)
    (let [new-left (reverse (cons current right))]
      (-> circle
          (assoc! :right '()
                 :current (first new-left)
                 :left (rest new-left))))
    (-> circle
        (assoc! :left (rest left))
        (assoc! :right (cons current right)
               :current (first left)))))

(defn shift-and-add [{:keys [left right current] :as circle} entry]
  (if (empty? right)
    (let [new-right (reverse (cons current left))]
      (-> circle
          (assoc! :left (list (first new-right))
                  :current entry
                  :right (rest new-right))))
    (-> circle
        (assoc! :right (rest right))
        (assoc! :left (cons (first right) (cons current left))
                :current entry))))

(defn add-entry [{:keys [left current] :as circle} entry]
  (assoc! circle :left (cons current left) :current entry))

(defn remove-current [{:keys [right] :as circle}]
  (-> circle
      (assoc! :current (first right)
             :right (rest right))))

(defn shift-7 [circle]
    (first (drop 7 (iterate shift-left circle))))

(defn insert-step [state marble]
  (let [current-player (mod (inc ^long (:last-player state))
                            (:players state))]
    (-> state
        (assoc! :circle (shift-and-add (:circle state) marble))
        (assoc! :last-player current-player))))

(defn update-points [state current-player marble]
  (let [new-points (get-in state [:circle :current])]
    (assoc! state :points
            (update (:points state) current-player #(+ ^long new-points ^long marble ^long %)))))

(defn remove-step [state marble]
  (let [current-player (mod (inc ^long (:last-player state))
                            (:players state))]
    (-> state
        (assoc! :circle (shift-7 (:circle state)))
        (update-points current-player marble)
        (assoc! :circle (remove-current (:circle state)))
        (assoc! :last-player current-player))))

(defn init-state [players]
  (transient
    {:circle      (circle)
     :points      (zipmap (range players) (repeat 0))
     :players     players
     :last-player -1}))

(defn play-step [state marble]
  (if (zero? ^long (mod ^long marble 23))
    (remove-step state marble)
    (insert-step state marble)))

(defn max-points [{:keys [points]}]
  (apply max (vals points)))

(defn run-game [players marbles]
  (max-points
    (reduce play-step
            (init-state players)
            (range 1 (inc ^long marbles)))))

(defn solve-1 []
  (let [{:keys [players marbles]} (parse-input)]
    (run-game (u/parse-int players)
              (u/parse-int marbles))))

(defn solve-2 []
  (let [{:keys [players marbles]} (parse-input)]
    (run-game (u/parse-int players)
              (* 100 ^long (u/parse-int marbles)))))


(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
