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
  {:current 0
   :left '()
   :right '()})

(defn switch-lr [{:keys [left right current]}]
  {:left right :right left :current current})

(defn shift-right [{:keys [left right current]}]
  (if (empty? right)
    (let [new-right (reverse (cons current left))]
      {:left '()
       :right  (rest new-right)
       :current (first new-right)})
    {:left (cons current left)
     :right (rest right)
     :current (first right)}))

(defn shift-left [circle]
  (switch-lr (shift-right (switch-lr circle))))

(defn add-entry [{:keys [left right current]} entry]
  {:right right
   :left (cons current left)
   :current entry})

(defn remove-current [{:keys [left right]}]
  {:right (rest right)
   :left left
   :current (first right)})

(defn shift [circle amount]
  (let [shift-fn (if (< 0 ^long amount)
                   shift-right
                   shift-left)]
    (first (drop (Math/abs ^long amount) (iterate shift-fn circle)))))

(defn insert-step [state marble]
  (let [current-player (mod (inc ^long (:last-player state))
                            (:players state))]
    (-> state
        (update :field #(-> %
                              (shift-right)
                              (add-entry marble)))
        (assoc :last-player current-player))))

(defn update-points [state current-player marble]
  (let [new-points (get-in state [:field :current])]
    (update-in state [:points current-player] #(+ ^long new-points ^long marble ^long %))))

(defn remove-step [state marble]
  (let [current-player (mod (inc ^long (:last-player state))
                            (:players state))]
    (-> state
        (update :field #(shift % -7))
        (update-points current-player marble)
        (update :field remove-current)
        (assoc :last-player current-player))))

(defn init-state [players]
  {:field (circle)
   :points (zipmap (range players) (repeat 0))
   :players players
   :last-player -1})

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