(ns aoc.y2018.d13.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d13.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]
    [clojure.set :as set]))

(defn slash [cart]
  (update cart :heading {:north :east
                         :east  :north
                         :south :west
                         :west  :south}))

(defn backslash [cart]
  (update cart :heading {:north :west
                         :west  :north
                         :south :east
                         :east  :south}))

(defn straight [cart] cart)

(def left-turn {:north :west
                :west  :south
                :south :east
                :east  :north})

(def right-turn (set/map-invert left-turn))

(def straight-turn identity)

(def next-turn {left-turn     straight-turn
                straight-turn right-turn
                right-turn    left-turn})

(defn intersection [{:keys [pending-turn] :as cart}]
  (-> cart
      (update :heading pending-turn)
      (update :pending-turn next-turn)))

(def parse-track-node {\/ slash
                       \\ backslash
                       \| straight
                       \- straight
                       \+ intersection
                       \> straight
                       \< straight
                       \v straight
                       \^ straight})

(def parse-cart-heading {\^ :north
                         \< :west
                         \v :south
                         \> :east})

(defn cart-at [loc heading]
  {:loc          loc
   :heading      heading
   :pending-turn left-turn})

(defn parse-cart [x y track-char]
  (when-let [heading (parse-cart-heading track-char)]
    (cart-at [x y] heading)))

(defn location-comparator-without-ties [cart1 cart2]
  (compare (:loc cart1)
           (:loc cart2)))

(defn map-indexed-2d [f colls]
  (map-indexed
    (fn [y coll]
      (map-indexed
        (fn [x e]
          (f x y e))
        coll))
    colls))

(defn parse [input]
  (let [input-lines (str/split-lines input)
        track (->> input-lines
                   (map-indexed-2d
                     (fn [x y track-char]
                       [[x y] (parse-track-node track-char)]))
                   (apply concat)
                   (filter #(nth % 1))
                   (into (sorted-map)))
        carts (->> input-lines
                   (map-indexed-2d parse-cart)
                   (apply concat)
                   (filter identity)
                   (into (sorted-set-by location-comparator-without-ties)))]
    {:track track
     :carts carts}))

(defn turn [track cart]
  (let [track-fn (track (:loc cart))]
    (track-fn cart)))

(def head {:north (fn [[x y]] [x (dec y)])
           :west  (fn [[x y]] [(dec x) y])
           :south (fn [[x y]] [x (inc y)])
           :east  (fn [[x y]] [(inc x) y])})

(defn move-one [{:keys [heading] :as cart}]
  (update cart :loc #((head heading) %)))

(defn step-cart [track cart]
  (let [turn' (partial turn track)]
    (->> cart
         turn'
         move-one)))

(defn step-carts [track carts]
  (reduce (fn [new-carts cart]
            (let [cart' (step-cart track cart)]
              (if (contains? new-carts cart')
                (reduced {:collision-at (:loc cart')})
                (-> new-carts
                    (disj cart)
                    (conj cart')))))
          carts carts))

(defn step-carts-with-removal [track carts]
  (reduce (fn [new-carts cart]
            (if (not (contains? new-carts cart))
              (disj new-carts cart)
              (let [cart' (step-cart track cart)]
                (if (contains? new-carts cart')
                  (-> new-carts
                      (disj cart cart'))
                  (-> new-carts
                      (disj cart)
                      (conj cart'))))))
          carts carts))

(defn solve-1 []
  (let [{:keys [carts track]} (parse input)
        step' (partial step-carts track)]
    (->> carts
         (iterate step')
         (some :collision-at)
         (map str)
         (str/join ","))))

(defn solve-2 []
  (let [{:keys [carts track]} (parse input)
        step' (partial step-carts-with-removal track)]
    (->> carts
         (iterate step')
         (filter #(<= (count %) 1))
         first
         first
         :loc
         (map str)
         (str/join ","))))

(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest part-2
         (is (= (str answer-2)
                (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)

  (let [{:keys [carts track]} (parse test-input2)
        step-with-removal' (partial step-carts-with-removal track)]
    (println test-input2)
    (println (first (filter
                      #(= 1 (count %))
                      (iterate (comp step-with-removal') carts)))))

  (let [{:keys [carts track]} (parse my-input)
        step' (partial step-carts-with-removal track)]
    (println (count carts))
    ;(println (count (nth (iterate step' carts)
    ;                     4000)))
    (println (map :loc (first (filter
                                #(<= (count %) 1)
                                (iterate step' carts))))))

  (disj (sorted-set-by location-comparator-without-ties
                       {:loc 1 :a 2})
        {:loc 1 :a 5}))
