(ns aoc.y2018.d13.mrmcc3
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d13.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]))

;; parse track and carts as initial state

(def carts [\> \^ \< \v])

(defn parse-char [{:keys [x y] :as state} char]
  (let [track (case char \space nil \> \- \< \- \^ \| \v \| char)
        cart  (get (set carts) char)]
    (cond->
      state
      track (assoc-in [:track [y x]] track)
      cart (assoc-in [:carts [y x]] {:dir cart :on track :last \r}))))

(defn parse-loc [state char]
  (update (parse-char state char) :x inc))

(defn parse-row [state line]
  (update (reduce parse-loc (assoc state :x 0) line) :y inc))

(def initial-state
  (delay (reduce parse-row {:y 0 :carts (sorted-map)}
                 (str/split-lines input))))

;; handle cart movement

(defn shift [[y x] dir]
  (case dir \> [y (inc x)] \< [y (dec x)]
            \^ [(dec y) x] \v [(inc y) x]))

(defn move [{:keys [track]} [loc cart] {:keys [dir] :as m}]
  (let [loc'  (shift loc dir)
        cart' (assoc m :on (track loc'))]
    [loc' (merge cart cart')]))

(defn next-turn [dir last]
  (let [next (case last \r \l \l \s \s \r)
        n    (case next \l 1 \r 3 0)]
    {:last next
     :dir  (->> (cycle carts)
                (drop-while #(not= % dir))
                (drop n) first)}))

(defn move-cart [state [_ {:keys [dir on last]} :as cart]]
  (let [cmd (if (or (= on \-) (= on \|))
              {:dir dir} ;; stay on track
              (case [dir on]

                ;; handle corner cases
                [\^ \/] {:dir \>} [\^ \\] {:dir \<}
                [\v \/] {:dir \<} [\v \\] {:dir \>}
                [\> \/] {:dir \^} [\> \\] {:dir \v}
                [\< \/] {:dir \v} [\< \\] {:dir \^}

                ;; handle intersection
                (next-turn dir last)))]
    (move state cart cmd)))

;; handle cart collisions

(defn update-cart [{:keys [carts] :as state} [loc _ :as cart]]
  (let [[loc' cart'] (move-cart state cart)]
    (cond

      ;; another cart has collided and removed this cart. ignore
      (not (contains? carts loc))
      state

      ;; collision with another cart. remove both
      (contains? carts loc')
      (-> (update state :carts dissoc loc loc')
          (assoc :collision loc')) ;; part-1

      :else ;; no collision move the cart
      (-> (update state :carts dissoc loc)
          (update :carts assoc loc' cart')))))

;; simulate carts

(defn solve-1 []
  (reduce
    (fn [{:keys [carts collision] :as state} _]
      (if-let [[y x] collision]
        (reduced (str x "," y))
        (reduce update-cart state carts)))
    @initial-state
    (range)))


(defn solve-2 []
  (reduce
    (fn [{:keys [carts] :as state} _]
      (if (= (count carts) 1)
        (let [[[[y x]]] (seq carts)]
          (reduced (str x "," y)))
        (reduce update-cart state carts)))
    @initial-state
    (range)))

(deftest part-1 (is (= (str answer-1) (str (solve-1)))))
(deftest part-2 (is (= (str answer-2) (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
  (time (solve-1))
  (time (solve-2))
  )
