(ns aoc.y2018.d24.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d24.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]
    [com.rpl.specter :refer [path MAP-VALS selected? setval pred= select select-one transform NONE]]))

(defn parse-additional [additional]
  (if additional
    (let [weak (re-find #"weak to ([^;)]+)" additional)
          immune (re-find #"immune to ([^;)]+)" additional)]
      {:weak   (if weak (into #{} (map str/trim (str/split (last weak) #","))) #{})
       :immune (if immune (into #{} (map str/trim (str/split (last immune) #","))) #{})})
    {:weak #{}
     :immune #{}}))


(def re #"(\d+) units each with (\d+) hit points (\([^)]+\) )?with an attack that does (\d+) ([a-z]+) damage at initiative (\d+)")

(defn parse-line [line]
  (let [[_ u hp add ap dmg-type ini] (re-matches re line)]
    (merge {:units (u/parse-int u)
            :hp (u/parse-int hp)
            :ap (u/parse-int ap)
            :dmg-type dmg-type
            :ini (u/parse-int ini)}
           (parse-additional add))))

(defn parse-input [input]
  (let [k (volatile! nil)
        id (volatile! -1)]
    (reduce (fn [m line]
              (cond (= "Immune System:" line) (do (vreset! k :immune-system) m)
                    (= "Infection:" line) (do (vreset! k :infection) m)
                    (str/blank? line) m
                    :else (do (vswap! id inc)
                              (assoc-in m [@k @id] (assoc (parse-line line)
                                                     :army @k :id @id)))))
            {:immune-system {} :infection {}}
            (str/split-lines input))))

(defn selection-order [groups]
  (sort-by (fn [{:keys [units ap ini]}] [(* units ap -1) (- ini)]) groups))

(defn potential-dmg [{:keys [units ap dmg-type]} {:keys [weak immune] :as enemy}]
  (let [modifier (cond (weak dmg-type) 2
                       (immune dmg-type) 0
                       :else 1)]
    (* units ap modifier)))

(defn target [attacker enemies]
  (:id (first (sort-by (fn [enemy] [(- (potential-dmg attacker enemy))
                                    (* -1 (:units enemy) (:ap enemy))
                                    (- (:ini enemy))])
                       (remove #(zero? (potential-dmg attacker %)) enemies)))))

(def other {:immune-system :infection
            :infection :immune-system})

(def ALL-GROUPS
  (path MAP-VALS MAP-VALS))

(defn group [id]
  (path ALL-GROUPS (selected? :id (pred= id))))

(defn target-map [state]
  (let [ordered (selection-order (select ALL-GROUPS state))]
    (loop [state state
           [current & more] ordered
           turn {}]
      (if (nil? current)
        turn
        (let [army (:army current)
              enemies (vals (state (other army)))
              target-id (or (target current enemies) :NONE)]
          (recur
            (setval [(other army) target-id] NONE state)
            more
            (assoc turn (:id current) target-id)))))))

(defn resolve-attack [attacker target]
  (let [dmg (potential-dmg attacker target)
        killed-units (quot dmg (:hp target))
        remaining-units (- (:units target) killed-units)]
    #_(println (:id attacker) "attacking" (:id target) "killing up to" killed-units)
    (if (<= remaining-units 0)
      NONE
      (assoc target :units remaining-units))))

(defn process-turn [state]
  #_(println (transform [MAP-VALS] #(reduce + (map :units (vals %))) state))
  (let [target-map (target-map state)
        attack-order (map :id (reverse (sort-by :ini (select ALL-GROUPS state))))]
    (loop [state state
           [current-id & more] attack-order]
      (if (nil? current-id)
        state
        (let [current (select-one (group current-id) state)
              target-id (target-map current-id)
              target (select-one (group target-id) state)]
          (recur
            (if (and current target)
              (transform (group target-id) (partial resolve-attack current) state)
              state)
            more))))))

(defn apply-boost [state boost]
  (transform [:immune-system MAP-VALS :ap] #(+ % boost) state))

(defn run [state]
  (u/fixed-point process-turn state))

(defn remaining-units [state]
  (reduce + (select [ALL-GROUPS :units] state)))

(defn solve-1 []
  (let [state (parse-input input)]
    (remaining-units (run state))))

(defn immune-system-won? [state]
  (empty? (:infection state)))

(defn find-indexed-fixpoint [state]
  ;we could do some binary search here, but this is not guaranteed to be successful, as the results might not be monotone
  ;so there maybe could be a draw after the immune-system has won with less boost
  (first (drop-while #(not (immune-system-won? %))
                     (map #(run (apply-boost state %)) (range)))))

(defn solve-2 []
  (remaining-units (find-indexed-fixpoint (parse-input input))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest ^:slow part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests))

