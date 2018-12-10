(ns aoc.y2018.d07.mrmcc3
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d07.data :refer [input answer-1 answer-2]]
    [clojure.test :refer [is testing]]
    [clojure.string :as str]))

(defn parse-line [s]
  [(nth s 5) (nth s 36)])

(def conj-set (fnil conj #{}))

(defn dep [m [a b]]
  (update m a conj-set b))

(defn complete-job [{:keys [done todo pres posts] :as state} job]
  (let [post-job (get posts job)
        pres'    (reduce #(update %1 %2 disj job) pres post-job)
        avail    (filter (comp empty? pres') post-job)]
    (assoc state
      :done (conj done job)
      :todo (into (disj todo job) avail)
      :pres pres')))

(defn order [{:keys [done todo] :as state}]
  (if-let [job (first todo)]
    (recur (complete-job state job))
    (apply str done)))

(defn solve-1 []
  (let [pairs (map parse-line (str/split-lines input))
        pres  (reduce dep {} (map reverse pairs))
        posts (reduce dep {} pairs)
        todo  (into (sorted-set) (remove pres) (keys posts))]
    (order {:done [] :todo todo :pres pres :posts posts})))

(defn char-time [c]
  (- #?(:clj (int c) :cljs (.charCodeAt c 0)) 4))

(defn complete-jobs [{:keys [workers] :as state}]
  (reduce
    (fn [state {:keys [job t] :as worker}]
      (cond
        (nil? job) (update state :workers conj worker)
        (= t 1)
        (-> (complete-job state job)
            (update :workers conj (dissoc worker :job)))
        :else
        (update state :workers conj (update worker :t dec))))
    (assoc state :workers [])
    workers))

(defn assign-jobs [{:keys [workers] :as state}]
  (reduce
    (fn [{:keys [todo] :as state} worker]
      (let [idle?   (nil? (:job worker))
            new-job (first todo)]
        (if (and idle? new-job)
          (-> state
              (update :workers conj {:job new-job :t (char-time new-job)})
              (update :todo disj new-job))
          (update state :workers conj worker))))
    (assoc state :workers [])
    workers))

(defn distribute-jobs [{:keys [todo workers t] :as state}]
  (if (or (seq todo) (seq (filter :job workers)))
    (-> (complete-jobs state
                       )
        (assign-jobs)
        (update :t inc)
        (recur))
    (dec t)))

(defn solve-2 []
  (let [pairs   (map parse-line (str/split-lines input))
        posts   (reduce dep {} pairs)
        pres    (reduce dep {} (map reverse pairs))
        todo    (into (sorted-set) (remove pres) (keys posts))
        workers (repeat 5 {})]
    (distribute-jobs
      {:done [] :todo todo :workers workers :pres pres :posts posts :t 0})))

(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest part-2
         (is (= (str answer-2)
                (str (solve-2)))))
