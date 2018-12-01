(ns aoc.y2017.d07.borkdude
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2017.d07.data :refer [input answer-1 answer-2]]
   [clojure.set :as set]
   [clojure.test :as t :refer [is testing]]
   [clojure.string :as str]))

(defn data
  []
  (into []
        (comp (map #(u/format "[%s]" %))
              (map u/read-string))
        (str/split-lines input)))

(defn parse
  [[name [weight]
    & [arrow & names]]]
  (-> {}
      (assoc :node {:name name
                    :weight weight})
      (assoc :ancestors (into {}
                              (for [n names]
                                [n name])))))

(defn parse-all
  [data]
  (reduce (fn [acc l]
            (let [{:keys [node ancestors]}
                  (parse l)]
              (-> acc
                  (update :nodes assoc (:name node) node)
                  (update :ancestors merge ancestors))))
          {:nodes {}
           :ancestors {}}
          data))

(defn solve-1* []
  (let [{:keys [nodes ancestors]} (parse-all (data))
        has-ancestor (keys ancestors)
        names (into #{} (map :name (vals nodes)))
        without-ancestors (set/difference names
                                          has-ancestor)]
    (first without-ancestors)))

(def solve-1 (memoize solve-1*))

(defn tree-weight
  [parent child-map nodes]
  (let [children (get child-map parent)
        child-weights (map #(tree-weight % child-map nodes)
                           children)]
    (when (seq child-weights)
      (when-not (apply = child-weights)
        (throw (ex-info (u/format "Weights not equal %s %s"
                                  parent
                                  (pr-str child-weights))
                        (zipmap children
                                child-weights)))))
    (apply + (:weight (get nodes parent))
           child-weights)))

(defn unbalanced-balanced
  [[a b c & nums]]
  (cond (= a b) [(some
                  #(when (not= a %) %)
                  (conj nums c))
                 a]
        (= a c) [b a]
        :else   [a b]))

(defn find-by-val
  [m v]
  (ffirst
   (filter (fn [[k v']]
             (= v v'))
           m)))

(defn child-map
  [ancestors]
  (let [children (group-by val ancestors)]
    (zipmap (keys children)
            (map #(map first %)
                 (vals children)))))

(defn solve-2
  ([] (solve-2 (solve-1)))
  ([p1]
   (let [{:keys [nodes ancestors]}
         (parse-all (data))
         cm (child-map ancestors)]
     (try (tree-weight p1 cm nodes)
          (catch #?(:cljs ExceptionInfo
                    :clj clojure.lang.ExceptionInfo) e
            (let [d (ex-data e)
                  [unbalanced balanced]
                  (unbalanced-balanced (vals d))
                  k (find-by-val d unbalanced)
                  diff (- balanced unbalanced)
                  w (:weight (get nodes k))]
              (+ w diff)))))))

(deftest part-1
  (is (= answer-1 (str (solve-1)))))

(deftest part-2
  (is (= answer-2 (solve-2))))

;;;; Scratch

(comment
  (t/run-tests))
