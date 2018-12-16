(ns aoc.utils
  (:refer-clojure :exclude [time format read-string ExceptionInfo])
  (:require #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])
            [clojure.test]
            #?(:cljs [goog.string :as gstring])
            #?(:cljs [goog.string.format])
            [clojure.string :as str]
            #?(:cljs [oops.core :refer [ocall oget]]))
  #?(:cljs (:require-macros [aoc.utils :refer [deftest]])))

(defmacro deftime
  "Private. deftime macro from https://github.com/cgrand/macrovich"
  [& body]
  (when #?(:clj (not (:ns &env))
           :cljs (when-let [n (and *ns* (ns-name *ns*))]
                   (re-matches #".*\$macros" (name n))))
    `(do ~@body)))

(defn parse-int [s]
  #?(:clj (Integer/parseInt s)
     :cljs (js/parseInt s)))

(defn parse-float [s]
  #?(:clj (Float/parseFloat s)
     :cljs (js/parseFloat s)))

(def format #?(:clj clojure.core/format
               :cljs gstring/format))

(def read-string edn/read-string)

(defn count'
  "Like core count, but when applied to a directly reduceable coll, does not
  force the collection to be realized fully in memory."
  [coll]
  (transduce identity (completing (fn [c _] (inc c))) 0 coll))

(defn nth'
  "Like core nth, but when applied to a directly reduceable coll, does not
  force the collection to be realized fully in memory."
  ([coll n]
   (let [result (nth' coll n ::not-found)]
     (if (= result ::not-found)
       (throw (ex-info "Index out of bounds" {:n n}))
       result)))
  ([coll n not-found]
   (if (neg? n)
     not-found
     (transduce (drop n) (completing #(reduced %2)) not-found coll))))

(defn some'
  "Like core some, but when applied to a directly reduceable coll, does not
  force the collection to be realized fully in memory."
  [pred coll]
  (reduce (fn [_ x]
            (when (pred x)
              (reduced x)))
    nil
    coll))

(defn fixed-point [f x]
  "Calculates the fixed point of f with respect to x."
  (reduce #(if (= %1 %2) (reduced %1) %2)
    (iterate f x)))

(defn map-vals
  "Maps f over the values of a map m."
  [f m]
  (reduce-kv (fn [m k v] (assoc m k (f v))) {} m))

;; #?(:clj (defn free-memory []
;;           (let [rt (java.lang.Runtime/getRuntime)]
;;             (format "%.2f"
;;                     (/ (float (.freeMemory rt))
;;                        (* 1024 1024))))))

(defn write-file [path v]
  (let [line (str (pr-str v) "\n")]
    #?(:clj (spit path line :append true)
       :cljs (let [fs (js/require "fs")]
               (ocall fs "appendFileSync" path line)))))

(defn ci? []
  #?(:clj (= "true" (System/getenv "CI"))
     :cljs (= "true" (oget js/process "env.CI"))))

(deftime

  (defmacro ?
    "Private. case macro from https://github.com/cgrand/macrovich"
    [& {:keys [cljs clj]}]
    (if (contains? &env '&env)
      `(if (:ns ~'&env) ~cljs ~clj)
      (if #?(:clj (:ns &env) :cljs true)
        cljs
        clj)))

  (defmacro time
    [expr]
    (? :clj
       `(let [start# (. System (nanoTime))
              ret# ~expr]
          {:ret ret#
           :ms (/ (double (- (. System (nanoTime)) start#)) 1000000.0)})
       :cljs
       `(let [start# (cljs.core/system-time)
              ret# ~expr]
          {:ret ret#
           :ms (- (cljs.core/system-time) start#)})))

  (defmacro deftest [name & body]
    (let [ns (str (ns-name *ns*))
          test (str name)]
      `(clojure.test/deftest ~name
         (let [timed# (time (do ~@body))
               ret# (:ret timed#)
               ms# (int (:ms timed#))]
           (println '~name "took" ms# "msecs")
           (when (ci?)
             (let [[_# year# day# user#] (re-find #"y(\d{4})\.d(\d{2})\.(\w+)" ~ns)
                   score# {:year (parse-int year#)
                           :day (parse-int day#)
                           :user user#
                           :test ~test
                           :env (? :clj "jvm"
                                   :cljs "node")
                           :time ms#}]
               (write-file "out/scores.edn" score#))))))))

;;;; Scratch

(comment)


