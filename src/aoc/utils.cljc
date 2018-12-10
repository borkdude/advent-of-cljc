(ns aoc.utils
  (:refer-clojure :exclude [time format read-string ExceptionInfo])
  (:require #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])
            [clojure.test]
            #?(:cljs [goog.string :as gstring])
            #?(:cljs [goog.string.format])
            [clojure.string :as str])
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

;; #?(:clj (defn free-memory []
;;           (let [rt (java.lang.Runtime/getRuntime)]
;;             (format "%.2f"
;;                     (/ (float (.freeMemory rt))
;;                        (* 1024 1024))))))

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
           :ms (.toFixed (- (cljs.core/system-time) start#) 6)})))

  (defmacro deftest [name & body]
    `(clojure.test/deftest ~name
       (let [timed# (time (do ~@body))
             ret# (:ret timed#)
             ms# (:ms timed#)
             ms# (format "%.2f" ms#)]
         (println '~name "took" ms# "msecs")))))

;;;; Scratch

(comment)


