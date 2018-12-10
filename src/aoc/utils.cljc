(ns aoc.utils
  (:refer-clojure :exclude [time format read-string ExceptionInfo])
  (:require #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])
            [clojure.test]
            #?(:cljs [goog.string :as gstring])
            #?(:cljs [goog.string.format])
            #?(:cljs [goog.object])
            #?(:cljs [oops.core]))
  #?(:cljs (:require-macros [aoc.utils :refer [deftest gc heap-used]]
                            [oops.core :refer [ocall oget]])))

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

  (defmacro gc []
    (? :cljs `(js/gc)))

  (defmacro heap-used []
    (? :clj 0 ;; TODO
       :cljs
       `(let [mem-usage# (oops.core/ocall js/process "memoryUsage")]
          (js/Math.round
           (/ (oops.core/oget mem-usage# "heapUsed")
              (* 1024 1024))))))

  (defmacro deftest [name & body]
    `(clojure.test/deftest ~name
       (gc)
       (let [heap-start# (heap-used)
             timed# (time (do ~@body))
             _# (gc)
             heap-end# (heap-used)
             ret# (:ret timed#)
             ms# (:ms timed#)
             ms# (format "%.2f" ms#)]
         (println '~name "took" ms# "msecs")
         (? :cljs
            (println "Heap used" (- heap-end# heap-start#)))))))

;;;; Scratch

(comment)
