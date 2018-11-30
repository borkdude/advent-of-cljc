(ns aos.utils
  (:refer-clojure :exclude [format read-string ExceptionInfo])
  (:require #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])
            [clojure.test]
            #?(:cljs [goog.string :as gstring])
            #?(:cljs [goog.string.format]))
  #?(:cljs (:require-macros [aos.utils :refer [deftest]])))

(defmacro deftime
  "Private. deftime macro from https://github.com/cgrand/macrovich"
  [& body]
  (when #?(:clj (not (:ns &env))
           :cljs (when-let [n (and *ns* (ns-name *ns*))]
                   (re-matches #".*\$macros" (name n))))
    `(do ~@body)))

(deftime
  (defmacro deftest [name & body]
    `(clojure.test/deftest ~name
       (let [time-str# (with-out-str (time (do ~@body)))
             time-str# (re-find #"\d+\.\d+ msecs" time-str#)]
         (println '~name "took" time-str#)))))

(defn parse-int [s]
  #?(:clj (Integer/parseInt s)
     :cljs (js/parseInt s)))

(def read-string edn/read-string)

(def format #?(:clj clojure.core/format
               :cljs gstring/format))

;;;; Scratch

(comment

  )
