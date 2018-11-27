(ns aos.utils
  (:refer-clojure :exclude [read-string])
  (:require #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])
            [clojure.test])
  #?(:cljs (:require-macros [aos.utils :refer [deftest]])))

(defmacro deftest [name & body]
  `(clojure.test/deftest ~name
     (let [time-str# (with-out-str (time ~@body))
           time-str# (re-find #"\d+\.\d+ msecs" time-str#)]
       (println '~name "took" time-str#))))

(defn parse-int [s]
  #?(:clj (Integer/parseInt s)
     :cljs (js/parseInt s)))

(def read-string edn/read-string)

;;;; Scratch

(comment

  )
