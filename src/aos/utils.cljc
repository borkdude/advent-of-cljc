(ns aos.utils
  (:refer-clojure :exclude [read-string])
  (:require #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])))

(defn parse-int [s]
  #?(:clj (Integer/parseInt s)
     :cljs (js/parseInt s)))

(def read-string edn/read-string)

;;;; Scratch

(comment

  )
