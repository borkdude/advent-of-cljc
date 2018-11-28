(ns aos.new
  (:require
   [clojure.java.io :as io]))

(defn data-ns [year day]
  (format "(ns aos.y%s.d%s.data)

(def input)

(def answer-1)

(def answer-2)
" year day))

(defn user-ns [year day user]
  (format "(ns aos.y%s.d%s.%s)
  (:require
   [aos.utils :as u :refer [deftest]]
   [aos.y%s.d%s.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]])

(deftest part-1
  (is true))

(deftest part-2
  (is true))
" year day user year day))

(defn emit-program [out year day user]
  )

(defn -main [& [year day user]]
  (let [data-out (io/file "src" "aos"
                          (str "y" year)
                          (str "d" day)
                          "data.cljc")
        out (io/file "src" "aos"
                     (str "y" year)
                     (str "d" day)
                     (str user ".cljc"))]
    (io/make-parents out)
    (when (not (.exists data-out))
      (spit data-out (data-ns year day)))
    (spit out (user-ns year day user))))
