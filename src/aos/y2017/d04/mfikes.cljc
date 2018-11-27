(ns aos.y2017.d04.mfikes
  (:require
   [aos.utils :as u :refer [deftest]]
   [aos.y2017.d04.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :as t :refer [is testing]]))

(defn valid-passphrase? [normalize passphrase]
  (->> (str/split passphrase #" ")
       (map normalize)
       (apply distinct?)))

(defn solve [normalize]
  (->> (str/split-lines input)
       (filter (partial valid-passphrase? normalize))
       count))

(deftest part-1
  (is (= answer-1 (solve identity))))

(deftest part-2
  (is (= answer-2 (solve sort))))
