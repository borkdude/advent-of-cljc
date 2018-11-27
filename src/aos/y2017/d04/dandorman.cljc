(ns aos.y2017.d04.dandorman
  (:require
   [aos.utils :as u :refer [deftest]]
   [aos.y2017.d04.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is]]))

(defn valid-passphrase? [s]
  (let [words (str/split s #"\s+")]
    (= (count words) (count (into #{} words)))))

(defn strict-passphrase? [s]
  (let [words    (str/split s #"\s+")
        charsets (into #{} (map frequencies words))]
    (= (count words) (count charsets))))

(deftest part-1
  (is (= answer-1 (->> (str/split input #"\n")
                       (filter valid-passphrase?)
                       count))))

(deftest part-2
  (is (= answer-2 (->> (str/split input #"\n")
                       (filter strict-passphrase?)
                       count))))
