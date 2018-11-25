(ns aos.y2017.d04
  (:require
   [aos.y2017.input :refer [input-d04] :rename {input-d04 input}]
   [clojure.string :as str]
   [clojure.test :as t :refer [deftest is testing]]
   [net.cgrand.xforms :as x]))

;;;;

(defn valid-passphrase?
  [phrase]
  (apply distinct? phrase))

(defn solution-ae7b7978-p1
  []
  (x/count
   (comp
    (map #(str/split % #"\s"))
    (filter valid-passphrase?))
   (str/split-lines input)))

(defn solution-ae7b7978-p2
  []
  (x/count
   (comp
    (map #(str/split % #"\s"))
    (map #(map sort %))
    (filter valid-passphrase?))
   (str/split-lines input)))

;;;;

(letfn [(valid-passphrase? [normalize passphrase]
          (->> (str/split passphrase #" ")
               (map normalize)
               (apply distinct?)))
        (solve [normalize]
          (->> (str/split-lines input)
               (filter (partial valid-passphrase? normalize))
               count))]
  (defn solution-ac129343-p1 []
    (solve identity))

  (defn solution-ac129343-p2 []
    (solve sort)))


;;;; Tests

(deftest p1-test
  (is (= 451 (solution-ae7b7978-p1)))
  (is (= 451 (solution-ac129343-p1)))
  )

(deftest p2-test
  (is (= 223 (solution-ae7b7978-p2)))
  (is (= 223 (solution-ac129343-p2)))
  )

;;;; Scratch

(comment
  (require '[speculative.instrument :refer [instrument]])
  (require '[patch.clj-2443])
  (instrument)
  (t/run-tests))
