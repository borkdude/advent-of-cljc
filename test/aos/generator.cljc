(ns aos.generator
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]))

(def syms
  '[aos.y2017.d01
    aos.y2017.d02
    aos.y2017.d03
    aos.y2017.d04])

(defn ns-form [problems]
  (let [require-syms problems]
    `(~'ns speculative.coal-mine-runner
      (:require
       ~@require-syms
       ~(reader-conditional '(:clj patch.clj-2443) false)
       [speculative.instrument :refer [~'instrument]]
       [clojure.test]))))

(defn run-tests-form [problems]
  (let [syms (map #(list 'quote %) problems)]
    `(defn ~'run-tests []
       (println "Instrumenting with speculative specs")
       (~'instrument)
       (println "Running tests for" ~@syms)
       (clojure.test/run-tests ~@syms))))

(defn run-form []
  `(do
     (time (~'run-tests))
     ~(reader-conditional '(:clj (shutdown-agents)) false)))

(defn emit-program [out problems]
  (spit out (str (with-out-str (pprint (ns-form problems))) "\n"))
  (spit out (str (with-out-str (pprint (run-tests-form problems))) "\n")
        :append true)
  (spit out (str (with-out-str (pprint (run-form))) "\n")
        :append true))

(defn -main [& [out op max-or-nth]]
  (let [max-or-n #?(:clj (Integer/parseInt max-or-nth)
                    :cljs (js/parseInt max-or-nth))
        problems (case op
                   "nth"
                   [(nth syms (dec max-or-n))]
                   "random"
                   (sort (take max-or-n (shuffle syms))))]
    (io/make-parents out)
    (emit-program out problems)))
