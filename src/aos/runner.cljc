(ns aos.runner
  (:require
   [clojure.test :as t]
   [speculative.instrument :refer [instrument]]
   [aos.y2017.d01]))

(defn -main [& args]
  (t/run-tests 'aos.y2017.d01))

#?(:cljs (set! *main-cli-fn* -main))
