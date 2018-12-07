(ns aoc.new
  (:require
   [clojure.java.io :as io]
   [clojure.tools.cli :refer [parse-opts]]))

(defn data-ns [year day]
  (format "(ns aoc.y%s.d%s.data)

(def input)

(def answer-1)

(def answer-2)
" year day))

(defn user-ns [year day user]
  (format "(ns aoc.y%s.d%s.%s
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y%s.d%s.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

(defn solve-1 []
  ;; TODO
)

(defn solve-2 []
  ;; TODO
)

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))
" year day user year day))

(defn create-new [{:keys [year day user]}]
  (let [day (format "%02d" day)
        data-out (io/file "src" "aoc"
                          (str "y" year)
                          (str "d" day)
                          "data.cljc")
        out (io/file "src" "aoc"
                     (str "y" year)
                     (str "d" day)
                     (str user ".cljc"))]
    (io/make-parents out)
    (when-not (.exists data-out)
      (spit data-out (data-ns year day))
      (println "Created a new file at" (.getPath data-out)))
    (if-not (.exists out)
      (do (spit out (user-ns year day user))
          (println "Created a new file at" (.getPath out)))
      (println "File already exists:" (.getPath out)))))

(def cli-options
  ;; An option with a required argument
  [["-y" "--year YEAR" "Year"
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 2015 % 2018) "Must be a number between 2015 and 2018 (inclusive)"]]
   ["-d" "--day DAY" "Day"
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 0 % 25) "Must be a number between 1 and 25 (inclusive)"]]
   ["-u" "--user USER" "User"
    :validate [#(re-find #"^[A-Za-z]" %) "Username must start with letter"]]
   ["-h" "--help"]])

(defn -main
  [& args]
  (let [{:keys [options arguments] :as opts}
        (parse-opts args cli-options)
        {:keys [options summary errors]}
        (if (empty? options)
          (parse-opts (interleave
                       ["-y" "-d" "-u"]
                       arguments)
                      cli-options)
          opts)]
    (cond (:help options)
          (println summary)
          errors
          (doseq [e errors]
            (println e))
          :else
          (create-new options))))
