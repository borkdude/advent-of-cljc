(require '[clojure.edn :as edn])
(require '[clojure.string :as str])

(def cmd (first *command-line-args*))

(assert (#{"update" "select"} cmd))
(assert (second *command-line-args*) "edn file")

(def data (edn/read-string
           (format "[%s]"
                   (slurp (second *command-line-args*)))))

(defn ->sql [v]
  (cond (string? v)
        (format "'%s'" v)
        (coll? v)
        (format "(%s)" (str/join ", " (map ->sql v)))
        :else v))

(def query
  (case cmd
    "update"
    (str "insert into scores (year, day, username, environment, test, time) values\n"
         (str/join ",\n" (map (comp ->sql (juxt :year :day :user :env :test :time)) data)) "\n"
         "on conflict (year, day, username, environment, test) do update set time = excluded.time\n"
         "returning *")
    "select" (let [days (->> data
                             (map (juxt :year :day))
                             distinct
                             ->sql)]
               (format "select year, day, test, username, time, environment
                       from scores where (year, day) in %s
                       order by year, day, test, time"
                       days))))

(println query)
