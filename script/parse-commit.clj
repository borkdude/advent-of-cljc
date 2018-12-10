(let [commit-message (first *command-line-args*)
      [year day user] (rest (re-find #"(\d{4})[ /,](\d{2})[ /,](\w+)" commit-message))]
  (assert (and year day user) "Please provide your commit message in the format yyyy/dd/user ...")
  (println year day user))
