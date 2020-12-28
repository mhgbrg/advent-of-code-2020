(defproject advent-of-code-2020 "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/algo.generic "0.1.3"]]
  :plugins [[cider/cider-nrepl "0.25.5"]]
  :main advent-of-code-2020.core
  :repl-options {:init-ns advent-of-code-2020.core})
