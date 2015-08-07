(defproject icfp2015 "0.1-SNAPSHOT"
  :description "ICFPContest 2015"

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [prismatic/plumbing "0.4.4"]
                 [aysylu/loom "0.5.4"]]

  :profiles {:dev
             {:source-paths ["dev"]
              :dependencies [[org.clojure/tools.namespace "0.2.4"]
                             [juxt/iota "0.1.2"]]}})
