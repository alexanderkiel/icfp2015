(defproject icfp2015 "0.1-SNAPSHOT"
  :description "ICFPContest 2015"

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [prismatic/plumbing "0.4.4"]
                 [aysylu/loom "0.5.4"]
                 [http-kit "2.1.18"]
                 [jarohen/chord "0.6.0"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.slf4j/slf4j-api "1.7.7"]
                 [ch.qos.logback/logback-classic "1.1.2"]]

  :profiles {:dev
             {:source-paths ["dev"]
              :dependencies [[org.clojure/tools.namespace "0.2.4"]
                             [juxt/iota "0.1.2"]]}})
