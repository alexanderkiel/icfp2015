 (ns user
   (:require [clojure.core.async :refer [put! close!]]
             [loom.graph :as g]
             [icfp2015.server :as server]
             [icfp2015.core :refer :all]))

(server/start 5011)

(def g (g/weighted-digraph [1 2 1] [2 3 2] [3 1 1]))

(g/nodes g)
(g/edges g)
(g/successors g 2)
(g/out-degree g 2)
(g/weight g 2 3)

(comment
  (put! server/ch (board 10 10 [0 9] [1 9]))
  )
