 (ns user
   (:require [loom.graph :as g]))

(def g (g/weighted-digraph [1 2 1] [2 3 2] [3 1 1]))

(g/nodes g)
(g/edges g)
(g/successors g 2)
(g/out-degree g 2)
(g/weight g 2 3)
