(ns icfp2015.tetris
  (:require [clojure.set :as set]
            [schema.core :as s :refer [Bool Int]]
            [icfp2015.schema :refer :all]
            [icfp2015.cell :as c]
            [loom.graph :as g]
            [loom.label :as l]
            [icfp2015.core :refer :all]))

(s/defn prepare-game :- Game
  "Returns a game from the problem with given seed index."
  [problem :- Problem seedidx :- Int]
  (let [punits (:units problem)
        unitnum (count punits)
        board (problem->board problem)
        graphs (zipmap punits (map #(graph board %) (map move-to-spawn-pos punits)))
        unitindices (map #(mod % unitnum) (take (:sourceLength problem) (rng (nth (:sourceSeeds problem) seedidx))))
        sequnits (map #(nth punits %) unitindices)]
    {:seedIdx seedidx
     :board board
     :graphs graphs
     :unitstack sequnits})
  )


; just puts a stone at the best local position
(defn naive-placement
  "game -> unit-final-location"
  [game]
  (let [u (first (:unitstack game))
        g (get (:graphs game) u)
        nodes (g/nodes g)
        goodlocations (into [] (remove-nodes-xf g :sw :se) nodes)
        ]
    (first goodlocations)))

; just puts a stone at the best local position
(defn play-naive-tetris
  [game]
  )

; board [units] ->
;
;



