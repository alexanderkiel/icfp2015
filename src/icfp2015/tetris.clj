(ns icfp2015.tetris
  (:require [clojure.set :as set]
            [schema.core :as s :refer [Bool Int]]
            [icfp2015.schema :refer :all]
            [icfp2015.cell :as c]
            [loom.graph :as g]
            [loom.label :as l]
            [icfp2015.core :refer :all]))

;; ---- Probability stuff ------------------------------------------------------

(defn softmax [values]
  (let [lst (map  #(Math/exp %) values)
        sum (reduce + lst)]
    (map #(/ % sum) lst)))

(defn sampler-max [values]
  (let [sm (softmax values)
        cummulative (reductions + sm)
        sample (rand)
        ]
    (count (take-while #(< % sample) cummulative))
    )
  )
(defn sampler-min [values]
  (sampler-max (map - values)))

;; ---- Init ------------------------------------------------------------------

(s/defn prepare-game :- Game
  "Returns a game from the problem with given seed index."
  [problem :- Problem seedidx :- Int]
  (let [punits (:units problem)
        unitnum (count punits)
        board (problem->board problem)
        graphs (zipmap punits
                       (map #(graph board %)
                            (map #(move-to-spawn-pos (:width board) %) punits)))
        unitindices (map #(mod % unitnum)
                         (take (:sourceLength problem)
                               (rng (nth (:sourceSeeds problem) seedidx))))
        sequnits (map #(nth punits %) unitindices)]
    {:seedIdx seedidx
     :board board
     :graphs graphs
     :unitstack sequnits}))

;; ---- Naive ------------------------------------------------------------------

(s/defn naive-placement :- Game
  "Locks unit in a first good naive end position."
  [{:keys [board graphs] :as game} unit]
  (let [graph (graphs unit)
        nodes (g/nodes graph)
        first-good-xf
        (comp
          (remove-nodes-xf graph :sw :se)
          (map #(vector % (count (unit-neighbors board %)))))
        end-node (ffirst (sort-by second (sequence first-good-xf nodes)))]
    (update game :board #(lock-unit % end-node))))

; just puts a stone at the best local position
(defn naive-placement2
  "game -> unit-final-location"
  [{:keys [graphs] :as game} unit]
  (let [g (graphs unit)
        nodes (g/nodes g)
        b (:board game)
        targetlocations (into [] (remove-nodes-xf g :sw :se) nodes)
        scores (map (count #(unit-neighbors b %)) targetlocations)
        loc (nth targetlocations (sampler-min scores))
        ]
    loc))

(s/defn step :- Game
  "Plays one step in the game.

  Pops one unit from unit stack and locks it at its end position. Does nothing
  if unit stack is empty."
  [ placer {:keys [unitstack] :as game} :- Game]
  (if-let [unit (first unitstack)]
    (-> (placer game unit)
        (update :unitstack rest))
    game))

