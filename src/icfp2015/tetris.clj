(ns icfp2015.tetris
  (:use plumbing.core)
  (:require [clojure.tools.logging :as log]
            [schema.core :as s :refer [Bool Int]]
            [icfp2015.schema :refer :all]
            [icfp2015.cell :as c]
            [loom.graph :as g]
            [loom.label :as l]
            [icfp2015.core :refer :all]))

;; ---- Probability stuff ------------------------------------------------------

(defn softmax [values]
  (let [lst (map #(Math/exp %) values)
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

(defn- graphs [board {:keys [units]}]
  (->> units
       (map #(move-to-spawn-pos (:width board) %))
       (map #(graph board %))
       (zipmap units)))

(defn- source-seed [problem seed-idx]
  (nth (:sourceSeeds problem) seed-idx))

(defn- source-units
  "Returns a seq of source units according the seed index."
  [problem seed-idx]
  (->> (rng (source-seed problem seed-idx))
       (map #(mod % (count (:units problem))))
       (map #(nth (:units problem) %))
       (take (:sourceLength problem))))

(s/defn prepare-game :- Game
  "Returns a game from the problem with given seed index."
  [problem :- Problem seed-idx :- Int]
  (let [board (problem->board problem)
        graphs (graphs board problem)]
    {:seed-idx seed-idx
     :board board
     :graphs graphs
     :node-indices (map-vals #(node-index board %) graphs)
     :unit-stack (source-units problem seed-idx)}))

;; ---- Naive ------------------------------------------------------------------

(s/defn naive-placement :- Unit
  "Locks unit in a first good naive end position."
  [{:keys [board graphs] :as game} :- Game unit :- Unit]
  (let [nodes-to-prune (nodes-to-prune game unit)
        graph (apply g/remove-nodes (graphs unit) nodes-to-prune)
        nodes (g/nodes graph)
        first-good-xf
        (comp
          (remove-nodes-xf graph :sw :se)
          (map #(vector % (count (unit-neighbors board %)))))]
    (ffirst (sort-by second (sequence first-good-xf nodes)))))

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
  [placer :- Placer {:keys [unit-stack] :as game} :- Game]
  (if-let [unit (first unit-stack)]
    (let [end-pos (placer game unit)]
      (-> (update game :board #(lock-unit % end-pos))
          (update :unit-stack rest)))
    game))

