(ns icfp2015.tetris
  (:use plumbing.core)
  (:require [clojure.tools.logging :as log]
            [schema.core :as s :refer [Bool Int]]
            [icfp2015.schema :refer :all]
            [icfp2015.cell :as c]
            [loom.graph :as g]
            [loom.alg :as ga]
            [loom.alg-generic :as alg-generic]
            [loom.label :as l]
            [icfp2015.core :refer :all]))

;; ---- Probability stuff -----------------------------------------------------

(defn softmax [values]
  (let [lst (map #(Math/exp %) values)
        sum (reduce + lst)]
    (map #(/ % sum) lst)))

(defn sampler-max [values]
  (let [sm (softmax values)
        cummulative (reductions + sm)
        sample (rand)]
    (count (take-while #(< % sample) cummulative))))

(defn sampler-min [values]
  (sampler-max (map - values)))

;; ---- Init ------------------------------------------------------------------

(defn- graphs [board units start-nodes]
  (->> start-nodes
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
        units (:units problem)
        start-nodes (map #(move-to-spawn-pos (:width board) %) units)
        graphs (graphs board units start-nodes)]
    {:seed-idx seed-idx
     :board board
     :graphs graphs
     :node-indices (map-vals #(node-index board %) graphs)
     :start-nodes (zipmap units start-nodes)
     :unit-stack (source-units problem seed-idx)
     :commands []
     :finished false}))

;; ---- Naive -----------------------------------------------------------------

(s/defn naive-placement :- Unit
  "Selects the locally best location for the given unit and returns its final location"
  [{:keys [board graphs]} :- Game unit :- Unit]
  (let [graph (graphs unit)
        nodes (g/nodes graph)
        first-good-xf
        (comp
          (remove-nodes-xf graph :sw :se)
          (map #(vector % (count (unit-neighbors board %)))))]
    (ffirst (sort-by second (sequence first-good-xf nodes)))))

; just puts a stone at the best local position
(defn naive-placement-sample
  "selects a good location for the given unit and returns its final location"
  [{:keys [graphs] :as game} unit]
  (let [g (graphs unit)
        nodes (g/nodes g)
        b (:board game)
        targetlocations (into [] (remove-nodes-xf g :sw :se) nodes)
        scores (map #(count (unit-neighbors b %)) targetlocations)
        loc (nth targetlocations (sampler-min scores))
        ]
    loc))


;; ---- Path ------------------------------------------------------------------

;{p, ', !, ., 0, 3}	move W
;{b, c, e, f, y, 2}	move E
;{a, g, h, i, j, 4}	move SW
;{l, m, n, o, space, 5}    	move SE
;{d, q, r, v, z, 1}	rotate clockwise
;{k, s, t, u, w, x}	rotate counter-clockwise
;\t, \n, \r	(ignored)

(def cmd-to-letter {:w   \!
                    :e   \e
                    :sw  \i
                    :se  \o
                    :cw  \r
                    :ccw \x
                    :noop \tab})
(def letter-to-cmd
  (apply hash-map (concat (interleave [\p, \', \!, \., \0, \3] (repeat :w))
                          (interleave [\b, \c, \e, \f, \y, \2] (repeat :e))
                          (interleave [\a, \g, \h, \i, \j ,\4] (repeat :sw))
                          (interleave [\l, \m, \n, \o, \space , \5] (repeat :se))
                          (interleave [\d, \q, \r, \v, \z, \1] (repeat :cw))
                          (interleave [\k, \s, \t, \u, \w, \x] (repeat :ccw))
                          (interleave [\tab, \return \newline] (repeat :noop))
                          )))


(s/defn stupid-path
  "Calculates the (shortest) path to target-location"
  [{:keys [board graphs] :as game} :- Game unit :- Unit target-location :- Unit]
  (let [start-position (move-to-spawn-pos (:width board) unit)
        g (get graphs unit)
        path (ga/shortest-path g start-position target-location)
        nonlocking (map #(:cmd (apply l/label g %)) (g/out-edges g target-location))
        lockingmove :sw                                        ;    (first (disj {:w :e :se :sw} nonlocking)
        ]
    (print lockingmove)
    (conj (vec (map (fn [edge] (cmd-to-letter (:cmd (apply l/label g edge))))
                          (partition 2 1 path)))
          (cmd-to-letter lockingmove))))

;; ---- Game ------------------------------------------------------------------


(defn- reachable-subgraph [graph start]
  (g/subgraph graph (alg-generic/bf-traverse (g/successors graph) start)))

(defn- prune-graph [graph nodes-to-prune start]
  (-> (apply g/remove-nodes graph nodes-to-prune)
      (reachable-subgraph start)))

(defn prune-game
  "Prunes the graph of unit in game."
  [game unit]
  (let [nodes-to-prune (nodes-to-prune game unit)
        start ((:start-nodes game) unit)]
    (update-in game [:graphs unit] #(prune-graph % nodes-to-prune start))))

(s/defn step :- Game
  "Plays one step in the game.

  Pops one unit from unit stack and locks it at its end position.
  Finishes game if empty unit stack or invalid spawn position."
  [placer :- Placer, path-gen :- PathGen
  , {:keys [unit-stack start-nodes board] :as game} :- Game]
  (if-let [unit (first unit-stack)]
    (if (valid? board (start-nodes unit))
      (let [pruned-game (prune-game game unit)
           end-pos (placer pruned-game unit)]
       (-> (update game :board #(lock-unit % end-pos))
           (update :commands #(into % (path-gen pruned-game unit end-pos)))
           (update :unit-stack rest)))
      (assoc game :finished true))
    (assoc game :finished true)))

(s/defn play :- Game
  "Plays the game to the end."
  [placer :- Placer, path-gen :- PathGen, game :- Game]
  (loop [game game]
    (if-not (:finished game)
      (recur (step placer path-gen game))
      game)))

(s/defn spawn-next :- Game
  [{:keys [unit-stack] :as game} :- Game ]
  (if (empty? unit-stack)
    (assoc game :finished true)
    (let [unit (first unit-stack)]
      (-> (update game :board #(spawn % unit))
          (update :unit-stack rest)))))

(s/defn micro-step :- Game
  "Plays one small move"
  [{:keys [board commands finished] :as game} :- Game]
  (let [cmd (letter-to-cmd (first commands))
        unit (first (:units board))]
    (if (nil? unit)
      (spawn-next game)
      (let [newunit ((cmd-move cmd) unit)]
        (if (valid? board newunit)
          (-> (assoc-in game [:board :units 0] newunit)
              (update :commands rest))
          (-> (update game :board #(lock-unit % unit))
              (assoc-in [:board :units] [])
              (update :commands rest)))))))

