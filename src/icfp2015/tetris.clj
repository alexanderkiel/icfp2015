(ns icfp2015.tetris
  (:use plumbing.core)
  (:require [clojure.tools.logging :as log]
            [clojure.string :as str]
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
  [problem seed]
  (->> (rng seed)
       (map #(mod % (count (:units problem))))
       (map #(nth (:units problem) %))
       (take (:sourceLength problem))))

(s/defn prepare-game :- Game
  "Returns a game from the problem with given seed index."
  [problem :- Problem seed-idx :- Int]
  (let [seed (source-seed problem seed-idx)
        board (problem->board problem)
        units (:units problem)
        start-nodes (map #(move-to-spawn-pos (:width board) %) units)
        graphs (graphs (assoc board :filled #{}) units start-nodes)]
    {:problem-id (:id problem)
     :seed seed
     :board board
     :graphs graphs
     :node-indices (map-vals #(node-index board %) graphs)
     :start-nodes (zipmap units start-nodes)
     :unit-stack (source-units problem seed)
     :commands []
     :finished false
     :phrases {}}))


;; ---- Naive -----------------------------------------------------------------

(s/defn naive-placement :- Unit
  "Selects the locally best location for the given unit and returns its final location"
  [{:keys [board graphs]} :- Game unit :- Unit]
  (let [graph (graphs unit)
        nodes (g/nodes graph)
        first-good-xf
        (comp
          (remove-nodes-xf graph :sw :se)
          (map #(vector % (+ (count (unit-neighbors board %))
                             (unit-distance-over-bottom board %)))))]
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
  "Calculates the (shortest) path of unit to target-location"
  [{:keys [board graphs]} :- Game, unit :- Unit, target-location :- Unit]
  (let [start-pos (move-to-spawn-pos (:width board) unit)
        graph (graphs unit)
        path (ga/shortest-path graph start-pos target-location)
        non-locking-cmds (map #(:cmd (apply l/label graph %)) (g/out-edges graph target-location))
        locking-cmd (first (apply disj #{:w :e :se :sw} non-locking-cmds))]
    (print unit start-pos target-location)
    (conj (vec (map (fn [edge] (cmd-to-letter (:cmd (apply l/label graph edge))))
                          (partition 2 1 path)))
          (cmd-to-letter locking-cmd))))

;; ---- Phrases ------------------------------------------------------------------

;; Ei!
;; ia! ia!
;; r'lyeh
;; yuggoth

(s/defn add-phrases :- Game
  [game phrasestrings]
  (let [phraseentry (fn [str]
                      (let [str' (str/lower-case str)]
                      [(map letter-to-cmd str') {:string str', :score (count str')}]))]
    (assoc game :phrases
                (into {} (map phraseentry phrasestrings)))))


(defn walk-graph
  "goes from start with cmds along true"
  [g visited start cmds]
  (let [moveable
        (fn [loc cmd]
          (if-let [options (map (fn [edge] [(:cmd (apply l/label g edge)), edge]) (g/out-edges g loc))]
            (some #(when (= cmd (first %)) (second %)) options) nil
            )
          )]
    (loop [node start
           cmds cmds
           vis  (conj visited start)]
      (if-let [cmd (first cmds)]
        (if-let [[_ tgt] (moveable node cmd)]
          (if (vis tgt) nil (recur tgt (rest cmds) (conj visited node)))
          nil)
        [node vis]))))

(defn- sel-random [coll]
  (nth coll (rand-int (count coll))))

; (sel-random {1 2 3 4 5 6})

(defn- max-by [keyfn coll]
  (reduce #(if (> (keyfn %1) (keyfn %2)) %1 %2) coll)
  )
; (max-by second [[:a 2] [:b 3] [:c 1]])

(defn finish-path
  [graph visited start-pos target-location]
  (if-let [path (ga/shortest-path graph start-pos target-location)]
    (if (some visited (rest path))
      ; (let [_ (println "overlap:" start-pos path)] nil)                                                 ; parts of path are visited
      nil
      (let [non-locking-cmds (map #(:cmd (apply l/label graph %)) (g/out-edges graph target-location))
            locking-cmd (first (apply disj #{:w :e :se :sw} non-locking-cmds))]
        (conj (vec (map (fn [edge] (cmd-to-letter (:cmd (apply l/label graph edge))))
                        (partition 2 1 path)))
              (cmd-to-letter locking-cmd)))
      )
      nil)
  )

(defn- traverse [graph phrases target-location depth node visited path score phrase]
  (if phrase
    (if-let [[end vis'] (walk-graph graph visited node phrase)]
      (let [path' (apply conj path (:string (phrases phrase)))
            score' (+ (:score (phrases phrase)) score)
            children
            (map #(traverse graph phrases target-location (dec depth)
                            end vis' path' score' %
                         ) (if (< depth 1) [nil] (cons nil (keys phrases))))]
        (max-by second children))
      [[] 0])  ; can not apply leave
    (if-let [restpath (finish-path graph visited node target-location)] ; try direct
      [(apply conj path restpath) score]
      [[] 0])  ; could not finish leave
    )
  )



(s/defn best-path
  "Calculates the best path"
  [ depth :- Int, {:keys [board graphs start-nodes phrases] :as game} :- Game, unit :- Unit, target-location :- Unit]
  (let [start-pos (start-nodes unit)
        graph (graphs unit)
        children
          (map #(traverse graph phrases target-location depth start-pos #{} [] 0 % )
               (cons nil (keys phrases)))
        [best-path best-score] (max-by second children)]
    (println "path-score: " best-score)
    best-path
    )
  )


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

