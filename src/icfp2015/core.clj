(ns icfp2015.core
  (:use plumbing.core)
  (:require [clojure.set :as set]
            [schema.core :as s :refer [Int Bool]]
            [icfp2015.cell :as c]
            [icfp2015.schema :refer :all]
            [loom.graph :as g]
            [loom.label :as l]))

;; ---- Constructors ----------------------------------------------------------

(s/defn unit :- Unit
  "Creates a unit."
  [pivot :- Cell & members :- [Cell]]
  {:pivot pivot
   :members members})

(s/defn board :- Board
  "Creates a board."
  [width :- Int height :- Int & filled :- [Cell]]
  {:width width
   :height height
   :filled (set filled)
   :units []})

;; ---- Commands --------------------------------------------------------------

(defn- move [dir unit]
  (-> (update unit :pivot dir)
      (update :members (partial mapv dir))))

(s/defn move-east :- Unit [unit :- Unit]
  (move c/move-east unit))

(s/defn move-west :- Unit [unit :- Unit]
  (move c/move-west unit))

(s/defn move-south-east :- Unit [unit :- Unit]
  (move c/move-south-east unit))

(s/defn move-south-west :- Unit [unit :- Unit]
  (move c/move-south-west unit))

(s/defn turn-cw :- Unit [unit :- Unit]
  (update unit :members (partial into [] (c/rotate-cw-xf (:pivot unit)))))

(s/defn turn-ccw :- Unit [unit :- Unit]
  (update unit :members (partial into [] (c/rotate-ccw-xf (:pivot unit)))))

(def cmd-move {:e move-east
               :w move-west
               :sw move-south-west
               :se move-south-east
               :cw turn-cw
               :ccw turn-ccw
               :noop identity
               :lock identity})

;; ---- RNG -------------------------------------------------------------------

(defn- lcg [a c m]
  (fn [x] (mod (+ (* a x) c) m)))

(defn- to-random-num [seed]
  (bit-and (bit-shift-right seed 16) 0x7FFF))

(defn rng [seed]
  (let [iter (lcg 1103515245 12345 (bit-shift-left 1 32))]
    (map to-random-num (iterate iter seed))))

;; ---- Board -----------------------------------------------------------------

(defnk find-min-member-y [members]
  (apply min (map second members)))

(s/defn move-to-spawn-pos :- Unit [board-width :- Int unit :- Unit]
  (let [dy (find-min-member-y unit)
        [px py] (:pivot unit)
        ; first move the thing up
        top-pivot-pos [px (- py dy)]
        top-aligned (map #(c/local-2-global top-pivot-pos (c/global-2-local [px py] %)) (:members unit))
        ; then move it to the right place
        min-x (apply min (map first top-aligned))
        max-x (apply max (map first top-aligned))
        width (inc (- max-x min-x))
        dx (- (quot (- board-width width) 2) min-x)
        final-pivot-pos [(+ (first top-pivot-pos) dx) (second top-pivot-pos)]
        moved (map #(c/local-2-global final-pivot-pos (c/global-2-local top-pivot-pos %)) top-aligned)]
    (-> (assoc unit :pivot final-pivot-pos)
        (assoc :members moved))))

(s/defn spawn :- Board
  "Spawns a unit centered on top of the board."
  [board :- Board unit :- Unit]
  (->> (move-to-spawn-pos (:width board) unit)
       (vector)
       (assoc board :units)))

(s/defn clear-lines :- Board
  "Clears all full lines on board."
  [{:keys [width height filled] :as board} :- Board]
  (->> (loop [filled filled
              row-idx (dec height)]
         (let [filled-in-row (filter #(= row-idx (second %)) filled)]
           (if (= width (count filled-in-row))
             (recur
               (->> (apply disj filled filled-in-row)
                    (map (c/translator 0 1))
                    (set))
               row-idx)
             (if (zero? row-idx)
               filled
               (recur filled (dec row-idx))))))
       (assoc board :filled)))

(s/defn lock-unit :- Board
  "Locks unit on board."
  [board :- Board unit :- Unit]
  (-> (update board :filled #(into % (:members unit)))
      (clear-lines)))

(s/defn valid-cell? :- Bool
  "Tests if a cell is a valid (unfilled) cell."
  [{:keys [filled] :as board} cell]
  (and (c/valid? board cell)
       (not (contains? filled cell))))

(s/defn filter-valid-cells :- [Cell]
  [board :- Board cells :- [Cell]]
  (filter #(valid-cell? board %) cells))

;; ---- Unit ------------------------------------------------------------------

(s/defn unit-neighbors :- [Cell]
  "Returns a seq of all neighboar cells of a unit."
  [board :- Board unit :- Unit]
  (->> (set/difference (into #{} (mapcat c/neighbors) (:members unit))
                       (set (:members unit)))
       (seq)
       (filter-valid-cells board)))

;; ---- Graph -----------------------------------------------------------------

(defn- valid?
  "Tests if a unit can be placed on the board."
  {:arglists '([board unit])}
  [{:keys [filled] :as board} {:keys [members]}]
  (and (every? #(c/valid? board %) members)
       (not (some (set members) filled))))

(s/defn moves :- [Move]
  "Returns a seq of possible moves of the unit on the board."
  [board :- Board unit :- Unit]
  (sequence
    (comp
      (map (fn [[cmd move]] [cmd (move unit)]))
      (filter #(valid? board (second %))))
    {:e move-east
     :w move-west
     :se move-south-east
     :sw move-south-west
     :cw turn-cw
     :ccw turn-ccw}))

(s/defn graph :- Graph
  "Creates a graph of all reachable units from start unit on board."
  [board :- Board start :- Unit]
  (loop [g (g/digraph)
         units [start]]
    (if-let [unit (first units)]
      (let [moves (moves board unit)]
        (recur
          (->> (mapcat (fn [[cmd dst]] [[unit dst] {:cmd cmd}]) moves)
               (apply l/add-labeled-edges g))
          (into (rest units) (remove #(contains? (g/nodes g) %) (map second moves)))))
      g)))

(defn- all-cells [board]
  (for [x (range (:width board))
        y (range (:height board))]
    [x y]))

(defn find-nodes [graph cell]
  (filter (fnk [members] (some #{cell} members)) (g/nodes graph)))

(s/defn node-index :- NodeIndex
  "Creates a node index for graph on board."
  [board :- Board graph :- Graph]
  (map-from-keys #(find-nodes graph %) (all-cells board)))

(s/defn remove-nodes-xf
  "Xform which removes all nodes with outgoing edges of cmds."
  [graph :- Graph & cmds :- [Cmd]]
  (remove
    (fn [node]
      (->> (g/out-edges graph node)
           (map (fn [edge] (:cmd (apply l/label graph edge))))
           (some (set cmds))))))

(s/defn nodes-to-prune :- [Unit] [game :- Game unit :- Unit]
  (let [filled (:filled (:board game))
        node-index ((:node-indices game) unit)]
    (mapcat node-index filled)))

;; ---- Problem ---------------------------------------------------------------

(defnk problem->board :- Board [width height filled]
  (apply board width height filled))
