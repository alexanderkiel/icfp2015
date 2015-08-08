(ns icfp2015.core
  (:use plumbing.core)
  (:require [schema.core :as s :refer [Int]]
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
   :filled filled
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

;; ---- Unit ------------------------------------------------------------------

(s/defn unit-neighors [unit :- Unit]
  (set (apply concat (map c/neighbors (:members unit)))))

;; ---- Board -----------------------------------------------------------------

(defnk problem->board :- Board [width height filled]
  (apply board width height filled))

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
        moved (map #(c/local-2-global final-pivot-pos (c/global-2-local top-pivot-pos %)) top-aligned)
        ]
    (-> (assoc unit :pivot final-pivot-pos)
        (assoc :members moved))
    )
  )


(s/defn spawn :- Board
  "Spawns a unit centered on top of the board."
  [board :- Board unit :- Unit]
  (->> (move-to-spawn-pos (:width board) unit)
       (vector)
       (assoc board :units)))

(s/defn lock-units :- Board [board :- Board]
  (-> (update board :filled #(into % (mapcat :members (:units board))))
      (assoc :units [])))

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

(s/defn remove-nodes-xf
  "Xform which removes all nodes with outgoing edges of cmds."
  [graph :- Graph & cmds :- [Cmd]]
  (remove
    (fn [node]
      (->> (g/out-edges graph node)
           (map (fn [edge] (:cmd (apply l/label graph edge))))
           (some (set cmds))))))
