(ns icfp2015.core
  (:use plumbing.core)
  (:require [schema.core :as s :refer [Int]]
            [icfp2015.cell :as c]
            [icfp2015.schema :refer :all]
            [loom.graph :as g]))

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

;; ---- Board -----------------------------------------------------------------

(defnk problem->board :- Board [width height filled]
  (apply board width height filled))

(defnk find-min-member-y [members]
  (apply min (map second members)))

(defn- align-top [unit]
  (let [dy (find-min-member-y unit)
        t (c/translator 0 (- dy))]
    (-> (update unit :pivot t)
        (update :members (partial mapv t)))))

(defnk find-min-member-x [members]
  (apply min (map first members)))

(defnk find-max-member-x [members]
  (apply max (map first members)))

(defn- center [board-width unit]
  (let [min-x (find-min-member-x unit)
        width (inc (- (find-max-member-x unit) min-x))
        dx (- (quot (- board-width width) 2) min-x)
        t (c/translator dx 0)]
    (-> (update unit :pivot t)
        (update :members (partial mapv t)))))

(s/defn move-to-spawn-pos :- Unit [board-width :- Int unit :- Unit]
  (center board-width (align-top unit)))

(s/defn spawn :- Board
  "Spawns a unit centered on top of the board."
  [board :- Board unit :- Unit]
  (->> (move-to-spawn-pos (:width board) unit)
       (vector)
       (assoc board :units)))

(s/defn lock-unit :- Board [board :- Board]
  (-> (update board :filled #(into % (:members (first (:units board)))))
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
     :sw move-south-west}))

(s/defn graph :- Graph [board :- Board unit :- Unit]
  (loop [g (g/weighted-digraph)
         units [unit]]
    (if-let [unit (first units)]
      (let [moves (moves board unit)]
        (recur
          (g/add-edges* g (map (fn [[cmd dst]] [unit dst cmd]) moves))
          (into (rest units) (remove #(contains? (g/nodes g) %) (map second moves)))))
      g)))
