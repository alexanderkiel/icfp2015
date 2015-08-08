(ns icfp2015.core
  (:use plumbing.core)
  (:require [schema.core :as s :refer [Int]]
            [icfp2015.cell :as c]
            [icfp2015.schema :refer :all]))

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
   :filled filled})

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

(defn even01 [x]
  (cond (even? x) 0 :else 1))

(defn to-abc [[x y]]
  (let [a (- x (quot (- y (even01 y)) 2))
        c y
        b (- (- a) c)
        ]
    [a b c])
  )

(defn from-abc [[a _ c]]
      [ (+ a (quot (- c (even01 c)) 2)) c]
  )

(defn local-2-global [[px py] [lx ly]]
  [(+ (+ px lx) (if (even? py) 0 (if (even? ly) 0 1))) (+ py ly)])
(defn global-2-local [[px py] [gx gy]]
  [(+ (- gx px) (if (even? py) 0 (if (even? gy) (- 1) 0))) (- gy py)])


(defn rotate-cw-abc [[a b c]]
  [(- c) (- a) (- b)])
(defn rotate-ccw-abc [[a b c]]
  [(- b) (- c) (- a)])


(defn turn-members [rotate-fn pivot members]
  (let [ppos pivot
        rotated (map (comp #(local-2-global ppos %) from-abc rotate-fn to-abc #(global-2-local ppos %)) members)
       ]
    rotated)
  )

(s/defn turn-cw :- Unit [unit :- Unit]
  (update unit :members #(turn-members rotate-cw-abc (:pivot unit) %))
  )

(s/defn turn-ccw :- Unit [unit :- Unit]
  (update unit :members #(turn-members rotate-ccw-abc (:pivot unit) %))
  )

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

(s/defn spawn :- Board
  "Spawns a unit centered on top of the board."
  [board :- Board unit :- Unit]
  (->> unit
       (align-top)
       (center (:width board))
       (assoc board :unit)))

(s/defn lock-unit :- Board [board :- Board]
  (-> (update board :filled #(into % (:members (:unit board))))
      (dissoc :unit)))
