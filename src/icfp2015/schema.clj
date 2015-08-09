(ns icfp2015.schema
  "Schemas for things in the game."
  (:require [schema.core :as s :refer [Int]]
            [loom.graph :as g]))

(def Cell
  [(s/one Int "x")
   (s/one Int "y")])

(def Unit
  {:pivot Cell
   :members [Cell]})

(def Cmd
  (s/enum :e :w :se :sw :cw :ccw))

(def Move
  [(s/one Cmd "cmd")
   (s/one Unit "dst")])

(def Graph
  (s/protocol g/Graph))

(def Board
  {:width Int
   :height Int
   :filled (s/pred set?)
   :units [Unit]})

(def Problem
  {:id Int
   :units [Unit]
   :width Int
   :height Int
   :filled [Cell]
   :sourceLength Int
   :sourceSeeds [Int]})

(def Game
  {:seedIdx Int
   :board Board
   :unitstack [Unit]
   :graphs {Unit Graph}})

(def Placer
  "A placer calculates an end position (unit) of a unit in game."
  (s/=> Unit Game Unit))
