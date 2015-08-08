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

(def Board
  {:width Int
   :height Int
   :filled [Cell]
   :units [Unit]})

(def Problem
  {:id Int
   :units [Unit]
   :width Int
   :height Int
   :filled [Cell]
   :sourceLength Int
   :sourceSeeds [Int]})

(def Graph
  (s/protocol g/Graph))
