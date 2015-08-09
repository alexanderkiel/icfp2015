(ns icfp2015.schema
  "Schemas for things in the game."
  (:require [schema.core :as s :refer [Bool Int]]
            [loom.graph :as g]))

(def Cell
  [(s/one Int "x")
   (s/one Int "y")])

(def Unit
  {:pivot Cell
   :members [Cell]})

(def Cmd
  (s/enum :e :w :se :sw :cw :ccw :noop :lock))

(def Char
  (s/pred char?))

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

(def NodeIndex
  "An index from a cell on the board to nodes in a graph which occupy it."
  {Cell [Unit]})

(def Game
  {:seed-idx Int
   :board Board
   :unit-stack [Unit]
   :graphs {Unit Graph}
   :node-indices {Unit NodeIndex}
   :start-nodes {Unit Unit}
   :commands [Char]
   :finished Bool
   })

(def Placer
  "A placer calculates an end position (unit) of a unit in game."
  (s/=> Unit Game Unit))

(def PathGen
  "A path generator calculates path from start to end of a unit."
  (s/=> Unit Game Unit Unit))
