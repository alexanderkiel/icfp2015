(ns icfp2015.schema
  "Schemas for things in the game."
  (:require [schema.core :as s :refer [Bool Int]]
            [loom.graph :as g]))

(def Cell
  [(s/one Int "x")
   (s/one Int "y")])

(def Set
  (s/pred set?))

(def Unit
  {:pivot Cell
   :members Set})

(def Cmd
  (s/enum :e :w :se :sw :cw :ccw :noop))

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
   :filled Set
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

(def Phrase
  {:string [Char]
   :score Int})

(def Game
  {:problem-id Int
   :seed Int
   :board Board
   :unit-stack [Unit]
   :graphs {Unit Graph}
   :node-indices {Unit NodeIndex}
   :start-nodes {Unit Unit}
   :commands [Char]
   :finished Bool
   :phrases {[Cmd] Phrase}
   })

(def Placer
  "A placer calculates an end position (unit) of a unit in game."
  (s/=> Unit Game Unit))

(def PathGen
  "A path generator calculates path from start to end of a unit."
  (s/=> Unit Game Unit Unit))
