(ns icfp2015.schema
  "Schemas for things in the game."
  (:require [schema.core :as s :refer [Int]]))

(def Cell
  [(s/one Int "x")
   (s/one Int "y")])

(def Unit
  {:pivot Cell
   :members [Cell]})

(def Board
  {:width Int
   :height Int
   :filled [Cell]
   (s/optional-key :unit) Unit})

(def Problem
  {:id Int
   :units [Unit]
   :width Int
   :height Int
   :filled [Cell]
   :sourceLength Int
   :sourceSeeds [Int]})
