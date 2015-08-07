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
  {:width s/Int
   :height s/Int
   :filled [Cell]
   (s/optional-key :unit) Unit})
