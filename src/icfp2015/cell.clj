(ns icfp2015.cell
  (:require [schema.core :as s :refer [Bool]]
            [icfp2015.schema :refer [Cell Board]]))

(defn translator
  "Returns a function which translates the cell by dx, dy."
  [dx dy]
  (fn [[x y]] [(+ x dx) (+ y dy)]))

(s/defn move-east :- Cell [[x y] :- Cell]
  [(inc x) y])

(s/defn move-west :- Cell [[x y] :- Cell]
  [(dec x) y])

(s/defn move-south-east :- Cell [[x y] :- Cell]
  [(if (even? y) x (inc x)) (inc y)])

(s/defn move-south-west :- Cell [[x y] :- Cell]
  [(if (even? y) (dec x) x) (inc y)])

(s/defn valid? :- s/Bool
  "Tests if a cell has a valid pos on board."
  [{:keys [width height]} :- Board [x y] :- Cell]
  (and (< -1 x width) (< -1 y height)))
