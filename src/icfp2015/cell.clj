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

(defn to-abc [[x y]]
  (let [a (- x (quot (- y (if (even? y) 0 1)) 2))
        c y
        b (- (- a) c)]
    [a b c]))

(defn from-abc [[a _ c]]
  [(+ a (quot (- c (if (even? c) 0 1)) 2)) c])

(defn global-2-local [[px py] [gx gy]]
  [(+ (- gx px) (if (even? py) 0 (if (even? gy) (- 1) 0))) (- gy py)])

(defn local-2-global [[px py] [lx ly]]
  [(+ (+ px lx) (if (even? py) 0 (if (even? ly) 0 1))) (+ py ly)])

(defn rotate-cw-abc [[a b c]]
  [(- c) (- a) (- b)])

(defn rotate-ccw-abc [[a b c]]
  [(- b) (- c) (- a)])
