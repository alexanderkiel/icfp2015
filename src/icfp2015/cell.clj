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

(defn- to-abc [[x y]]
  (let [a (- x (quot (if (even? y) y (dec y)) 2))]
    [a (- (+ a y)) y]))

(defn- from-abc [[a _ c]]
  [(+ a (quot (if (even? c) c (dec c)) 2)) c])

(s/defn global-2-local :- Cell [[px py] :- Cell [gx gy] :- Cell]
  [(+ (- gx px) (if (even? py) 0 (if (even? gy) (- 1) 0))) (- gy py)])

(s/defn local-2-global  :- Cell [[px py] :- Cell [lx ly] :- Cell]
  [(+ (+ px lx) (if (even? py) 0 (if (even? ly) 0 1))) (+ py ly)])

(defn- rotate-xf [rotate-fn pivot]
  (comp
    (map #(global-2-local pivot %))
    (map to-abc)
    (map rotate-fn)
    (map from-abc)
    (map #(local-2-global pivot %))))

(defn- rotate-cw-abc [[a b c]]
  [(- c) (- a) (- b)])

(defn- rotate-ccw-abc [[a b c]]
  [(- b) (- c) (- a)])

(s/defn rotate-cw-xf
  "Returns a xform which rotates cells clockwise around the pivot point."
  [pivot :- Cell]
  (rotate-xf rotate-cw-abc pivot))

(s/defn rotate-ccw-xf
  "Returns a xform which rotates cells clockwise around the pivot point."
  [pivot :- Cell]
  (rotate-xf rotate-ccw-abc pivot))

(s/defn neighbors :- [Cell]
  [[x y] :- Cell]
  (let [incdec (if (even? y) dec inc)]
    [[(dec x) y]
     [(inc x) y]
     [x (dec y)]
     [x (inc y)]
     [(incdec x) (inc y)]
     [(incdec x) (dec y)]]))
