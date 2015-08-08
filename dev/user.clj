(ns user
  (:require [clojure.core.async :refer [put! close!]]
            [clojure.pprint :refer [pprint]]
            [criterium.core :refer [quick-bench]]
            [loom.graph :as g]
            [icfp2015.cell :as c]
            [icfp2015.server :as server]
            [icfp2015.io :refer [read-problem]]
            [icfp2015.core :refer :all]))

(defonce stop (server/start 5011))

(def g (g/weighted-digraph [1 2 1] [2 3 2] [3 1 1]))

(g/nodes g)
(g/edges g)
(g/successors g 2)
(g/out-degree g 2)
(g/weight g 2 3)

(def b (atom {}))
(add-watch b :send (fn [_ _ _ b] (put! server/ch b)))

(comment
  (def p0 (read-problem "problems/problem_0.json"))
  (reset! b (problem->board p0))

  (swap! b #(spawn % (first (:units p0))))
  (swap! b #(spawn % (second (:units p0))))
  (swap! b #(spawn % (nth (:units p0) 2)))
  (swap! b #(spawn % (nth (:units p0) 3)))
  (swap! b #(update % :unit move-east))
  (swap! b #(update % :unit move-west))
  (swap! b #(update % :unit move-south-east))
  (swap! b #(update % :unit move-south-west))
  (swap! b #(update % :unit turn-cw))
  (swap! b #(update % :unit turn-ccw))
  (swap! b #(lock-unit %))

  (to-abc [1 2])
  (map (c/translator (- 1) (- 1)) [[1 1], [2 3]])

  (turn-members rotate-cw-abc [1 1] [[2 1], [0 1]])

  second (:units p0)
  (nth (:units p0) 3)
  )
