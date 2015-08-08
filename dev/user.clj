(ns user
  (:use plumbing.core)
  (:require [clojure.core.async :refer [put! close!]]
            [clojure.pprint :refer [pprint]]
            [criterium.core :refer [quick-bench]]
            [loom.graph :as g]
            [loom.label :as l]
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
  (swap! b #(spawn % (nth (:units p0) 18)))
  (swap! b #(spawn % {:members [[0 1] [1 2]], :pivot [0 1]}))
  (swap! b #(update-in % [:units 0] move-east))
  (swap! b #(update-in % [:units 0] move-west))
  (swap! b #(update-in % [:units 0] move-south-east))
  (swap! b #(update-in % [:units 0] move-south-west))
  (swap! b #(update-in % [:units 0] turn-cw))
  (swap! b #(update-in % [:units 0] turn-ccw))
  (swap! b #(lock-unit %))

  ;; Show all childs of first unit
  (swap! b (fn [b] (update b :units #(into % (moves b (first (:units b)))))))

  )

(comment

  (moves @b (first (:units @b)))
  (pprint *1)

  (let [g (graph @b (first (:units @b)))
        nodes (g/nodes g)]
    (->> (sequence
           (comp
             (remove-nodes-xf g :sw :se)
             (take 6))
           nodes)
         (assoc @b :units)
         (reset! b)))

  (pprint *1)

  )
