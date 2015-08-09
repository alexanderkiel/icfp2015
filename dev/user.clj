(ns user
  (:use plumbing.core)
  (:require [clojure.core.async :refer [put! close!]]
            [clojure.pprint :refer [pprint]]
            [criterium.core :refer [quick-bench]]
            [loom.graph :as g]
            [loom.label :as l]
            [icfp2015.server :as server]
            [icfp2015.io :refer [read-problem]]
            [icfp2015.core :refer :all]
            [icfp2015.tetris :refer :all]))

(defonce stop (server/start 5011))

(def b (atom {}))
(add-watch b :send (fn [_ _ _ b] (put! server/ch b)))

(defn find-bottom-nodes
  "Finds six bottom nodes of board starting with first unit."
  [board]
  (let [g (graph board (first (:units board)))
        nodes (g/nodes g)]
    (->> (into []
           (comp
             (remove-nodes-xf g :sw :se)
             (take 6))
           nodes)
         (assoc board :units))))

(comment
  (def p0 (read-problem "problems/problem_0.json"))
  (reset! b (problem->board p0))

  (swap! b #(spawn % (first (:units p0))))
  (swap! b #(spawn % (second (:units p0))))
  (swap! b #(spawn % (nth (:units p0) 2)))
  (swap! b #(spawn % (nth (:units p0) 3)))
  (swap! b #(spawn % (nth (:units p0) 17)))
  (swap! b #(spawn % {:members [[0 1] [1 2]], :pivot [0 1]}))

  (swap! b #(update-in % [:units 0] move-east))
  (swap! b #(update-in % [:units 0] move-west))
  (swap! b #(update-in % [:units 0] move-south-east))
  (swap! b #(update-in % [:units 0] move-south-west))
  (swap! b #(update-in % [:units 0] turn-cw))
  (swap! b #(update-in % [:units 0] turn-ccw))

  (swap! b find-bottom-nodes)
  (swap! b lock-units)

  ;; Show all childs of first unit
  (swap! b (fn [b] (update b :units #(into % (moves b (first (:units b)))))))

  ;; show neigbors of current unit
  (first (:units @b))
  (unit-neighbors @b (first (:units @b)))
  (swap! b #(assoc-in % [:units 1] {:pivot [0 0], :members (seq (unit-neighbors @b (first (:units @b))))}))

  )

;; ---- Game ------------------------------------------------------------------

(def game (atom {}))

(defn init-game! [problem seed-idx]
  (reset! game (prepare-game problem seed-idx))
  :ok)

(defn step-game! []
  (swap! game (partial step naive-placement))
  :ok)

(defn step-game2! []
  (swap! game (partial step naive-placement2))
  :ok)

(defn show-game! []
  (reset! b (:board @game))
  :ok)

(comment
  (def p0 (read-problem "problems/problem_0.json"))
  (init-game! p0 0)
  (step-game!)
  (step-game2!)
  (show-game!)

  (count (:unitstack @game))
  (:board @game)

)
