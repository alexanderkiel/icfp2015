(ns icfp2015.core-test
  (:require [clojure.test :refer :all]
            [icfp2015.core :refer :all]
            [juxt.iota :refer [given]]
            [schema.test :refer [validate-schemas]]
            [loom.graph :as g]))

(use-fixtures :once validate-schemas)

(deftest move-east-test
  (testing "Move east"
    (given (move-east (unit [1 0] [1 0]))
      :pivot := [2 0]
      :members := [[2 0]])))

(deftest spawn-test
  (testing "Spawn one-cell unit on 1x1 board."
    (given (spawn (board 1 1) (unit [0 0] [0 0]))
      :units :> [(unit [0 0] [0 0])]))
  (testing "Spawn one-cell unit on 2x1 board."
    (given (spawn (board 2 1) (unit [0 0] [0 0]))
      :units :> [(unit [0 0] [0 0])]))
  (testing "Spawn one-cell unit on 3x1 board."
    (given (spawn (board 3 1) (unit [0 0] [0 0]))
      :units :> [(unit [1 0] [1 0])]))
  (testing "Spawn one-cell unit on 4x1 board."
    (given (spawn (board 4 1) (unit [0 0] [0 0]))
      :units :> [(unit [1 0] [1 0])]))
  (testing "Spawn one-cell unit on 5x1 board."
    (given (spawn (board 5 1) (unit [0 0] [0 0]))
      :units :> [(unit [2 0] [2 0])]))
  (testing "Spawn right shifted one-cell unit on 3x1 board."
    (given (spawn (board 3 1) (unit [1 0] [1 0]))
      :units :> [(unit [1 0] [1 0])]))
  (testing "Spawn left shifted one-cell unit on 3x1 board."
    (given (spawn (board 3 1) (unit [-1 0] [-1 0]))
      :units :> [(unit [1 0] [1 0])])))

(deftest moves-test
  (testing "All moves are possible"
    (given (moves (board 3 3) (unit [1 0] [1 0]))
      set := #{[:w (unit [0 0] [0 0])]
               [:e (unit [2 0] [2 0])]
               [:sw (unit [0 1] [0 1])]
               [:se (unit [1 1] [1 1])]
               [:cw (unit [1 0] [1 0])]
               [:ccw (unit [1 0] [1 0])]}))
  (testing "Only east moves are possible"
    (given (moves (board 3 3) (unit [0 0] [0 0]))
      set := #{[:e (unit [1 0] [1 0])]
               [:se (unit [0 1] [0 1])]
               [:cw (unit [0 0] [0 0])]
               [:ccw (unit [0 0] [0 0])]})))

(deftest graph-test
  (given (graph (board 2 2) (unit [0 0] [0 0]))
    [g/nodes set] := #{(unit [0 0] [0 0])
                       (unit [1 0] [1 0])
                       (unit [0 1] [0 1])
                       (unit [1 1] [1 1])}
    [g/edges set] :> #{[(unit [0 0] [0 0]) (unit [0 0] [0 0])]
                       [(unit [0 0] [0 0]) (unit [1 0] [1 0])]
                       [(unit [0 0] [0 0]) (unit [0 1] [0 1])]
                       [(unit [1 0] [1 0]) (unit [1 0] [1 0])]
                       [(unit [1 0] [1 0]) (unit [0 0] [0 0])]
                       [(unit [1 0] [1 0]) (unit [0 1] [0 1])]
                       [(unit [1 0] [1 0]) (unit [1 1] [1 1])]
                       [(unit [0 1] [0 1]) (unit [0 1] [0 1])]
                       [(unit [0 1] [0 1]) (unit [1 1] [1 1])]
                       [(unit [1 1] [1 1]) (unit [1 1] [1 1])]
                       [(unit [1 1] [1 1]) (unit [0 1] [0 1])]}))

(deftest rng-test
  (is (= [0 24107 16552 12125 9427 13152 21440 3383 6873 16117]
         (take 10 (rng 17)))))

(deftest node-index-test
  (let [board (board 2 2)
        graph (graph board (unit [0 0] [0 0]))]
    (is (= (node-index board graph)
           {[0 0] [(unit [0 0] [0 0])]
            [1 0] [(unit [1 0] [1 0])]
            [0 1] [(unit [0 1] [0 1])]
            [1 1] [(unit [1 1] [1 1])]}))))

(deftest clear-lines-test
  (testing "Works on empty filled"
    (given (clear-lines (board 2 2))
      :filled := #{}))

  (testing "Clears one line"
    (given (clear-lines (board 2 2 [0 1] [1 1]))
      :filled := #{}))

  (testing "Doesn't clear single filled cell"
    (given (clear-lines (board 2 2 [0 1]))
      :filled := #{[0 1]}))

  (testing "Moves upper filled cells downwards"
    (given (clear-lines (board 2 2 [0 0] [0 1] [1 1]))
      :filled := #{[0 1]}))

  (testing "Moves all upper filled cells downwards"
    (given (clear-lines (board 2 3 [0 0] [0 1] [1 1] [0 2] [1 2]))
      :filled := #{[0 2]})))
