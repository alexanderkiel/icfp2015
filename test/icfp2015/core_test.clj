(ns icfp2015.core-test
  (:require [clojure.test :refer :all]
            [icfp2015.core :refer :all]
            [juxt.iota :refer [given]]
            [schema.test :refer [validate-schemas]]))

(use-fixtures :once validate-schemas)

(deftest move-east-test
  (testing "Move east"
    (given (move-east (unit [1 0] [1 0]))
      :pivot := [0 0]
      :members :> [[0 0]])))

(deftest spawn-test
  (testing "Spawn one-cell unit on 1x1 board."
    (given (spawn (board 1 1) (unit [0 0] [0 0]))
      :unit := (unit [0 0] [0 0])))
  (testing "Spawn one-cell unit on 2x1 board."
    (given (spawn (board 2 1) (unit [0 0] [0 0]))
      :unit := (unit [0 0] [0 0])))
  (testing "Spawn one-cell unit on 3x1 board."
    (given (spawn (board 3 1) (unit [0 0] [0 0]))
      :unit := (unit [1 0] [1 0])))
  (testing "Spawn one-cell unit on 4x1 board."
    (given (spawn (board 4 1) (unit [0 0] [0 0]))
      :unit := (unit [1 0] [1 0])))
  (testing "Spawn one-cell unit on 5x1 board."
    (given (spawn (board 5 1) (unit [0 0] [0 0]))
      :unit := (unit [2 0] [2 0])))
  (testing "Spawn right shifted one-cell unit on 3x1 board."
    (given (spawn (board 3 1) (unit [1 0] [1 0]))
      :unit := (unit [1 0] [1 0])))
  (testing "Spawn left shifted one-cell unit on 3x1 board."
    (given (spawn (board 3 1) (unit [-1 0] [-1 0]))
      :unit := (unit [1 0] [1 0]))))
