(ns icfp2015.tetris-test
  (:require [clojure.test :refer :all]
            [icfp2015.tetris :refer :all]
            [icfp2015.io :refer [read-problem]]
            [juxt.iota :refer [given]]
            [schema.test :refer [validate-schemas]]))

(use-fixtures :once validate-schemas)

(deftest prepare-game-test
  (is (prepare-game (read-problem "problems/problem_0.json") 0)))

(deftest step-test
  (let [game (prepare-game (read-problem "problems/problem_0.json") 0)]
    (given (step naive-placement game)
      :unit-stack :< (:unit-stack game))))
