(ns icfp2015.cell-test
  (:require [clojure.test :refer :all]
            [icfp2015.cell :refer :all]
            [icfp2015.core :refer [board]]
            [juxt.iota :refer [given]]
            [schema.test :refer [validate-schemas]]
            [criterium.core :refer [quick-bench]]))

(use-fixtures :once validate-schemas)

(deftest move-east-test
  (is (= [1 0] (move-east [0 0]))))

(deftest move-south-east-test
  (is (= [0 1] (move-south-east [0 0])))
  (is (= [1 2] (move-south-east [0 1]))))

(deftest valid?-test
  (let [b (board 10 10)]
    (is (valid? b [0 0]))
    (is (valid? b [9 0]))
    (is (valid? b [0 9]))
    (is (valid? b [9 9]))

    (is (not (valid? b [-1 -1])))
    (is (not (valid? b [-1 0])))
    (is (not (valid? b [0 -1])))
    (is (not (valid? b [0 10])))))

(deftest rotate-cw-xf-test
  (let [xf (rotate-cw-xf [3 2])]
    (is (= [[4 2] [2 2]] (sequence xf [[3 1] [2 3]])))))

(deftest rotate-ccw-xf-test
  (let [xf (rotate-ccw-xf [3 2])]
    (is (= [[3 1] [2 3]] (sequence xf [[4 2] [2 2]])))))

(deftest neighbors-test
  (is (= #{[1 0] [2 0] [2 1] [2 2] [1 2] [0 1]} (set (neighbors [1 1]))))
  (is (= #{[1 1] [2 1] [3 2] [2 3] [1 3] [1 2]} (set (neighbors [2 2])))))

(comment
  (quick-bench (move-east [0 0]))                           ;  30 ns
  (quick-bench (move-south-east [0 0]))                     ;  35 ns
  (let [b (board 10 10)] (quick-bench (valid? b [0 0])))    ; 190 ns
  (quick-bench (neighbors [1 1]))                           ; 135 ns

  (let [xf (rotate-cw-xf [3 2])]
    (quick-bench (sequence xf [[3 1] [2 3]])))              ; 500 ns
  )
