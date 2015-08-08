(ns icfp2015.cell-test
  (:require [clojure.test :refer :all]
            [icfp2015.cell :refer :all]
            [icfp2015.core :refer [board]]
            [juxt.iota :refer [given]]
            [schema.test :refer [validate-schemas]]))

(use-fixtures :once validate-schemas)

(deftest move-east-test
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
