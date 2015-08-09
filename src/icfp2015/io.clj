(ns icfp2015.io
  (:use plumbing.core)
  (:require [clojure.data.json :as json]
            [schema.core :as s]
            [icfp2015.schema :refer [Problem]]))

(defnk transform-cell [x y]
  [x y])

(defn- transform-unit [unit]
  (-> (update unit :pivot transform-cell)
      (update :members #(set (map transform-cell %)))))

(s/defn read-problem :- Problem [file]
  (-> (json/read-str (slurp file) :key-fn keyword)
      (update :units (partial mapv transform-unit))))
