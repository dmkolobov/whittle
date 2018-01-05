(ns choreograph.util
  (:require [clojure.set :as set]))

(defn assoc-set
  [m k v]
  (assoc m k (conj (get m k #{}) v)))

(defn merge-set
  [m1 m2]
  (reduce (fn [m k] (update m k set/union (get m2 k)))
          m1
          (keys m2)))