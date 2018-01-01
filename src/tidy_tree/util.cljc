(ns tidy-tree.util
  (:require [clojure.set :as set]
            [clojure.zip :as zip]))

;; ---- hash-map functions ----------------------------------------------------------------

(defn merge-f
  "Merges maps m1 and m2. Whenever two values v1 and v2 exist for the same key,
  take the value returned by (f v1 v2)."
  [f m1 m2]
  (reduce (fn [m level]
            (assoc m
              level (if (and (contains? m1 level)
                             (contains? m2 level))
                      (f (get m1 level) (get m2 level))
                      (get m1 level (get m2 level)))))
          {}
          (set/union (set (keys m1))
                     (set (keys m2)))))

(defn diff-map
  [m1 m2]
  (reduce dissoc m1 (keys m2)))

(defn common-keys
  "Given two hash-maps, return the set of keys present in both."
  [m1 m2]
  (set/intersection (set (keys m1))
                    (set (keys m2))))

;; ---- zipper functions -----------------------------------------------------------------

(defn fast-forward
  [loc f]
  (let [loc' (zip/edit loc f)]
    (if (zip/end? (zip/next loc'))
      loc'
      (recur (zip/next loc') f))))

(defn rewind
  [loc f]
  (let [loc' (zip/edit loc f)]
    (if-let [prev (zip/prev loc')]
      (recur prev f)
      (if-let [up (zip/up loc')]
        (recur up f)
        loc'))))

(defn zip-seq
  "Given a zipper location, return the sequence of tree nodes in depth
  first order."
  [loc]
  (loop [nodes []
         loc   loc]
    (if (zip/end? loc)
      nodes
      (recur (conj nodes (zip/node loc))
             (zip/next loc)))))

;; ---- tidy tree -----------------------------------------------------------------------

(defrecord LayoutNode
  [id level label x y width height lcontour rcontour children delta shift])

(defn layout-node? [x] (instance? LayoutNode x))

(defn node-branch? [x] (and (layout-node? x) (seq (:children x))))

(defn zipper
  [tidy]
  (zip/zipper node-branch?
              :children
              #(assoc %1 :children %2)
              tidy))

(defn node-seq
  [tidy-tree]
  (zip-seq (zipper tidy-tree)))