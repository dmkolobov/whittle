(ns whittle-ide.graph
  (:require [clojure.set :as set]))

;; [:a :b] [:b :c] [:c :d] [:d :f]
;;                 [:c :e]
;;
;; map-graph:
;; ---------
;;
;; {:a #{:b}
;;  :b #{:c}
;;  :c #{:d :e}
;;  :d #{:f}}

(defn ->map-graph
  [edges]
  (reduce (fn [g [parent child]]
            (update g
                    parent
                    (fn [s] (if (seq s) (conj s child) #{child}))))
          {}
          edges))

(defn no-outgoing
  [map-graph]
  (let [parents  (set (keys map-graph))
        children (reduce set/union (vals map-graph))]
    (set/difference children parents)))

(defn remove-nodes
  [map-graph nodes]
  (reduce (fn [g [node children]]
            (let [children' (set/difference children nodes)]
              (if (seq children') (assoc g node children') g)))
          {}
          map-graph))

(defn topsort
  "Returns a topological sort of the nodes in edges."
  [edges]
  (let [init-graph (->map-graph edges)]
    (loop [sort-nodes []
           graph      init-graph]
      (if (seq graph)
        (let [nodes (no-outgoing graph)]
          (if (seq nodes)
            (recur (into sort-nodes nodes)
                   (remove-nodes graph nodes))
            (throw (ex-info "cycle detected" {:graph graph}))))
        (into sort-nodes
              (set/difference (set (keys init-graph)) (set sort-nodes)))))))