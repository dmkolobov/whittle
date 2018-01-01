(ns tidy-tree.plot
  (:require [tidy-tree.util :refer [zipper zip-seq]]
            [tidy-tree.rect :as rect]))


(defn plot-branch
  [{:keys [stroke v-gap]} {:keys [children] :as node}]
  (let [cx    (rect/center-x node)
        cxs   (map rect/center-x children)
        min-x (apply min cxs)
        max-x (apply max cxs)
        width (- max-x min-x)
        left  (- min-x (/ stroke 2))]
    {:stem   (rect/center cx {:width stroke :height v-gap})
     :branch {:x      left
              :width  (+ width stroke)
              :offset (- cx left)
              :height stroke}}))

(defn plot-node
  "Given a tidy-tree node, return a parts map."
  [{:keys [stroke v-gap] :as opts}
   {:keys [level children width height y] :as node}]
  (let [cx (rect/center-x node)]
    (merge (when (not= level 0)
             {:root (rect/center cx {:width stroke :height (+ v-gap y)})})
           {:body (rect/center cx {:width width :height height})}
           (when (seq children)
             (if (= 1 (count children))
               {:stem (rect/center cx {:width  stroke
                                       :height (+ v-gap stroke)})}
               (plot-branch opts node))))))

(defn plot
  "Given a tidy-tree, return a sequence of vectors [node parts], 'node' is a node in
  the tree, and 'parts' is a map of rectangles representing the drawing of the tree node.
  The map 'parts' has the following keys:

    :root   - the edge drawn from the parent node branch to the current node
    :body   - the node label itself
    :stem   - the portion of the edge drawn from this node to where the edge branches
    :branch - the horizontal portion of the child edge.

  Each value of 'parts' is map whose keys are a sub-set of #{:root :body :stem branch}.

  'opts' should be a map of the following rendering options:

     :stroke - the width of the edge stroke, in pixels
     :v-gap  - the minimum height of the root and stem of each node, in pixels
     :h-gap  - the horizontal gap between each node, in pixels
    "
  [tidy-tree opts]
  (loop [baseline 0
         rows     (->> (zipper tidy-tree)
                       (zip-seq)
                       (group-by :level)
                       (sort-by first)
                       (map second))
         result   []]
    (if (seq rows)
      (let [nodes        (first rows)
            parts        (map (partial plot-node opts) nodes)
            spaced-rects (map (partial rect/space baseline) (map vals parts))]
        (recur (rect/find-baseline (apply concat spaced-rects))
               (rest rows)
               (into result
                     (map (fn [node part-keys part-rects]
                            [node (zipmap part-keys part-rects)])
                          nodes
                          (map keys parts)
                          spaced-rects))))
      result)))