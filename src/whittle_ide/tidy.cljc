(ns whittle-ide.tidy
  (:require [whittle-ide.rect :as rect]
            [whittle-ide.util :refer [common-keys merge-f fast-forward rewind zip-seq]]
            [clojure.zip :as zip]
            [clojure.set :as set]))

(defrecord LayoutNode
  [id level label x y width height lcontour rcontour children delta shift])

(defn layout-node? [x] (instance? LayoutNode x))

(defn make-child
  [tree {:keys [branch? children label-fn id-fn default-id level] :as args}]
  (if (layout-node? tree)
    tree
    (let [id             (or (id-fn tree) default-id)
          label          (label-fn tree)]
      (map->LayoutNode
        {:id       id

         :level    level
         :label    label

         :children (when (branch? tree) (children tree))}))))

(defn contour-center
  "Given a left contour and a right contour, return the point centered between
  their topmost limits."
  [lcontour rcontour]
  (let [level (apply min (common-keys lcontour rcontour))]
    (/ (- (get rcontour level) (get lcontour level)) 2.0)))

(defn push-contour
  [contour delta]
  (into {}
        (map (fn [[level x]] [level (+ x delta)]))
        contour))

(defn pair-contours
  [left-tree-rcontour right-tree-lcontour]
  (let [common-levels (sort (common-keys left-tree-rcontour right-tree-lcontour))]
    (map list
         (map (partial get left-tree-rcontour) common-levels)
         (map (partial get right-tree-lcontour) common-levels))))

(defn find-overlap
  [left-rcontour right-lcontour delta]
  (let [pairs (pair-contours left-rcontour right-lcontour)]
    (if-let [max-overlap (->> pairs
                              (filter (partial apply >=))
                              (map #(- (first %1) (second %1) delta))
                              (apply max))]
      max-overlap
      0)))

(def merge-lcontour (partial merge-f min))
(def merge-rcontour (partial merge-f max))

(defn push-and-merge-contours
  "This function simulates threads."
  [node delta lcontour rcontour]
  (-> node
      (update :delta + delta)
      (update :lcontour #(merge-lcontour lcontour (push-contour % delta)))
      (update :rcontour #(merge-rcontour rcontour (push-contour % delta)))))

(defn spread-trees
  [children {:keys [h-gap]}]
  (reduce (fn [row child]
            (let [{:keys [delta lcontour rcontour]} (last row)
                  overlap (find-overlap rcontour (:lcontour child) delta)
                  delta   (+ delta overlap h-gap)]
              (conj row
                    (push-and-merge-contours child delta lcontour rcontour))))
          [(first children)]
          (rest children)))

(defn center-node
  "Returns a vector [min-x max-x] representing the x-bounds of the given node
   when centered horizontally about 'x'."
  [{:keys [width]} x]
  [(- x (/ width 2.0))
   (+ x (/ width 2.0))])

(defn layout-node
  [{:keys [level width] :as node} children args]
  (let [children      (spread-trees children args)
        lcontour      (:lcontour (last children))
        rcontour      (:rcontour (last children))
        [min-x max-x] (center-node node (contour-center lcontour rcontour))]
    (assoc node
      :shift    min-x
      :lcontour (push-contour (assoc lcontour level min-x) (- min-x))
      :rcontour (push-contour (assoc rcontour level max-x) (- min-x))
      :children children)))

(defn node-branch? [x] (and (layout-node? x) (seq (:children x))))

(defn layout-zipper
  [tidy]
  (zip/zipper node-branch?
              :children
              #(assoc %1 :children %2)
              tidy))

(defn ->layout-nodes
  [loc args]
  (fast-forward loc (fn [{:keys [children id level] :as node}]
                      (assoc node
                        :children
                        (map-indexed #(make-child
                                       %2
                                       (assoc args
                                         :default-id (conj id %1)
                                         :level      (inc level)))
                                     children)))));;

(defn space-nodes
  [loc args]
  (rewind loc (fn [tree]
                (if (node-branch? tree)
                  (layout-node tree (:children tree) args)
                  tree))))

(defn position-nodes
  [loc]
  (fast-forward (zip/edit loc
                          (fn [{:keys [lcontour] :as node}]
                            (update node :delta - (apply min (vals lcontour)))))
                (fn [{:keys [delta shift] :as node}]
                          (-> node
                            (assoc :x delta)
                            (update :children
                                    (fn [children]
                                      (map (fn [child]
                                             (-> child
                                                 (update :delta + delta (- shift))))
                                           children)))))))

(defn ->tidy
  [tree opts]
    (println opts)
    (-> (make-child tree (assoc opts :level 0))
        (layout-zipper)
        (->layout-nodes opts)
        (zip/root)))

(defn get-labels
  [tidy-tree]
  (map (juxt :id :label) (zip-seq (layout-zipper tidy-tree)))) ;

(defn add-dimensions
  [measures {:keys [level] :as node}]
  (let [[width height] (get measures (:id node))]
    (assoc node
      :width    width
      :height   height
      :lcontour {level 0}
      :rcontour {level width})))

(defn tidy
  [tidy-tree measures args]
  (-> tidy-tree
      (layout-zipper)
      (fast-forward (partial add-dimensions measures))
      (space-nodes args)
      (position-nodes)
      (zip/root)))

;; ---- Plotting ----------------------------------------------------------------

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
         rows     (->> (layout-zipper tidy-tree)
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

;; ---- Animation -----------------------------------------------------------------

(defn add-ticks
  "Given a location in the tree, the current tick, and a map of ticks, returns
  a new map of ticks with an entry for each sibling of the location node, including
  the location node itself."
  [loc ticks current-tick]
  (if loc
    (recur (zip/right loc)
           (assoc ticks (:id (zip/node loc)) current-tick)
           current-tick)
    ticks))

(defn choreograph
  "Given a tidy tree, return a map where keys are node ids and values
  are node ticks."
  ([tidy]
   (choreograph 0 tidy))
  ([start tidy]
  (loop [loc          (layout-zipper tidy)
         current-tick start
         ticks        {}]
    (if (zip/end? loc)
      ticks
      (if (contains? ticks (:id (zip/node loc)))
        (recur (zip/next loc)
               current-tick
               ticks)
        (recur (zip/next loc)
               (inc current-tick)
               (add-ticks loc ticks current-tick)))))))
