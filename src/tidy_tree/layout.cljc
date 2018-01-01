(ns tidy-tree.layout
  (:require [clojure.zip :as zip]
            [tidy-tree.util :refer [layout-node? map->LayoutNode common-keys merge-f fast-forward rewind zip-seq node-branch? zipper]]))

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
      (zipper)
      (->layout-nodes opts)
      (zip/root)))

(defn get-labels
  [tidy-tree]
  (map (juxt :id :label) (zip-seq (zipper tidy-tree)))) ;

(defn add-dimensions
  [measures {:keys [level] :as node}]
  (let [[width height] (get measures (:id node))]
    (assoc node
      :width    width
      :height   height
      :lcontour {level 0}
      :rcontour {level width})))

(defn layout
  [tidy-tree measures args]
  (-> tidy-tree
      (zipper)
      (fast-forward (partial add-dimensions measures))
      (space-nodes args)
      (position-nodes)
      (zip/root)))
