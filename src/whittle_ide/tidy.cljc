(ns whittle-ide.tidy
  (:require [whittle-ide.rect :as rect]
            [whittle-ide.util :refer [common-keys merge-f fast-forward]]
            [clojure.zip :as zip]
            [clojure.set :as set]))

(defrecord LayoutNode
  [id level label x y width height lcontour rcontour children delta shift])

(defn layout-node? [x] (instance? LayoutNode x))

(defn make-child
  [tree {:keys [branch? children measurements labels id-fn default-id level] :as args}]
  (if (layout-node? tree)
    tree
    (let [id             (or (id-fn tree) default-id)
          [width height] (get measurements id)
          label          (get labels id)]
      (map->LayoutNode
        {:id       id

         :level    level
         :label    label

         :width    width
         :height   height

         :lcontour {level 0}
         :rcontour {level width}

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
         (map (partial get left-tree-rcontour)
              common-levels)

         (map (partial get right-tree-lcontour)
              common-levels))))

(defn find-delta
  [left-rcontour right-lcontour]
  (let [pairs (pair-contours left-rcontour right-lcontour)]
    (if-let [max-overlap (->> pairs
                              (filter (partial apply >=))
                              (map (partial apply -))
                              (apply max))]
      max-overlap
      0)))

(def gap 10)

(defn push-and-merge-contours
  "This function simulates threads."
  [node delta lcontour rcontour]
  (-> node
      (update :lcontour #(merge-f min lcontour (push-contour % delta)))
      (update :rcontour #(merge-f max rcontour (push-contour % delta)))))

(defn spread-trees
  [children]
  (reduce (fn [row {:keys [width] :as child}]
            (let [last-dt (:delta (last row))
                  overlap (find-delta (:rcontour (last row))
                                      (push-contour (:lcontour child) last-dt))
                  delta   (+ last-dt overlap gap)]
              (conj row
                    (-> child
                        (update :delta + delta);; this delta value will be the final x coordinate
                        (push-and-merge-contours delta (:lcontour (last row)) (:rcontour (last row)))))))
          [(first children)]
          (rest children)))

(defn center-node
  "Returns a vector [min-x max-x] representing the x-bounds of the given node
   when centered horizontally about 'x'."
  [{:keys [width]} x]
  [(- x (/ width 2.0))
   (+ x (/ width 2.0))])

(defn layout-node
  [{:keys [level width] :as node} children]
  (let [children      (spread-trees children)
        lcontour      (:lcontour (last children))
        rcontour      (:rcontour (last children))
        [min-x max-x] (center-node node (contour-center lcontour rcontour))]
    (-> node
        (assoc :shift min-x)
        (assoc :lcontour
               (push-contour (assoc lcontour level min-x) (- min-x)))
        (assoc :rcontour
               (push-contour (assoc rcontour level max-x) (- min-x)))
        (assoc :children children))))

(defn node-branch? [x] (and (layout-node? x) (seq (:children x))))

(defn layout-zipper
  [tree args]
  (zip/zipper node-branch?
              (fn [{:keys [children]}] children)
              #(assoc %1 :children %2)
              (if (layout-node? tree)
                tree
                (make-child tree (assoc args :level 0)))))

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
  [loc]
  (let [loc' (if (zip/branch? loc)
               (zip/replace loc (layout-node (zip/node loc)
                                             (zip/children loc)))
               loc)]
  (if-let [prev (zip/prev loc')]
    (recur prev)
    (if-let [up (zip/up loc')]
      (recur up)
      loc'))))

(defn position-nodes
  [loc]
  (fast-forward loc
                (fn [{:keys [delta shift] :as node}]
                          (-> node
                            (assoc :x delta)
                            (update :children
                                    (fn [children]
                                      (map (fn [child]
                                             (-> child
                                                 (update :delta + delta (- shift))))
                                           children)))))))

(defn node-seq-zipper
  [tidy]
  (zip/zipper node-branch?
              :children
              #(assoc %1 :children %2)
              tidy))

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
  [tidy]
  (loop [loc          (node-seq-zipper tidy)
         current-tick 0
         ticks        {}]
     (if (zip/end? loc)
       ticks
       (if (contains? ticks (:id (zip/node loc)))
         (recur (zip/next loc)
                current-tick
                ticks)
         (recur (zip/next loc)
                (inc current-tick)
                (add-ticks loc ticks current-tick))))))

(defn node-seq
  [tidy]
  (loop [nodes []
         loc   (node-seq-zipper tidy)]
    (if (zip/end? loc)
      nodes
      (recur (conj nodes (zip/node loc))
             (zip/next loc)))))

(defn tidy
  [tree & args]
  (let [args (apply hash-map args)]
    (-> tree
        (layout-zipper args)
        (->layout-nodes args)
        (space-nodes)
        (position-nodes)
        (zip/root)))) ;

(defn plot-branch
  [{:keys [edge-stroke edge-height]} {:keys [children] :as node}]
  (let [cx    (rect/center-x node)
        cxs   (map rect/center-x children)
        min-x (apply min cxs)
        max-x (apply max cxs)
        width (- max-x min-x)
        left  (- min-x (/ edge-stroke 2))]
    {:stem   (rect/center cx {:width edge-stroke :height edge-height})
     :branch {:x      left
              :width  (+ width edge-stroke)
              :offset (- cx left)
              :height edge-stroke}}))

(defn plot-node
  "Given a tidy-tree node, return a parts map."
  [{:keys [edge-stroke edge-height] :as opts}
   {:keys [level children width height y] :as node}]
  (let [cx (rect/center-x node)]
    (merge (when (not= level 0)
             {:root (rect/center cx {:width edge-stroke :height (+ edge-height y)})})
           {:body (rect/center cx {:width width :height height})}
           (when (seq children)
             (if (= 1 (count children))
               {:stem (rect/center cx {:width  edge-stroke
                                       :height (+ edge-height edge-stroke)})}
               (plot-branch opts node))))))

(defn plot
  "Given a tidy-tree, return a sequence of vectors [node parts], where parts
  is a map containing the following keys:

  :root   - the edge drawn from the parent node branch to the current node
  :body   - the node label itself
  :stem   - the portion of the edge drawn from this node to where the edge branches
  :branch - the horizontal portion of the child edge.

  Each part has an :x, :y, :width, and :height."
  [tidy-tree & opts]
  (loop [baseline 0
         rows     (->> tidy-tree
                       (node-seq)
                       (group-by :level)
                       (sort-by first)
                       (map second))
         result   []]
    (println baseline)
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