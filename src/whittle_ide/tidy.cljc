(ns whittle-ide.tidy
  (:require [whittle.inspect :refer [hiccup-tree? tree-index]]
            [whittle-ide.rect :as rect]
            [clojure.zip :as zip]
            [clojure.set :as set]))

(defn fast-forward
  [loc]
  (if (zip/end? loc) loc (recur (zip/next loc))))

(defn rewind
  [loc]
  (if (zip/end? loc)
    loc
    (if-let [prev (zip/prev loc)] (recur prev) loc)))

(defrecord LayoutNode
  [id level label width height lcontour rcontour children delta shift])

(defn layout-node? [x] (instance? LayoutNode x))

(defn make-child
  [parse-tree & {:keys [measurements labels id-fn default-id level] :as args}]
  (if (layout-node? parse-tree)
    parse-tree
    (let [id             (or (id-fn parse-tree) default-id)
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

         :children (when (hiccup-tree? parse-tree) (rest parse-tree))}))))

(defn merge-f
  "Merges maps m1 and m2. Whenever two values v1 and v2 exist for the same key,
  take the value returned by (f v1 v2)."
  [f m1 m2]
  (reduce (fn [m level]
            (assoc m
              level (cond (and (contains? m1 level)
                               (contains? m2 level))
                          (f (get m1 level) (get m2 level))

                          (contains? m1 level)
                          (get m1 level)

                          (contains? m2 level)
                          (get m2 level))))
          {}
          (set/union (set (keys m1))
                     (set (keys m2)))))

(defn intersect-contour
  "Given two contours, return a sorted sequence of levels contained in both."
  [contour-1 contour-2]
  (sort
    (set/intersection (set (keys contour-1))
                      (set (keys contour-2)))))

(defn contour-center
  "Given a left contour and a right contour, return the point centered between
  their topmost limits."
  [lcontour rcontour]
  (let [level (apply min (intersect-contour lcontour rcontour))]
    (/ (- (get rcontour level) (get lcontour level)) 2.0)))

(defn find-lcontour
  [children]
  (reduce (partial merge-f min)
          {}
          (concat (map :lcontour children)
                  (map :rcontour children))))

(defn find-rcontour
  [children]
  (reduce (partial merge-f max)
          {}
          (concat (map :lcontour children)
                  (map :rcontour children))))

(defn push-contour
  [contour delta]
  (into {}
        (map (fn [[level x]] [level (+ x delta)]))
        contour))

(defn pair-contours
  [left-tree-rcontour right-tree-lcontour]
  (let [common-levels (intersect-contour left-tree-rcontour right-tree-lcontour)]
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

(defn spread-trees
  [children]
  (let [[first & siblings] children]
    (if (seq siblings)
      (reduce (fn [row {:keys [width] :as child}]
                (let [last-dt (:delta (last row))
                      overlap (find-delta (push-contour (find-rcontour row)
                                                        (- last-dt))
                                          (:lcontour child))
                      delta   (+ last-dt overlap gap)]
                  (conj row
                        (-> child
                            (assoc :overlap overlap)
                            (update :delta + delta);; this delta value will be the final x coordinate
                            (update :lcontour push-contour delta)
                            (update :rcontour push-contour delta)))))
              [first]
              siblings)
      [first])))

(defn center-node
  "Returns a vector [min-x max-x] representing the x-bounds of the given node
   when centered horizontally about 'x'."
  [{:keys [width]} x]
  [(- x (/ width 2.0))
   (+ x (/ width 2.0))])

(defn layout-node
  [{:keys [level width] :as node} children]
  (let [children      (spread-trees children)
        lcontour      (find-lcontour children)
        rcontour      (find-rcontour children)
        [min-x max-x] (center-node node (contour-center lcontour rcontour))]
    (-> node
        (assoc :shift min-x)
        (assoc :lcontour
               (push-contour (assoc lcontour level min-x)
                             (- min-x)))
        (assoc :rcontour
               (push-contour (assoc rcontour level max-x)
                             (- min-x)))
        (assoc :children children))))

(defn node-branch? [x] (and (layout-node? x) (seq (:children x))))

(defn layout-zipper
  [parse-tree args]
  (zip/zipper node-branch?
              (fn [{:keys [children]}] children)
              #(assoc %1 :children %2)
              (if (layout-node? parse-tree)
                parse-tree
                (apply make-child parse-tree (concat args [:level 0])))))

(defn replace-trees
  [loc args]
  (loop [loc loc]
    (let [loc' (-> loc
                   (zip/edit (fn [{:keys [children id level] :as node}]
                               (assoc node
                                 :children
                                 (map-indexed #(apply make-child
                                                    %2
                                                    (concat args
                                                            [:default-id (conj id %1)]
                                                            [:level       (inc level)]))
                                              children)))))]
      (if (zip/end? (zip/next loc'))
        loc
        (recur (zip/next loc'))))))

(defn replace-nodes
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
  (loop [loc loc]
    (if (zip/end? loc)
      (zip/root loc)
      (recur
        (-> loc
            (zip/edit (fn [{:keys [delta shift level width height] :as node}]
                        (let [y           (* level 33)
                              edge-origin [(+ delta (/ width 2))
                                           (+ y height)]]
                          (-> node
                            (assoc :x delta)
                            (update :children
                                    (fn [children]
                                      (map (fn [child]
                                             (-> child
                                                 (assoc :edge-origin edge-origin)
                                                 (update :delta + delta (- shift))))
                                           children)))))))
            (zip/next))))))

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
  (-> tree
      (layout-zipper args)
      (replace-trees args)
      (replace-nodes)
      (position-nodes)))

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