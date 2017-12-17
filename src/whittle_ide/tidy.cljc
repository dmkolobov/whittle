(ns whittle-ide.tidy
  (:require [whittle.inspect :refer [hiccup-tree? tree-index]]
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

         ;:delta    (- (/ width 2))
         ;:shift    (/ width 2)
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

;;   a
;;  / \
;; b   d
;;    / \
;;   c   e

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
        ;(assoc :delta (- width))
        ;(assoc :delta 0)
        (assoc :lcontour
               (push-contour (assoc lcontour level min-x)
                             (- min-x)))
        (assoc :rcontour
               (push-contour (assoc rcontour level max-x)
                             (- min-x)))
        ;(assoc :lcontour lcontour)
        ;(assoc :rcontour rcontour)
        ;(update :lcontour push-contour (- max-x))
        ;(update :rcontour push-contour (- min-x))
        ;(update :lcontour assoc level min-x)
        ;(update :rcontour assoc level max-x)
        (assoc :children children))))

(defn layout-zipper
  [parse-tree args]
  (zip/zipper (fn branch? [x] (and (layout-node? x) (seq (:children x))))
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
                            (assoc :x delta :y y)
                            (update :children
                                    (fn [children]
                                      (map (fn [child]
                                             (-> child
                                                 (assoc :edge-origin edge-origin)
                                                 (update :delta + delta (- shift))))
                                           children)))))))
            (zip/next))))))

(defn tidy
  [tree & args]
  (-> tree
      (layout-zipper args)
      (replace-trees args)
      (replace-nodes)
      (position-nodes)))