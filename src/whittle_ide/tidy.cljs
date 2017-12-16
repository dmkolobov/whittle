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
  [key level label width height lcontour rcontour children delta shift])

(defn layout-node? [x] (instance? LayoutNode x))

(defn make-child
  [parse-tree & {:keys [measurements labels key-fn default-key level] :as args}]
  (if (layout-node? parse-tree)
    parse-tree
    (let [key            (or (key-fn parse-tree) default-key)
          [width height] (get measurements key)
          label          (get labels key)]
      (map->LayoutNode
        {:key      key
         :level    level
         :label    label

         :delta    0
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
  (let [level (min (intersect-contour lcontour rcontour))]
    (/ (- (get rcontour level) (get lcontour level)) 2)))

(defn find-lcontour
  [children]
  (reduce (partial merge-f min) {} (map :lcontour children)))

(defn find-rcontour
  [children]
  (reduce (partial merge-f max) {} (map :rcontour children)))

(defn push-contour
  [contour delta]
  (into {}
        (map (fn [[level x]] [level (+ x delta)]))
        contour))

(defn max-overlap
  [left-tree-rcontour right-tree-lcontour]
  (let [common-levels (intersect-contour left-tree-rcontour right-tree-lcontour)]
    (println (map list
                  (map (partial get left-tree-rcontour)
                       common-levels)

                  (map (partial get right-tree-lcontour)
                       common-levels)))
    (->> (map list
              (map (partial get left-tree-rcontour)
                   common-levels)

              (map (partial get right-tree-lcontour)
                   common-levels))
         (filter (partial apply >))
         (map (partial apply -))
         (apply max))))

(def gap 40)

(defn spread-trees
  [children]
  (reduce (fn [row child]
            (let [overlap (max-overlap (find-rcontour children) (:lcontour child))
                  delta   (+ (:delta (last row)) overlap gap)]
              (println (:key child)
                       (find-rcontour children)
                       (:lcontour child)
                       overlap)
              (conj row
                    (-> child
                        (assoc :delta delta);; this delta value will be the final x coordinate
                        (update :lcontour push-contour delta)
                        (update :rcontour push-contour delta)))))
          [(first children)]
          (rest children)))

;;   a
;;  / \
;; b   d
;;    / \
;;   c   e

(defn center-node
  "Returns a vector [min-x max-x] representing the x-bounds of the given node
   when centered horizontally about 'x'."
  [{:keys [width]} x]
  [(- x (/ width 2)) (+ x (/ width 2))])

(defn layout-node
  [{:keys [level] :as node} children]
  (let [children      (spread-trees children)
        lcontour      (find-lcontour children)
        rcontour      (find-rcontour children)
        [min-x max-x] (center-node node (contour-center lcontour rcontour))]
    (-> node
        (assoc :shift    min-x)
        (assoc :lcontour (assoc lcontour level min-x))
        (assoc :rcontour (assoc rcontour level max-x))
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
  (println "replacing" (zip/node loc))
  (loop [loc loc]
    (let [loc' (-> loc
                   (zip/edit (fn [{:keys [children key level] :as node}]
                               (assoc node
                                 :children
                                 (map-indexed #(apply make-child
                                                    %2
                                                    (concat args
                                                            [:default-key (conj key %1)]
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
      (zip/node loc')))))

(defn tidy
  [tree & args]
  (-> tree
      (layout-zipper args)
      (replace-trees args)
      (replace-nodes)))