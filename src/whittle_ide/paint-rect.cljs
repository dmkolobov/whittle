(ns whittle.paint-rect
  (:require [whittle-ide.graph :refer [topsort]]))

(defn translate
  [x y]
  (str "translate("x"px, "y"px)"))

(defn transit
  [prop duration ease delay]
  (str prop" "duration"s "ease" "delay"s"))

(defn paint-rect
  [{:keys [x
           y
           width
           height

           child]}]
  (let [t "transform .2s ease-in"]
    [:div
     [:div.node
      {:style {:position   "absolute"
               :transform  (translate x y)
               :transition t}}
      child]
      [:div.v-mask {:style {:transform  (translate (+ x width) y)
                            :transition t}}]
      [:div.h-mask {:style {:transform  (translate x (+ y height))
                            :transition t}}]]))


(defn clips?
  [mask rect]
  (and (< (:x mask) (+ (:x rect) (:width rect)))
       (< (:y mask) (+ (:y rect) (:height rect)))))


(defn paint-graph
  [rects]
  (for [mask rects
        rect rects :when (not= mask rect)]
    (when (clips? mask rect) [rect mask])))

(defn paint-list
  [rects]
  (topsort (filter some? (paint-graph (set rects)))))

(defn center-x
  [{:keys [x width]}]
  (+ (/ width 2) x))

(defn center
  "Centers the given rects about x"
  [x rects]
  (map (fn [{:keys [width] :as rect}]
         (assoc rect :x (- x (/ width 2))))
       rects))

(defn find-baseline
  "Returns the minimum y bound of the given rects."
  [& rects]
  (apply max (map #(+ (:y %) (:height %)) rects)))

(defn space
  [baseline rects]
  (loop [rects'   []
         rects    rects
         baseline baseline]
    (if (seq rects)
      (let [[r & rs] rects]
        (recur (conj rects' (assoc r :y baseline))
               rs
               (+ baseline (:height r))))
      rects')))