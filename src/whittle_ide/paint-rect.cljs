(ns whittle.paint-rect
  (:require [whittle-ide.graph :refer [topsort]]

            [cljsjs.react-transition-group]

            ["react-transition-group/TransitionGroup" :as TransitionGroup]
            ["react-transition-group/Transition" :as Transition]
            ["react-transition-group/CSSTransition" :as CSSTransition]

            [reagent.core :as reagent]
            [clojure.string :as string]))

(defn translate ;;
  [x y]
  (str "translate3d("x"px, "y"px, 0)"))

(defn transit
  [prop duration ease]
  (str prop" "duration"s "ease))

(defn move
  [{:keys [id]}]
  (reagent/create-class
    {:reagent-render
     (fn [{:keys [x y class transition child]}]
       [:div.prect
         {:class class
          :style {:transform  (translate x y)
                  :transition transition}}
         child])}))

(def drop-transit
  (transit "transform" .15 "ease-in"));

(defn do-paint
  [{:keys [id
               child
               x
               y
               cx-open
               width height
               fix-width? fix-height?
               duration ease timeout]
        :or {duration 0.2
             ease     "ease-in"}
        :as node} state]
    [:div.prect
     [move
      {:id id
       :class "node"
       :x (if (some? cx-open)
            (if (= state "entering")
              cx-open
              x)
            x)
       :y y
       :child child
       :transition (if (= state "entered")
                     drop-transit
                     "none")}]
     (when-not fix-width?
       [move {:id id
              :class "v-mask"
              :x (if (some? cx-open)
                   (if (= state "entering")
                     cx-open
                     (+ x width))
                   (+ x width))
              :y y
              :transition (if (= state "entered")
                            drop-transit
                            "none")}])
     (when-not fix-height?
       [move {:id        id
              :class     "h-mask"
              :x          x
              :y          (if (= state "entered")
                            (+ y height)
                            y)
              :transition drop-transit}])])

 (defn paint
   [rects]
   [:> TransitionGroup
    {:component :div}
    (doall
      (for [{:keys [id timeout] :as node} rects]
        ^{:key id}
        [:> Transition
         {:component :div
          :timeout   timeout
          :appear    true}
          (fn [state]
            (reagent/as-element [do-paint node state]))]))])


(defn clips?
  [mask rect]
  (and (< (:x mask) (+ (:x rect) (:width rect)))
       (< (:y mask) (+ (:y rect) (:height rect)))))


(defn paint-graph
  [rects]
  (for [mask rects
        rect rects :when (not= mask rect)]
    (when (clips? mask rect) [rect mask])))

;(defn paint-list
;  [rects]
;  (topsort (filter some? (paint-graph (set rects)))))

(defn paint-list
  [rects]
  (->> (set rects)
       (sort-by :level)
       (partition-by :level)
       (map paint-graph)
       (map (partial filter some?))
       (mapcat topsort)))

(defn center-x
  [{:keys [x width]}]
  (+ (/ width 2) x))

(defn center
  "Centers the given rects about x"
  [x {:keys [width] :as rect}]
  (assoc rect :x (- x (/ width 2))))

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