(ns tidy-tree.anim
  (:require [reagent.core :as reagent]
            [cljsjs.react-transition-group]

            ["react-transition-group/TransitionGroup" :as TransitionGroup]
            ["react-transition-group/Transition" :as Transition]
            ["react-transition-group/CSSTransition" :as CSSTransition]))

(defn translate
  [x y z]
  (str "translate3d("x"px, "y"px, "z"px)"))

(defn transit
  ([prop duration ease]
   (str prop" "(/ duration 1000)"s "ease))

  ([prop duration ease delay]
   (str (transit prop duration ease) " " (/ delay 1000) "s")))

(defn run-transitions
  [{:keys [timeout-fn]} animations]
  [:> TransitionGroup
   {:component :div}
   (for [[anim {:keys [id] :as opts}] animations]
     ^{:key [id]}
     [:> Transition
      {:component :div
       :timeout   (timeout-fn opts)
       :on-enter  #(.-scrollTop %)
      ; :unmount-on-exit true
       :appear    true}
      (fn [transition-state]
        (reagent/as-element [anim (assoc opts :transition-state transition-state)]))])])

(defn compose-child
  [child transition-state]
  (if (and (vector? child)
           (map? (second child)))
    (update child 1 assoc :transition-state transition-state)
    child))

(defn moves
  [{:keys [child
           transition-state
           x
           y
           z

           duration
           ease
           delay]
    :or {ease  "linear"
         delay 0
         z     0}}]
  (let [tx         (translate x y z)
        transition (transit "transform" duration ease delay)]
    [:div.rect-child
     {:style {:transition transition
              :transform  tx}}
     (compose-child child transition-state)]))

(defn transit-transform
  [transition]
  (if transition
    (let [{:keys [duration delay ease]
           :or   {ease "linear"}} transition]
      (transit "transform" duration ease delay))
    "none"))

(defn mask
  [{:keys [width height transition-state transitions initial-transform final-transform]}]
  (let [{:keys [enter leave]} transitions]
    [:div.rect-mask
     {:style (merge {:width width :height height}
                    (condp = transition-state
                      "entering" {:transform final-transform    :transition (transit-transform enter)}
                      "entered"  {:transform final-transform    :transition "none"}
                      "exiting"  {:transform initial-transform  :transition (transit-transform leave)}
                      "exited"   {:transform initial-transform  :transition (transit-transform leave)}))}]))

(defn opens-horiz
  [{:keys [child
           transition-state
           width
           height

           offset

           z

           transitions]
    :or {offset (/ width 2)
         ease   "linear"}}]
    [:div
     {:style {:position "absolute" :overflow "hidden" :width width :height height}}
     child
     [mask {:transition-state  transition-state
            :width             width
            :height            height
            :transitions       transitions
            :initial-transform (translate (- offset width) 0 (+ z .000001))
            :final-transform   (translate (- width) 0 (+ z .000001))}]

     [mask {:transition-state  transition-state
            :width             width
            :height            height
            :transitions       transitions
            :initial-transform (translate offset 0 (+ z .000002))
            :final-transform   (translate width 0 (+ z .000002))}]])

(defn opens-down
  [{:keys [child
           transition-state
           width
           height

           z

           transitions]
    :or {z     0}}]
  [:div
   {:style {:position "absolute" :overflow "hidden" :width width :height height}}
   child
   [mask {:transition-state  transition-state
          :width             width
          :height            height
          :transitions       transitions
          :initial-transform (translate 0 0 z)
          :final-transform   (translate 0 height z)}]]);;