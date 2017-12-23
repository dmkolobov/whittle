(ns whittle-ide.anim
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
       :timeout   {"enter" (timeout-fn opts)
                   "exit"  (timeout-fn opts)}
       :on-enter  #(.-scrollTop %)
       :appear    true}
       (fn [state]
         (reagent/as-element [anim (assoc opts :state state)]))])])

(defn snake
  [{:keys [x]}]
  (let [state (atom {:x    x
                     :el   "none"
                     :mask "none"})]
    (reagent/create-class
     {:component-will-receive-props
      (fn [this]
        (let [{:keys [x duration delay]} (reagent/props this)]
          (swap! state
               (fn [old-state]
                 (cond (< x (:x old-state))
                       {:x    x
                        :el   (transit "transform" (/ duration 2) "linear" delay)
                        :mask (transit "transform" (/ duration 2) "linear" (+ delay (/ duration 2)))}

                       (> x (:x old-state))
                       {:x    x
                        :el   (transit "transform" (/ duration 2) "linear" (+ delay (/ duration 2)))
                        :mask (transit "transform" (/ duration 2) "linear" delay)}

                       :default (do (println "same") old-state))))))
      :reagent-render
      (fn [{:keys [style
                   x
                   y
                   width
                   height]}]
        [:div
         [:div.rect-child
          {:style (merge style
                        {:height     height
                         :transform  (translate x y 0)
                         :width      2000
                         :transition (get @state :el)})}]
         [:div.rect-mask
          {:style {:transform  (translate (+ x width) y 0)
                   :width      2000
                   :transition (get @state :mask)}}]])})))

(defn compose-child
  [child state]
  (if (and (vector? child)
           (map? (second child)))
    (update child 1 assoc :state state)
    child))

(defn moves
  [{:keys [child
           state
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
     (compose-child child state)]))

(defn mask
  [{:keys [width height state transition initial-transform final-transform]}]
  [:div.rect-mask
   {:style (merge {:width width :height height}
                  (condp = state
                    "entering" {:transform final-transform    :transition transition}
                    "entered"  {:transform final-transform    :transition "none"}
                    "exiting"  {:transform initial-transform  :transition transition}
                    "exited"   {:transform initial-transform  :transition "none"}))}])

(defn opens-horiz
  [{:keys [child
           state
           width
           height

           offset

           z

           duration
           ease
           delay]
    :or {offset (/ width 2)
         ease   "linear"}}]
  (let [transition (transit "transform" duration ease delay)]
    [:div
     {:style {:position "absolute" :overflow "hidden" :width width :height height}}
     child
     [mask {:state             state
            :width             width
            :height            height
            :transition        transition
            :initial-transform (translate (- offset width) 0 (+ z .000001))
            :final-transform   (translate (- width) 0 (+ z .000001))}]

      [mask {:state             state
             :width             width
             :height            height
             :transition        transition
             :initial-transform (translate offset 0 (+ z .000002))
             :final-transform   (translate width 0 (+ z .000002))}]]))

(defn opens-down
  [{:keys [child
           state
           width
           height

           z

           duration
           ease
           delay]
    :or {ease  "linear"
         delay 0
         z     0}}]
    [:div
     {:style {:position "absolute" :overflow "hidden" :width width :height height}}
     child
     [mask {:state             state
            :width             width
            :height            height
            :transition        (transit "transform" duration ease delay)
            :initial-transform (translate 0 0 z)
            :final-transform   (translate 0 height z)}]])