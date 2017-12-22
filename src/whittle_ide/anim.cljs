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
  [animations]
  [:> TransitionGroup
   {:component :div}
   (for [[anim {:keys [z transit-delay duration] :as opts}] animations]
     ^{:key z}
     [:> Transition
      {:component :div
       :timeout   {"enter" (+ transit-delay duration)
                   "exit"  (+ transit-delay duration)}
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
                        :el   (transit "transform" (/ duration 2) "ease-in-out" delay)
                        :mask (transit "transform" (/ duration 2) "ease-in-out" (+ delay (/ duration 2)))}

                       (> x (:x old-state))
                       {:x    x
                        :el   (transit "transform" (/ duration 2) "ease-in-out" (+ delay (/ duration 2)))
                        :mask (transit "transform" (/ duration 2) "ease-in-out" delay)}

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

(defn drops
[{:keys [in
           child
           state
           x
           y
           z
           height
           duration
           ease
           delay
           transit-delay]
    :as opts}]
  (let [default-transition (transit "transform" duration "ease-in-out" delay)
        transit-transition (transit "transform" duration "ease-in-out" transit-delay)

        child-transform    (translate x y (/ z 1000))
        mask-transform     (translate x (+ y height) (/ z 1000))]
    [:div
     [:div.rect-child
      {:style
       {
        :transform  child-transform
        :transition (if (= state "entering")
                      "none"
                      default-transition)}}
      child]
     [:div.rect-mask
      {:class state
       :style (condp = state
                "entering" {:transform  mask-transform
                            :transition transit-transition}

                "entered"  {:transform  mask-transform
                            :transition default-transition}

                "exiting"  {:transform  child-transform
                            :transition transit-transition}

                "exited"   {:transform  child-transform
                            :transition "none"})}]]))