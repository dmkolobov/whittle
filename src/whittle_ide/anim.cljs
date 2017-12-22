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