(ns whittle-ide.anim
  (:require [reagent.core :as reagent]
            [cljsjs.react-transition-group]

            ["react-transition-group/TransitionGroup" :as TransitionGroup]
            ["react-transition-group/Transition" :as Transition]
            ["react-transition-group/CSSTransition" :as CSSTransition]))

(defn translate
  [x y]
  (str "translate3d("x"px, "y"px, 0)"))

(defn transit
  ([prop duration ease]
   (str prop" "(/ duration 1000)"s "ease))

  ([prop duration ease delay]
   (str (transit prop duration ease) " " (/ delay 1000) "s")))

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

        child-transform    (translate x y)
        mask-transform     (translate x (+ y height))]
    [:div
     {:style {:z-index z}}
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

