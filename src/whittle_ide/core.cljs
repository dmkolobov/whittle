(ns whittle-ide.core
  (:require [cljs.pprint :refer [pprint] :as pprint]
            ;[cljsjs.react]
            ;[cljsjs.react-dom]
            [cljsjs.react-transition-group]

            ["react-transition-group/TransitionGroup" :as TransitionGroup]
            ["react-transition-group/Transition" :as Transition]
            ["react-transition-group/CSSTransition" :as CSSTransition]

            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [subscribe
                                   dispatch
                                   reg-sub
                                   reg-event-db
                                   reg-event-fx]]

            [whittle.core :refer [whittle]]
            [whittle.paint-rect :as paint-rect]
            [whittle.lang.arithmetic :refer [lang clj-lang]]
            [whittle.inspect :refer [inspect playback index-tree tree-index playback-node? hiccup-tree?]]
            [whittle-ide.tidy :refer [tidy] :as tidy]
            [clojure.zip :as zip]
            [clojure.set :as set]
            [clojure.string :as string]))

(enable-console-print!)
;;;;
(defn print-code
  [code]
  (pprint/with-pprint-dispatch pprint/code-dispatch
                               (string/trim
                                 (with-out-str
                                   (pprint code)))))

(def program-1
  "let square(x) ={x*x},
       sumsq(a,b)={square(a)+square(b)}:
     sumsq(2,3)")

(def program-2 "2+2+2")

(defn label-tree
  [result]
  (into {}
        (map (fn [[index result]]
               [index
                (cond (playback-node? result) ^{:key "result"} [:pre [:code (print-code (:result result))]]
                      (hiccup-tree? result)   ^{:key "node"} [:pre.black [:code (name (first result))]]
                      :default                ^{:key "term"} [:pre [:code (print-code result)]])]))
        (index-tree result)))

(defn all-measured?
  [labels measures]
  (let [l (set (keys labels))
        m (set (keys measures))]
    (= l m)))

(reg-event-db :make-tidy
              (fn [db [_ tree labels]]
                (assoc db
                  :tree          tree
                  :tree-labels   labels
                  :tree-measures {})))

(reg-event-fx :measure-tree
              (fn [{:keys [db]} [_ index bounds]]
                (let [{:keys [tree-labels tree-measures] :as db'}
                      (assoc-in db [:tree-measures index] bounds)]
                  (if (all-measured? tree-labels tree-measures)
                    {:db db' :dispatch [:layout-tree]}
                    {:db db'}))))

(reg-event-db :layout-tree
              (fn [db]
                (let [{:keys [tree tree-labels tree-measures]} db]
                  (assoc db
                    :tidy-tree (tidy tree
                                     :id-fn       tree-index
                                     :labels       tree-labels
                                     :measurements tree-measures)
                    :tree-labels  {}
                    :measurements {}))))

(reg-sub :debug identity)
(reg-sub :labels (fn [db] (get db :tree-labels)))
(reg-sub :tidy-tree (fn [db] (get db :tidy-tree)))

(defn translate
  [x y]
  {:style {:transform (str "translate("x"px,"y"px)")}})

(defn render-node
  [{:keys [label width x y height]}]
  [:div.node
   (translate x y) label])

(defn view-box
  [points & {:keys [min-width min-height]}]
  (let [xs     (map first points)
        ys     (map second points)
        top    (apply min ys)
        right  (+ (apply max xs) 5)
        bottom (apply max ys)
        left   (- (apply min xs) 5)]
    {:top    (if (= top bottom) (+ top (/ min-height -2)) top)
     :left   (if (= left right) (+ left (/ min-width -2)) left)
     :bottom  (if (= top bottom) (+ top (/ min-height 2)) bottom)
     :right (if (= left right) (+ left (/ min-width 2)) right)}))

(defn parent-connection
  [{:keys [x y width height]}]
  [(+ x (/ width 2)) (+ y height)])

(defn child-connection
  [{:keys [x y width]}]
  [(+ x (/ width 2)) y])

(defn render-segment
  [x y child]
  (let [[child-x child-y] (child-connection child)]
    [:path
     {:stroke "#000"
      :fill "none"
      :stroke-width 1
      :vector-effect "non-scaling-stroke"
      :d (string/join " "
                      ["M" x y
                       "L" child-x y
                       "L" child-x child-y])}]))

(defn render-edges
  [{:keys [width height x y children] :as node}]
  (let [parent-conn (parent-connection node)
        child-conns (map child-connection children)
        {:keys [top left bottom right] :as vb} (view-box (conj child-conns parent-conn)
                                                         :min-width 10)
        width  (- right left)
        height (- bottom top)
        [parent-x parent-y] parent-conn]
    [:svg
     (-> (translate left top)
         (assoc :width  width
                :height height
                :viewBox (string/join " " [left top (- right left) (- bottom top)]))
         (update :style
                 assoc
                 :position         "absolute"
                 :background-color "rgba(0,0,0,0)"))
     [:path
      {:stroke "#000"
       :fill "none"
       :stroke-width 1
       :d (string/join " "
                       ["M" parent-x parent-y
                        "L" parent-x parent-y
                        "L" parent-x (+ parent-y (/ height 2))])}]
     (doall
       (for [{:keys [id] :as child} children]
         ^{:key id}
         [render-segment parent-x (+ parent-y (/ height 2)) child]))]))
   ;  (doall
   ;    (for [{:keys [id] :as child} children]
   ;      (let [[child-x child-y]   (child-connection child)]
   ;        ^{:key (str "edge:" id)}
   ;        [:line
   ;         {:stroke "#000"
   ;          :stroke-width 1
   ;          :x1 parent-x
   ;          :y1 parent-y
   ;          :x2 child-x
   ;          :y2 child-y}])))]))

(def edge-stroke 1)

(defn box
  [points & {:keys [margin-top margin-bottom margin-left margin-right]
             :or   {margin-top 0
                    margin-bottom 0
                    margin-left 0
                    margin-right 0}}]
  (let [xs    (map first points)
        ys    (map last points)
        min-x (apply min xs)
        min-y (apply min ys)
        max-x (apply max xs)
        max-y (apply max ys)]
    {:top    (- min-y margin-top)
     :bottom (+ max-y margin-bottom)
     :left   (- min-x margin-left)
     :right  (+ max-x margin-right)
     :width  (+ (- max-x min-x) margin-left margin-right)
     :height (+ (- max-y min-y) margin-top margin-bottom)}))

(defn ts
  [x y]
  (str "translate("x"px,"y"px)"))

(defn transit
  [duration ease delay]
  (str "transform " duration "s " ease " " delay "s"))

(defn masked
  [& {:keys [x
             y
             width
             height
             component
             child
             duration ease delay]
      :or {component :div}
      :as spec}]
  [component
   {:style {:transform   (translate x y)
            :transition  (transit duration ease delay)}}
   [component
    {:style {:transform  (translate (- x) (- y))
             :transition (transit duration ease delay)}}
    child]])

(defn move
  [x y]
  {:style {:transform (paint-rect/translate x y)}})

(def stem-stroke 4)
(def stem-height 7)

(def v-line
  [:svg.node
   {:width stem-stroke :height "1000"}
   [:line {:stroke-width stem-stroke
           :x1 (/ stem-stroke 2) :y1 0
           :x2 (/ stem-stroke 2) :y2 1000}]])

(def h-line
  [:svg.node
   {:width "1000" :height stem-stroke}
   [:line {:stroke-width stem-stroke
           :y1 (/ stem-stroke 2) :x1 0
           :y2 (/ stem-stroke 2) :x2 1000}]])


(defn node->rects
  [baseline {:keys [id y width height children label level]
             :or   {y 0}
             :as   node}]
  (let []
    (->> [(when (not= level 0)
            {:id     [:node-root id]
             :width  stem-stroke
             :height (+ stem-height y)
             :child  v-line})
          {:id     [:node-body id]
           :width  width
           :height height
           :child  label}
          (when (seq children)
            (let [cxs   (map paint-rect/center-x children)
                  min-x (apply min cxs)
                  max-x (apply max cxs)
                  width (- max-x min-x)]
              [{:id     [:node-stem-v id]
                :width  stem-stroke
                :height stem-height
                :child  v-line}
               {:id     [:node-stem-h id]
                :width  (+ width stem-stroke)
                :height stem-stroke
                :child  h-line}]))]
         (flatten)
         (filter some?)
         (paint-rect/center (paint-rect/center-x node))
         (paint-rect/space baseline))))

(defn draw-tidy
  [tidy]
  (let [rows  (group-by :level (tidy/node-seq tidy))
        plist (loop [baseline 0
                     rows     rows
                     rects    []]
                (if (seq rows)
                  (let [[_ nodes] (first rows)
                        row-rects (mapcat #(node->rects baseline %) nodes)]
                    (recur (apply paint-rect/find-baseline row-rects)
                           (rest rows)
                           (into rects row-rects)))
                  (paint-rect/paint-list rects)))]
    (println (map :id plist))
    [:div
     (for [r plist]
       ^{:key (:id r)}
       [paint-rect/paint-rect r])]))

(defn render-node-anim
  [{:keys [x y width height label]} state]
  (reagent/as-element
    [:div.node
     (update (translate x y)
             :style
             assoc
             :position "absolute")
     label
     [:div.mask
      (update (condp = state
                        "entering" (translate (- width) 0)
                        "entered"  (translate (- width) height)
                        "exiting"  (translate (- width) 0)
                        "exited"   (translate (- width) 0))
              :style
              assoc
              :transition (condp = state
                            "entering" "none"
                            "entered"  "transform .2s ease-in"
                            "exiting"  "transform .2s ease-in"
                            "exited"   "none")
              :position "absolute"
              :width    width
              :height   height
              :display "inline-block")]]))

(def render-n
  (reagent/reactify-component render-node-anim))


(defn render
  [tidy]
  (let [max-level (max (count (:lcontour tidy)) (count (:rcontour tidy)))
        nodes     (tidy/node-seq tidy)
        nodes     (->> nodes
                       (group-by :level)
                       (sort-by first)
                       (vals)
                       (apply concat))]
    [:div
     [:div.nodes
      [:> TransitionGroup
       {:component "div"}
       (doall
        (for [{:keys [id level] :as node} nodes]
          ^{:key id}
          [:> Transition
           {:timeout     {"enter"  (* (inc level) 200 2)
                          "exit"   (* (inc (- max-level level)) 200 2)}
            :appear      true}
           (partial render-node-anim node)]))]]
     [:div.edges
      (doall
        (for [{:keys [id level] :as node} nodes]
          ^{:key id}
          [render-edges node]))]]))

(defn measure
  [& {:keys [child on-measure]}]
  (reagent/create-class
    {:component-did-mount
     (fn [owner]
       (let [dom  (reagent/dom-node owner)
             rect (.getBoundingClientRect dom)]
         (on-measure [(.-offsetWidth dom) (.-offsetHeight dom)])))
     :reagent-render (fn [& _] child)}))

(defn measure-labels
  [labels]
  [:div
   (for [[index label] labels]
     ^{:key index}
     [measure :child     label
              :on-measure #(dispatch [:measure-tree index %])])])

(defn draw-tree
  [index tree]
  (let [labels    (subscribe [:labels])
        tidy-tree (subscribe [:tidy-tree])]
    (fn [tree]
      [:div.tree
       (when-let [labels @labels]
         ^{:key index} [:div.hidden [measure-labels labels]])
       (when-let [tidy-tree @tidy-tree]
         [:div.tidy-tree
          {:style {:position "relative"
                   :transform "translateX(400px)"}}
          [draw-tidy tidy-tree]])])))

(reg-sub :state
         (fn [{:keys [state states] :as db}]
           (get states state)))

(reg-event-fx :advance-state
              (fn [{:keys [db]}]
                (let [idx  (inc (:state db))
                      tree (get-in db [:states idx])]
                  {:db       (assoc db :state idx)
                   :dispatch [:make-tidy tree (label-tree tree)]})))

(reg-event-fx :rewind-state
              (fn [{:keys [db]}]
                (let [idx  (dec (:state db))
                      tree (get-in db [:states idx])]
                  {:db       (assoc db :state idx)
                   :dispatch [:make-tidy tree (label-tree tree)]})))

(reg-event-db :register-states (fn [db [_ states]]
                                 (assoc db
                                   :state  0
                                   :states (vec states))))

(reg-sub :state-idx (fn [db] (get db :state)))

(def exp-1
  "(2 + (5 - 4) * (2 - 2) + 4) * 5 + 6 + 7")

(defn hello-world
  []
  (let [state (subscribe [:state])
        state-idx (subscribe [:state-idx])]
    (dispatch [:register-states (playback (inspect clj-lang exp-1))])
                                ;(playback (inspect clj-lang program-1))])
    (fn []
      [:div
       [:pre [:code exp-1]]
        (if-let [state @state]
          [:div
           [:a {:href "#"
                :on-click #(do (.preventDefault %) (dispatch [:rewind-state]))}
            "prev"]
           [:a {:href "#"
                :on-click #(do (.preventDefault %) (dispatch [:advance-state]))}
            "next"]
           [:div [draw-tree @state-idx state]]]
          [:div "..."])])))

(defn test-concept
  []
  (let [state (reagent/atom 0)]
    (fn []
      @state
      [:div
       [:a {:href "#" :on-click (fn [_] (swap! state inc))} "shuffle"]
        [:> TransitionGroup
         {:component :ul}
         (doall
           (for [x (->> '[a b c d e] (shuffle) (random-sample 0.5))]
             ^{:key x}
             [:> Transition
              {:timeout     {"enter" 3000
                             "exit"  3000}
               :appear      true}
              (fn [state]
                (reagent/as-element
                  [:li
                   {:style {:opacity (if (= state "entered")
                                        1
                                        0)
                            :transition "opacity 1s ease-in"}}
                   (str x)]))]))]])))


(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )