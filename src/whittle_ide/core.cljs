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
                (println "making tidy" tree labels)
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
                  (println "made tidy")
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

(defn render-node-anim
  [node state]
  (println "state" state (:id node))
  (reagent/as-element
    [:div {:style {:opacity    (condp = state
                                  "entering" 0
                                  "entered"  1
                                  "exiting"  0.99
                                  "exited"   0)

                   :transition (condp = state
                                 "entering" "none"
                                 "entered"  "opacity 0.2s ease-in"
                                 "exiting"  "opacity 0.2s ease-out"
                                 "exited"   "opacity 0.2s ease-out")}}
   [render-node node]]))

(def render-n
  (reagent/reactify-component render-node-anim))

(defn render
  [tidy]
  (let [max-level (max (count (:lcontour tidy)) (count (:rcontour tidy)))
        nodes     (tidy/node-seq tidy)]
    (println "max-level" max-level)
    [:div
     [:div.nodes
      [:> TransitionGroup
       {:component "div"}
       (doall
        (for [{:keys [id level] :as node} nodes]
          ^{:key id}
          [:> Transition
           {:timeout     {"enter"  (* level 200)
                          "exit"   (* (- max-level level) 200)}
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
          [render tidy-tree]])])))

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
    (println "exp"
             (playback (inspect lang exp-1)))
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

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )