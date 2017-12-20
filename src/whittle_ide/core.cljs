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
            [whittle.paint-rect :as paint-rect :refer [center]]
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
                (cond (playback-node? result) [:pre.prect [:code (print-code (:result result))]]
                      (hiccup-tree? result)   [:pre.prect.black [:code (name (first result))]]
                      :default                [:pre.prect [:code (print-code result)]])]))
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

(reg-event-fx :layout-tree
              (fn [{:keys [db]}]
                (let [{:keys [tree tree-labels tree-measures]} db]
                  {:db (assoc db
                         :tidy-tree (tidy tree
                                          :id-fn tree-index
                                          :labels tree-labels
                                          :measurements tree-measures)
                         :tree-labels {}
                         :measurements {})
                   :dispatch [:reflow-tree]})))


(def stem-stroke 5)
(def stem-height 7)

(def black-h [:div.black-h])
(def black-v [:div.black-v])

(def duration 500)

(defn node->rects
  [{:keys [id y width height children label level]
     :or   {y 0}
     :as   node}]
  (let [cx    (paint-rect/center-x node)]
    [node
     (merge (when (not= level 0)
             {:root (center cx
                            {:width      stem-stroke
                             :height     (+ stem-height y)
                             :child      black-v
                             :fix-width? true})})

           {:node (center cx
                          {:width  width
                           :height height
                           :child  label})}

          (when (seq children)
            (let [cxs   (map paint-rect/center-x children)
                  min-x (apply min cxs)
                  max-x (apply max cxs)
                  width (- max-x min-x)]
              {:stem   (center cx
                               {:width      stem-stroke
                                :fix-width? true
                                :height     stem-height
                                :child      black-v})

               :branch {:x           (- min-x (/ stem-stroke 2))
                        :width       (+ width stem-stroke)
                        :height      stem-stroke
                        :fix-height? true
                        :child       black-h}})))]))

(defn choreograph-node
  [{:keys [id child-tick tick children]} {:keys [root node stem branch] :as parts}]
  (let [start-time  (* duration tick)
        child-start (when (some? child-tick)
                      (* duration child-tick))]
    (println "choreo" id)
    (cond-> []
            root     (conj (assoc root
                             :id [id :root]
                             :timeout (+ start-time (* 0.25 duration))))
            node     (conj (assoc node
                             :id [id :node]
                             :timeout (+ start-time (* 0.5 duration))))
            (seq children) (conj (assoc stem
                                   :id [id :stem]
                                   :timeout (- child-start (* 0.125 duration)))
                           (assoc branch
                             :id [id :branch]
                             :timeout (- child-start (* 0.125 duration)))))))

(defn tree->nodes
  [tidy-tree]
  (let [ticks (tidy/choreograph tidy-tree)]
    (->> (tidy/node-seq tidy-tree)
         (map (fn [{:keys [children] :as node}]
                (assoc node
                  :tick       (get ticks (:id node))
                  :child-tick (when (seq children)
                                (get ticks (:id (first children)))))))
         (group-by :level))))

(reg-event-db :reflow-tree
              (fn [{:keys [tidy-tree] :as db}]
                (loop [baseline 0
                       rows     (tree->nodes tidy-tree)
                       rects    []]
                  (if (seq rows)
                    (let [[level nodes] (first rows)
                          row-rects (->> nodes
                                         (sort-by :x)
                                         (map node->rects)
                                         (map (partial apply choreograph-node))
                                         (mapcat (partial paint-rect/space baseline))
                                         (map #(assoc % :level level)))]
                      (recur (apply paint-rect/find-baseline row-rects)
                             (rest rows)
                             (into rects row-rects)))
                    (assoc db
                      :paint-list (paint-rect/paint-list rects))))))

(reg-sub :debug identity)
(reg-sub :labels (fn [db] (get db :tree-labels)))
(reg-sub :tidy-tree (fn [db] (get db :tidy-tree)))
(reg-sub :paint-list (fn [db] (get db :paint-list)))


(defn draw-tidy
  []
  (let [paint-list (subscribe [:paint-list])]
    (fn []
      (when-let [plist @paint-list]
        (println "plist type" (type plist))
        [paint-rect/paint plist]))))

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
  (let [labels    (subscribe [:labels])]
    (fn [tree]
      [:div.tree
       (when-let [labels @labels]
         [:div.hidden [measure-labels labels]])
       [:div.tidy-tree
          {:style {:position "absolute"
                   :transform "translateX(400px)"}}
          [draw-tidy]]])))

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
    (dispatch [:register-states ;(playback (inspect clj-lang exp-1))])
                                (playback (inspect clj-lang program-1))])
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