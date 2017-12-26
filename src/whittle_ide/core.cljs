(ns whittle-ide.core
  (:require [cljs.pprint :refer [pprint] :as pprint]

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

            [whittle-ide.anim :as anim]

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
                         :tidy-tree (time
                                      (tidy tree
                                            :id-fn        tree-index
                                            :branch?      hiccup-tree?
                                            :gap          10
                                            :children     rest
                                            :labels       tree-labels
                                            :measurements tree-measures))
                         :tree-labels {}
                         :measurements {})
                   :dispatch [:prepare-drawing]})))

(reg-sub :debug identity)
(reg-sub :labels (fn [db] (get db :tree-labels)))
(reg-sub :tidy-tree (fn [db] (get db :tidy-tree)))
(reg-sub :paint-list (fn [db] (get db :paint-list)))


(reg-sub :drawing (fn [db] (get db :drawing)))

(reg-event-db :prepare-drawing
              (fn [{:keys [tidy-tree] :as db}]
                (assoc db
                  :drawing {:ticks (tidy/choreograph tidy-tree)
                            :rects (tidy/plot tidy-tree
                                              :edge-stroke 5
                                              :edge-height 10)})))

(defn draw-rect
  [child {:keys [x y width height]}]
  (println y)
  [:div {:style {:transform (anim/translate x y 0)
                 :position  "absolute"
                 :width     width
                 :height    height}}
   child])

(defn draw-edge
  [{:keys [width height]}]
  [:div.edge {:style {:width     width
                      :height    height}}])

(defn choreograph-parts
  [ticks {:keys [id children]} {:keys [stem branch]}]
  (let [tick (get ticks id)]
    (merge {:root {:delay    tick
                   :duration 0.25}
            :body {:delay    (+ tick 0.25)
                   :duration 0.5}}
           (when (seq children)
             (let [[{:keys [id]} & other-children] children
                   child-tick   (get ticks id)]
               (if (seq other-children)
                 (let [slen         (:height stem)
                       blen         (:width branch)
                       tlen         (+ slen blen) ;;
                       stemdur      (* 0.25 (/ slen tlen))
                       branchdur    (* 0.25 (/ blen tlen))]
                   {:stem   {:delay    (- child-tick stemdur branchdur)
                             :duration stemdur}
                    :branch {:delay    (- child-tick branchdur)
                             :duration branchdur}})
                 {:stem {:delay (- child-tick 0.25) :duration .25}}))))))

(def tick-len 600) ;

(defn wrap-part
  [id choreo transition part-id part child]
  (let [duration (* tick-len (get-in choreo [part-id :duration]))
        delay    (* tick-len (get-in choreo [part-id :delay]))]
    [anim/moves (assoc part
                  :id      [id part-id]
                  :timeout (+ delay duration)
                  :child   [transition (assoc part
                                         :child    child
                                         :duration duration
                                         :delay    delay)])]))

(defn animate-node
  [ticks [{:keys [id label] :as node} {:keys [root body stem branch] :as parts}]]
  (let [choreo (choreograph-parts ticks node parts)]
    [(when root
       (wrap-part id choreo anim/opens-down :root root [draw-edge root]))
     (wrap-part id choreo anim/opens-down :body body label)
     (when stem
       (wrap-part id choreo anim/opens-down :stem stem [draw-edge stem]))
     (when branch
       (wrap-part id choreo anim/opens-horiz :branch branch [draw-edge branch]))]))

(defn animate
  []
  (let [drawing (subscribe [:drawing])]
    (fn []
      [anim/run-transitions {:timeout-fn :timeout}
       (when-let [d @drawing]
         (let [{:keys [ticks rects]} d]
           (doall
             (->> rects
                  (mapcat (partial animate-node ticks))
                  (filter some?)))))])))

(defn measure
  [& {:keys [child on-measure]}]
  (reagent/create-class
    {:component-did-mount
     (fn [owner]
       (let [dom  (reagent/dom-node owner)
             rect (.getBoundingClientRect dom)]
         (on-measure [(.-width rect) (.-height rect)])))
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
          {:style {:position "absolute"}}
          [animate]]])))

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

(reg-event-fx :register-states
              (fn [{:keys [db]} [_ states]]
                {:db (assoc db
                       :state  0
                       :states (vec states))
                 :dispatch [:make-tidy (first states) (label-tree (first states))]}))

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
        (if-let [state @state]
          [:div
           [:div {:style {:float "right"}}
            [:a {:href "#"
                :on-click #(do (.preventDefault %) (dispatch [:rewind-state]))}
            "prev"]
           [:a {:href "#"
                :on-click #(do (.preventDefault %) (dispatch [:advance-state]))}
            "next"]]
           [:pre [:code program-1]]
           [:div [draw-tree @state-idx state]]]
          [:div "..."])])))

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )