(ns tidy-tree.reagent
  (:require [cljsjs.react-transition-group]

            ["react-transition-group/TransitionGroup" :as TransitionGroup]
            ["react-transition-group/Transition" :as Transition]
            ["react-transition-group/CSSTransition" :as CSSTransition]

            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [subscribe
                                   dispatch
                                   reg-sub
                                   reg-event-db
                                   reg-event-fx]]

            [tidy-tree.util :refer [node-seq diff-map]]
            [tidy-tree.layout :refer [->tidy get-labels layout]]
            [tidy-tree.plot :refer [plot]]
            [tidy-tree.timeline :refer [choreograph]]

            [tidy-tree.anim :as anim]
            [clojure.set :as set]))

;; ---- events ------------------------------------------------

(defn entering-nodes
  [old-nodes new-nodes]
  (set (remove (set (map :id old-nodes))
               (map :id new-nodes))))

(defn leaving-nodes
  [old-nodes new-nodes]
  (set (remove (set (map :id new-nodes))
               (map :id old-nodes))))

;; Initializes DB state for laying out tree. Puts the tidy tree
;; component into the measuring state.
(reg-event-db
  ::init
  (fn [db [_ new-tree opts]]
    (update db
            ::tidy
            (fn [{:keys [tree] :as tidy-db}]
              (let [new-tidy-tree  (->tidy new-tree opts)
                    old-nodes      (node-seq tree)
                    new-nodes      (node-seq new-tidy-tree)]
                (assoc tidy-db
                  :opts      opts
                  :tree      new-tidy-tree
                  :prev-tree new-tree
                  :labels    (get-labels new-tidy-tree)
                  :measures  {}
                  :lifecycle {:entering (entering-nodes old-nodes new-nodes)
                              :leaving  (leaving-nodes old-nodes new-nodes)}))))))
;
(defn ready-for-layout?
  "Returns true if all nodes have been measured and labeled."
  [db]
  (let [{:keys [labels measures]} (::tidy db)]
    (= (set (keys labels))
       (set (keys measures)))))

;; Records the dimensions of a tree node. If all nodes have been
;; measured, removes the component from the measuring state.
(reg-event-fx
  ::measure
  (fn [{:keys [db]} [_ id dimensions]]
    (let [next-db (assoc-in db [::tidy :measures id] dimensions)]
      (if (ready-for-layout? next-db)
        {:db       (update next-db ::tidy dissoc :labels)
         :dispatch [::layout]}
        {:db next-db}))))

;; Applies the layout algorithm to the tree and dispatches the
;; ::prepare event.
(reg-event-fx
  ::layout
  (fn [{:keys [db]} _]
    (let [{:keys [tree measures opts]} (::tidy db)]
      {:db       (assoc-in db [::tidy :layout-tree] (time (layout tree measures opts)))
       :dispatch [::plot]})))

;; Converts the layout returned by 'tidy' into rectangles representing
;; nodes and edges.
(reg-event-fx
  ::plot
  (fn [{:keys [db]} _]
    (let [{:keys [layout-tree opts]} (::tidy db)]
      {:db       (assoc-in db [::tidy :plot] (plot layout-tree opts))
       :dispatch [::choreograph]})))

;; Defines a timeline for entering/exiting/moving of nodes and edges
;; for animation

(reg-event-db
  ::choreograph
  (fn [db _]
    (let [{:keys [layout-tree lifecycle plot]} (::tidy db)]
      (assoc-in db
                [::tidy :drawing]
                {:plot plot
                 :timeline (choreograph layout-tree plot lifecycle)}))))


;; ---- subscriptions ----------------------------------------------

(reg-sub
  ::labels-to-measure
  (fn [db] (get-in db [::tidy :labels])))

(defn drawing-to-draw
  "Returns a map containing :rects and :enter-times whenever the layout has
  been plotted and choreographed. Returns nil otherwise."
  [db]
  (let [tidy-db (::tidy db)]
    (when (contains? tidy-db :drawing)
      (:drawing tidy-db))))

(reg-sub ::drawing-to-draw drawing-to-draw)

;; ---- rendering --------------------------------------------------

(defn measure
  [& {:keys [child on-measure]}]
  (reagent/create-class
    {:component-did-mount
                     (fn [owner]
                       (let [dom  (reagent/dom-node owner)
                             rect (.getBoundingClientRect dom)]
                         (on-measure [(.ceil js/Math (.-width rect))
                                      (.ceil js/Math (.-height rect))])))
     :reagent-render (fn [& _] child)}))

(defn measure-labels
  [labels]
  [:div.hidden
   (for [[index label] labels]
     ^{:key index}
     [measure :child     label
      :on-measure #(dispatch [::measure index %])])])

(defn draw-edge
  [{:keys [width height]}]
  [:div.edge {:style {:width     width
                      :height    height}}])

(def enter-len 600)

(defn wrap-part
  [id timeline transition part-id part child]
  (let [duration (* enter-len (get-in timeline [id part-id :duration]))
        delay    (* enter-len (get-in timeline [id part-id :delay]))]
    [anim/moves (assoc part
                  :id      [id part-id]
                  :timeout (+ delay duration)
                  :child   [transition (assoc part
                                         :child    child
                                         :duration duration
                                         :delay    delay)])]))

(defn animate-node
  [timeline [{:keys [id label] :as node} {:keys [root body stem branch] :as parts}]]
  [(when root
     (wrap-part id timeline anim/opens-down :root root [draw-edge root]))
   (wrap-part id timeline anim/opens-down :body body label)
   (when stem
     (wrap-part id timeline anim/opens-down :stem stem [draw-edge stem]))
   (when branch
     (wrap-part id timeline anim/opens-horiz :branch branch [draw-edge branch]))])

(defn draw-tree
  [_]
  (fn
  [drawing]
  [anim/run-transitions {:timeout-fn :timeout}
   (doall
     (->> (:plot drawing)
          (mapcat (partial animate-node (:timeline drawing)))
          (filter some?)))]))

(reg-sub ::prev-tree
         (fn [db] (get-in db [::tidy :prev-tree])))

(defn tidy-tree
  [tree opts]
  (let [labels-to-measure (subscribe [::labels-to-measure])
        drawing-to-draw   (subscribe [::drawing-to-draw])
        prev-tree         (subscribe [::prev-tree])]

    (fn [tree opts]
      (when (not= @prev-tree tree) (dispatch [::init tree opts]))
      [:div
       [draw-tree @drawing-to-draw]

       (when-let [labels @labels-to-measure]
         [measure-labels labels])])))