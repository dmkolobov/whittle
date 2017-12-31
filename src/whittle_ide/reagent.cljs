(ns whittle-ide.reagent
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

            [whittle-ide.tidy :as tidy]
            [whittle-ide.anim :as anim]))

;; ---- events ------------------------------------------------

;; Initializes DB state for laying out tree. Puts the tidy tree
;; component into the measuring state.
(reg-event-db
  ::init
  (fn [db [_ tree opts]]
    (let [tidy-tree (tidy/->tidy tree opts)
          labels    (tidy/get-labels tidy-tree)]
      (println "init")
      (update db
              ::tidy
              assoc
              :opts     opts
              :tree     tidy-tree
              :labels   labels
              :measures {}))))

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
      {:db       (assoc-in db [::tidy :tree] (time (tidy/tidy tree measures opts)))
       :dispatch [::plot]})))

;; Converts the layout returned by 'tidy' into rectangles representing
;; nodes and edges.
(reg-event-fx
  ::plot
  (fn [{:keys [db]} _]
    (let [{:keys [tree opts]} (::tidy db)]
      {:db       (assoc-in db [::tidy :rects] (tidy/plot tree opts))
       :dispatch [::choreograph]})))

;; Defines a timeline for entering/exiting/moving of nodes and edges
;; for animation

(reg-event-db
  ::choreograph
  (fn [db _]
    (let [{:keys [tree]} (::tidy db)]
      (assoc-in db
                [::tidy :ticks]
                (tidy/choreograph tree)))))

;; ---- subscriptions ----------------------------------------------

(reg-sub
  ::labels-to-measure
  (fn [db] (get-in db [::tidy :labels])))

(defn drawing-to-draw
  "Returns a map containing :rects and :ticks whenever the layout has
  been plotted and choreographed. Returns nil otherwise."
  [db]
  (let [tidy-db (::tidy db)]
    (when (and (contains? tidy-db :rects)
               (contains? tidy-db :ticks))
      (select-keys tidy-db [:rects :ticks]))))

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

(defn choreograph-parts
  [ticks {:keys [id children]} {:keys [stem branch]}]
  (let [tick (get ticks id)]
    (merge {:root {:delay tick          :duration 0.25}
            :body {:delay (+ tick 0.25) :duration 0.5}}
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

(defn draw-tree
  [{:keys [rects ticks]}]
  [anim/run-transitions {:timeout-fn :timeout}
   (doall
     (->> rects
          (mapcat (partial animate-node ticks))
          (filter some?)))])

(defn tidy-tree
  [tree opts]
  (let [labels-to-measure (subscribe [::labels-to-measure])
        drawing-to-draw   (subscribe [::drawing-to-draw])]

    ;; kickstart layout algorithm
    (dispatch [::init tree opts])

    (fn [_ _]
      [:div 
       (when-let [labels @labels-to-measure]
         [measure-labels labels])

       (when-let [drawing @drawing-to-draw]
         [draw-tree drawing])])))