(ns whittle-ide.core
  (:require [cljs.pprint :refer [pprint] :as pprint]
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
;;

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
                (cond (playback-node? result) [:pre (print-code (:result result))]
                      (hiccup-tree? result)   [:pre (name (first result))]
                      :default                [:pre (print-code result)])]))
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
                  :tidy-tree     nil
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
                                     :key-fn       tree-index
                                     :labels       tree-labels
                                     :measurements tree-measures)))))

(reg-sub :debug identity)
(reg-sub :labels (fn [db] (get db :tree-labels)))
(reg-sub :tidy-tree (fn [db] (get db :tidy-tree)))

(defn render-tidy
  [{:keys [delta children shift label width height level lcontour rcontour overlap]
    :or   {delta 0}}]
  [:div.tidy-tree
   [:div.node
    {:on-click  #(println "dimensions" width  height
                          ;"overlap" overlap
                          "delta" delta
                          "shift" shift
                          "center" (tidy/contour-center (dissoc lcontour level)
                                                        (dissoc rcontour level)))
     :style {:width width
             :transform (str "translate("delta"px,"(* level 50)"px)")}}
    label]
   (doall
     (map #(vector render-tidy (update % :delta + delta (- shift))); shift (- delta)))
          children))])

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
     [measure :child     label
              :on-measure #(dispatch [:measure-tree index %])])])

(defn draw-tree
  [index tree]
  (let [labels    (subscribe [:labels])
        tidy-tree (subscribe [:tidy-tree])]
    (fn [index tree]
      ^{:key index}
      [:div.tree {:key index}
       ;[:pre (print-code tree)]
       (if-let [tidy-tree @tidy-tree]
         [:div.tidy-tree
          {:style {:transform "translateX(400px)"}}
          ^{:key index} [render-tidy tidy-tree]]
         (when-let [labels @labels] [:div.hidden [measure-labels labels]]))])))

(reg-sub :state
         (fn [{:keys [state states] :as db}]
           (println "state" state)
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

(defn hello-world
  []
  (let [state (subscribe [:state])
        state-idx (subscribe [:state-idx])]
    (dispatch [:register-states (playback (inspect clj-lang program-1))])
    (fn []
      (println @state-idx)
      [:div
        (if-let [state @state]
          [:div
           [:a {:href "#"
                :on-click #(do (.preventDefault %) (dispatch [:rewind-state]))}
            "prev"]
           [:a {:href "#"
                :on-click #(do (.preventDefault %) (dispatch [:advance-state]))}
            "next"]
           [:div ^{:key @state-idx} [draw-tree @state-idx state]]]
          [:div "..."])])))

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )