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
            [whittle.lang.arithmetic :refer [lang clj-lang]]
            [whittle.inspect :refer [inspect playback index-tree tree-index playback-node? hiccup-tree?]]

            [tidy-tree.reagent :refer [tidy-tree]]

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

(defn label-ast
  [ast-node]
  (cond (playback-node? ast-node) [:pre.prect [:code (print-code (:result ast-node))]]
        (hiccup-tree? ast-node)   [:pre.prect.black [:code (name (first ast-node))]]
        :default                  [:pre.prect [:code (print-code ast-node)]]))

(reg-sub :state
         (fn [{:keys [state states] :as db}]
           (get states state)))

(reg-event-fx :advance-state
              (fn [{:keys [db]}]
                (let [idx  (inc (:state db))
                      tree (get-in db [:states idx])]
                  {:db       (assoc db :state idx)})))

(reg-event-fx :rewind-state
              (fn [{:keys [db]}]
                (let [idx  (dec (:state db))
                      tree (get-in db [:states idx])]
                  {:db       (assoc db :state idx)})))

(reg-event-fx :register-states
              (fn [{:keys [db]} [_ states]]
                {:db (assoc db
                       :state  0
                       :states (vec states))}))

(reg-sub :state-idx (fn [db] (get db :state)))

(def exp-1
  "(2 + (5 - 4) * (2 - 2) + 4) * 5 + 6 + 7")

(defn hello-world
  []
  (let [state (subscribe [:state])
        state-idx (subscribe [:state-idx])]
    (dispatch [:register-states (playback (inspect clj-lang program-1))])
    (fn []
      [:div
        (if-let [tree @state]
          [:div
           [:div {:style {:float "right"}}
            [:a {:href "#"
                :on-click #(do (.preventDefault %) (dispatch [:rewind-state]))}
            "prev"]
           [:a {:href "#"
                :on-click #(do (.preventDefault %) (dispatch [:advance-state]))}
            "next"]]
           [:pre [:code program-1]]
           [:div [tidy-tree tree
                            {:id-fn    tree-index
                             :label-fn label-ast
                             :branch?  hiccup-tree?
                             :children rest

                             :v-gap    10
                             :h-gap    10
                             :stroke   5}]]]
          [:div "..."])])))

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )