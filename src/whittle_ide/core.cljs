(ns whittle-ide.core
  (:require [cljs.pprint :refer [pprint] :as pprint]
            [reagent.core :as reagent :refer [atom]]
            [whittle.core :refer [whittle]]
            [whittle.lang.arithmetic :refer [lang clj-lang]]
            [whittle.inspect :refer [inspect playback index-tree tree-index playback-node? hiccup-tree?]]
            [whittle-ide.tidy :refer [tidy]]
            [clojure.zip :as zip]))

(enable-console-print!)

(println "This text is printed from src/whittle-ide/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload
;
(defonce app-state (atom {:text "Hello world!"}))

(defn print-code
  [code]
  (pprint/with-pprint-dispatch pprint/code-dispatch
                               (with-out-str (pprint code))))

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
                (cond (playback-node? result) (print-code (:result result))
                      (hiccup-tree? result)   (name (first result))
                      :default                (print-code result))]))
        (index-tree result)))

(def char-width 8)
(def char-height 16)

(defn measure-label
  [[index label]]
  (let [lines     (clojure.string/split label #"\n")
        max-lines (apply max (map count lines))]
    [index [(* char-width max-lines) (* char-height (count lines))]]))

(defn render-tidy
  [{:keys [delta children shift label width height level]
    :or   {delta 0}}]
  [:div
   [:div.node
    {:style { :transform (str "translate("delta"px,"(* level 100)"px)")}}
    [:pre label]
    [:div {:position "absolute"} width height]]
   (doall
     (map #(vector render-tidy (update % :delta - shift (- delta)))
          children))])

(defn draw-tree
  [tree]
  (let [labels       (label-tree tree)
        measurements (into {} (map measure-label) labels)
        tidy-tree    (tidy tree
                           :key-fn       tree-index
                           :measurements measurements
                           :labels       labels)]
    [:div.tree
     [:pre (print-code tree)]
     [:div {:style {:position :relative :transform (str "translateX(400px)")}}
      [render-tidy tidy-tree]]]))

(defn hello-world
  []
  [:div
   [:h1 (:text @app-state)]
   (let [states (playback (inspect clj-lang program-2))]
     (doall (for [tree (take 1 states)] [draw-tree tree])))])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")));

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )