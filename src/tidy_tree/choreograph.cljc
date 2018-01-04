(ns tidy-tree.choreograph
  (:require [clojure.zip :as zip]
            [clojure.set :as set]
            [tidy-tree.timeline :refer [record register-handler run-schedule]]
            [tidy-tree.util :refer [zipper fast-forward rewind zip-seq]]))

(defn enters [loc]  (map :id (conj (zip/rights loc) (zip/node loc))))
(defn leaves [loc]  (map :id (conj (zip/lefts loc) (zip/node loc))))

(defn make-schedule
  [tidy-tree fired-events]
  (let [front   (zipper tidy-tree)
        back    (fast-forward front identity)]
    (record fired-events
            (fn [fire!]
              (rewind back (fn [loc] (fire! :leave (leaves loc)) loc))
              (fire! :move (map :id (zip-seq front)))
              (fast-forward front (fn [loc] (fire! :enter (enters loc)) loc))))))

(defn proportional
  [time & lengths]
  (let [total (reduce + lengths)]
    (map #(* time (/ % total)) lengths)))

(defn skinny-node
  [{:keys [id] :as node} _]
  (let [child-id (:id (first (:children node)))]
    {[:before :enter child-id] [[id :stem :enter 200]]
     [:after  :leave child-id] [[id :stem :leave 200]]}))

(defn fat-node
  [{:keys [id children] :as node} {:keys [stem branch] :as parts}]
  (let [child-id        (:id (first children))
        [s-time b-time] (proportional 200 (:height stem) (:width branch))]
    (println "fat" parts s-time b-time)
    {[:before :enter child-id] [[id :stem   :enter s-time]
                                [id :branch :enter b-time]]

     [:after :leave child-id] [[id :branch :leave b-time]
                               [id :stem   :leave s-time]]}))

(defn rooted-node
  [{:keys [id] :as node} _]
  {[:on :enter id] [[id :root :enter 200]
                    [id :body :enter 250]]

   [:on :leave id] [[id :body :leave 250]
                    [id :root :leave 200]]})

(defn solo-node
  [{:keys [id] :as node} _]
  {[:on :enter id] [[id :body :enter 250]]
   [:on :leave id] [[id :body :leave 250]]})

(defn node-listeners
  [{:keys [id children] :as node} {:keys [stem branch root] :as parts}]
  (let [listeners (merge (if root
                           (rooted-node node parts)
                           (solo-node node parts))
                         (cond (and (seq children)
                                    (seq (rest children))) (fat-node node parts)
                               (seq children)              (skinny-node node parts)))]
    (println (keys listeners))
    listeners))
;;
(defn plot-listeners
  [tidy-tree plot]
  (reduce (fn [listeners [node parts]]
            (reduce (partial apply register-handler)
                    listeners
                    (node-listeners node parts)))
          {}
          plot))

(defn choreograph
  [tidy-tree plot fired-events]
  (reduce (fn [timeline [path interval]]
            (assoc-in timeline path interval))
          {}
          (run-schedule (make-schedule tidy-tree fired-events)
                        [:before :on :after]
                        (plot-listeners tidy-tree plot))))