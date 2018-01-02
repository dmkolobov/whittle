(ns tidy-tree.choreograph
  (:require [clojure.zip :as zip]
            [tidy-tree.timeline :refer [starting-at record ending-at with-recorder]]
            [tidy-tree.util :refer [zipper fast-forward]]))

(defn schedule-enter
  [loc]
  (let [id  (:id (zip/node loc))
        ids (map :id (zip/rights loc))]
    (zipmap (conj ids id) (repeat (inc (count ids)) 0))))

(defn schedule
  [tidy-tree fired]
  (with-recorder fired
    (fn [state]
      (record state
              (partial fast-forward (zipper tidy-tree))
              :on    :enter
              :write schedule-enter))))

(defn proportional
  [time & lengths]
  (let [total (reduce + lengths)]
    (map #(* time (/ % total)) lengths)))

(defn scheduled-for
  [sched event {:keys [id children]} & {:keys [child?]}]
  (get-in sched [(if child? (:id (first children)) id) event]))

(defn mul-child?
  [{:keys [children]}]
  (and (seq children) (seq (rest children))))

(defn one-child?
  [{:keys [children]}]
  (and (seq children) (not (seq (rest children)))))

(defn choreograph-entrance
  [scheduled-for node {:keys [stem branch]}]
  (let [enter (scheduled-for node)]
    (cond-> (zipmap [:root :body] (starting-at enter 0.25 0.5))
            (one-child? node) (merge (zipmap [:stem]
                                             (ending-at (scheduled-for node :child? true) 0.25)))
            (mul-child? node) (merge (zipmap [:stem :branch]
                                             (apply ending-at
                                                    (scheduled-for node :child? true)
                                                    (proportional 0.25
                                                                  (:height stem)
                                                                  (:width branch))))))))
(defn lifecycle
  [fired-events sched event choreo-fn]
  (let [fired?   (fn [{:keys [id]}] (contains? (get fired-events event) id))
        sched-fn (partial scheduled-for sched :enter)]
    (fn [timeline {:keys [id] :as node} parts]
      (if (fired? node)
        (reduce (fn [timeline [part time]] (assoc-in timeline [id part event] time))
                timeline
                (choreo-fn sched-fn node parts))
        timeline))))

(defn choreograph
  [tidy plot fired-events]
  (let [sched (schedule tidy fired-events)
        enter (lifecycle fired-events sched :enter choreograph-entrance)]
    (reduce (fn [timeline [node parts]]
              (enter timeline node parts))
            {}
            plot)))