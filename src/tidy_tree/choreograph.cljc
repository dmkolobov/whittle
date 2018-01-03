(ns tidy-tree.choreograph
  (:require [clojure.zip :as zip]
            [tidy-tree.timeline :refer [starting-at record ending-at skip with-recorder]]
            [tidy-tree.util :refer [zipper fast-forward rewind zip-seq]]))

(defn schedule-enter
  [loc]
  (let [id  (:id (zip/node loc))
        ids (map :id (zip/rights loc))]
    (zipmap (conj ids id) (repeat (inc (count ids)) 0))))

(defn schedule-leave
  [loc]
  (println "Scheduling leave")
  (let [id  (:id (zip/node loc))
        ids (map :id (zip/rights loc))]
    (zipmap (conj ids id) (repeat (inc (count ids)) 0))))

(defn schedule-move
  [loc]
  (let [{:keys [id level]} (zip/node loc)]
    {id 0}))

(defn schedule
  [tidy-tree fired]
  (with-recorder fired
    (fn [state]
      (record state
              (partial rewind (fast-forward (zipper tidy-tree) identity))
              :on    :leave
              :write schedule-leave)
      (record state
              (partial fast-forward (zipper tidy-tree))
              :on        :move
              :write     schedule-move
              :in-place? true)
      (skip state 1)
      ;(skip state (max (count (:lcontour tidy-tree))
      ;                 (count (:rcontour tidy-tree))))
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

(defn choreograph-leave
  [scheduled-for node {:keys [stem branch]}]
  (let [leave (scheduled-for node)]
    (cond-> (zipmap [:body :root] (starting-at leave 0.5 0.25))
            (one-child? node) (merge (zipmap [:stem]
                                             (starting-at (+ (scheduled-for node :child? true) 0.75) 0.25)))
            (mul-child? node) (merge (zipmap [:branch :stem]
                                             (apply starting-at
                                                    (+ (scheduled-for node :child? true) 0.75)
                                                    (proportional 0.25
                                                                  (:width branch)
                                                                  (:height stem))))))))

(defn choreograph-move
  [scheduled-for node _]
  (let [move (scheduled-for node)]
    (cond-> {:root [move 1]
             :body [move 1]
             :stem [move 1]}
            (mul-child? node) (merge (let [child-move (scheduled-for node :child? true)]
                                       {:branch [child-move 1]})))))

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
  (let [sched     (schedule tidy fired-events)
        lifecycle (partial lifecycle fired-events sched)
        leave     (lifecycle :leave choreograph-leave)
        move      (lifecycle :move choreograph-move)
        enter     (lifecycle :enter choreograph-entrance)]
    (reduce (fn [timeline [node parts]]
              (-> timeline
                  (leave node parts)
                  (move node parts)
                  (enter node parts)))
            {}
            plot)))