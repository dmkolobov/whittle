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

(defn choreograph-entrance
  [schedule {:keys [id children]} {:keys [stem branch]}]
  (let [enter (get-in schedule [id :enter])]
    (merge (zipmap [:root :body]
                   (starting-at enter 0.25 0.5))
           (when (seq children)
             (let [[{:keys [id]} & other-children] children
                   child-enter   (get-in schedule [id :enter])]
               (if (seq other-children)
                 (let [slen         (:height stem)
                       blen         (:width branch)
                       tlen         (+ slen blen) ;;
                       stemdur      (* 0.25 (/ slen tlen))
                       branchdur    (* 0.25 (/ blen tlen))]
                   (zipmap [:stem :branch]
                           (ending-at child-enter stemdur branchdur)))
                 {:stem {:delay (- child-enter 0.25) :duration 0.25}}))))))

(defn choreograph
  [tidy plot fired-events]
  (let [enter-times (schedule tidy fired-events)]
    (reduce (fn [timeline [{:keys [id] :as node} parts]]
              (assoc timeline
                id (choreograph-entrance enter-times node parts)))
            {}
            plot)))