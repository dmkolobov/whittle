(ns tidy-tree.timeline
  (:require [clojure.zip :as zip]
            [tidy-tree.util :refer [fast-forward rewind]]))

(defn write-schedule
  [schedule event now additions]
  (reduce (fn [schedule [id offset]]
            (assoc-in schedule [id event] (+ now offset)))
          schedule
          additions))

(defn with-recorder
  [fired f]
  (let [state (atom {:now 0 :schedule {} :fired fired})]
    (f state)
    (:schedule @state)))

(defn should-schedule?
  [{:keys [schedule fired]} event loc]
  (let [id (:id (zip/node loc))]
    (and
      ;; event has been fired for this location
      (contains? (get fired event) id)
      ;; event is yet to be scheduled for this location
      (not (contains? (get schedule id) event)))))

(defn schedule-event
  [{:keys [now] :as recorder} event additions]
  (-> recorder
      (update :now + (inc (apply max (vals additions))))
      (update :schedule write-schedule event now additions)))

(defn record
  [state walk & {:keys [on write]}]
  (walk (fn [loc]
          (swap! state
                 (fn [recorder]
                   (if (should-schedule? recorder on loc)
                     (schedule-event recorder on (write loc))
                     recorder)))
          loc)))

(defn starting-at
  [start & durations]
  (loop [now      start
         durs     durations
         timeline []]
    (if (seq durs)
      (recur (+ now (first durs))
             (rest durs)
             (conj timeline [now (first durs)]))
      timeline)))

(defn ending-at
  [end & durations]
  (loop [now      end
         durs     (reverse durations)
         timeline (list)]
    (if (seq durs)
      (recur (- now (first durs))
             (rest durs)
             (conj timeline [(- now (first durs)) (first durs)]))
      timeline)))