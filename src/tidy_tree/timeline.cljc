(ns tidy-tree.timeline
  (:require [clojure.set :as set]))

(defn assoc-set
  [m k v]
  (assoc (or m {})
    k (conj (get m k #{}) v)))

(defn merge-set
  [m1 m2]
  (reduce (fn [m k]
            (assoc m
              k (set/union (get m1 k)
                           (get m2 k))))
          (merge m1 m2)
          (set/intersection (set (keys m1))
                            (set (keys m2)))))

(defn register-handler
  [listeners [lifecycle-event & event] transition-seq]
  (update listeners
          (vec event)
          (fn [m]
            (assoc-set (or m {}) lifecycle-event transition-seq))))

(defn fire
  [listeners event]
  (reduce (fn [lifecycle [method-id transition-seqs]]
            (assoc lifecycle method-id transition-seqs))
          {}
          (get listeners event)))

(defn triggered
  [listeners events]
  (reduce (fn [lifecycle event]
            (merge-set lifecycle (fire listeners event)))
          {}
          events))

(defn cycle-length
  [cycle]
  (apply max
         (map (fn [transit-seq]
                (reduce + (map last transit-seq)))
              cycle)))

(defn start-at
  [start & durations]
  (loop [now      start
         durs     durations
         timeline []]
    (if (seq durs)
      (recur (+ now (first durs))
             (rest durs)
             (conj timeline [now (first durs)]))
      timeline)))

(defn add-transitions
  [now timeline transit-seq]
  (into timeline
         (zipmap (map pop transit-seq)
                 (apply start-at now (map last transit-seq)))))

(defn run-lifecycle
  [now timeline cycles]
  (if (seq cycles)
    (let [cycle (first cycles)]
      (recur (+ now (cycle-length cycle))
             (reduce (partial add-transitions now) timeline cycle)
             (rest cycles)))
    [now timeline]))

(defn run-schedule
  [schedule order listeners]
  (println schedule)
  (loop [now       0
         sched     schedule
         timeline  {}]
    (if (seq sched)
      (let [lifecycle      (triggered listeners (first sched))
            [now timeline] (run-lifecycle now
                                          timeline
                                          (filter seq (map (partial get lifecycle) order)))]
        (recur now (rest sched) timeline))
      (do (println (keys timeline)) timeline))))

(defn record!
  [state-atom event ids]
  (swap! state-atom
         (fn [{:keys [fired-events] :as state}]
           (let [ids           (set ids)
                 target-ids    (set/intersection (get fired-events event #{}) ids)
                 target-events (set (map (partial vector event) target-ids))]
             (if (seq target-ids)
               (-> state
                   (update :fired-events update event set/difference target-ids)
                   (update :sched conj target-events))
               state)))))

(defn record
  [fired-events f]
  (let [state-atom (atom {:fired-events fired-events :sched []})
        fire!      (partial record! state-atom)]
    (f fire!)
    (:sched @state-atom)))