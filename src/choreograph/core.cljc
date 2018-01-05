(ns choreograph.core
  (:require [clojure.set :as set]
            [choreograph.util :refer [assoc-set merge-set]]))

(defn starting
  [start & durations]
  (loop [now      start
         durs     durations
         timeline []]
    (if (seq durs)
      (recur (+ now (first durs))
             (rest durs)
             (conj timeline [now (first durs)]))
      timeline)))

;; ---- listeners & lifecycles -----------------------------------------------------------------

(defn add-listener
  [listeners [[stage & event] transition]]
  (update-in listeners (reverse event) assoc-set stage transition))

(defn trigger
  [listeners event]
  (get-in listeners (reverse event)))

(defn trigger-lifecycles
  "Returns a map whose keys are those in 'stages', and values are seqs of transitions
  occuring during each respective stage of each fired event."
  [listeners events]
  (reduce merge-set (map (partial trigger listeners) events)))

(defn trigger-stages
  "Returns a sequence of transition sequences, grouped by the stage during which they occur."
  [listeners stages events]
  (filter seq
          (map (partial get (trigger-lifecycles listeners events))
               stages)))

;; ---- transitions ---------------------------------------------------------------------

(defn duration
  [transition]
  (cond (vector? transition)
        (reduce + (map last transition))

        (set? transition)
        (apply max (map last transition))))

(defn schedule
  [now timeline transition]
  (into timeline
        (cond (vector? transition)
              (zipmap (map pop transition)
                      (apply starting now (map last transition)))

              (set? transition)
              (map (juxt pop (comp (partial vector now) last))
                   transition))))

;; ---- event filtering -----------------------------------------------------------------

(defn firing-events
  [fired-events event targets]
  (seq
    (map (partial vector event)
         (set/intersection (set targets)
                           (get fired-events event)))))

(defn ignore
  [fired events]
  (reduce (fn [fired [event-name id]] (update fired event-name disj id))
          fired
          events))

;; ---- frames: our state machine --------------------------------------------------------

(defrecord AnimationFrame
  [drawing now timeline fired-events listeners stages])

(def blank-frame
  (AnimationFrame. nil 0 {} {} {} []))

(defn fire
  [{:keys [now timeline fired-events listeners stages] :as frame} event targets]

  ;; though the animation handler may invoke 'fire!' with a certian event, it
  ;; should only trigger the transitions associated with it under two conditions:
  ;;
  ;; 1) the event has previously been registered with 'buffer'

  ;; 2) the event has not been fired yet
  ;;
  ;; So, we can only fire each event on a particular target once, and we must explicitely
  ;; state our intent to fire said event via 'buffer'.

  (if-let [events (firing-events fired-events event targets)]

    (loop [now       now
           timeline  timeline
           stage-seq (trigger-stages listeners stages events)]

      (if (seq stage-seq)

        (let [transitions (first stage-seq)]

          (recur (+ now (apply max (map duration transitions)))
                 ;; the time is advanced by the maximum transition duration.

                 (reduce (partial schedule now) timeline transitions)
                 ;; every transition triggered during this lifecycle is scheduled to run
                 ;; starting at 'now'.

                 (rest stage-seq)
                 ;; we drop the transitions we just processed and continue
                 ))

        (assoc frame
          :now          now
          :timeline     timeline
          :fired-events (ignore fired-events events)
                        ;; we remove the target ids for the events we just processed from the
                        ;; 'fired-events' map, because we "consume" events as we process them.
                        )))
    frame))

;; ---- animation frames ----------------------------------------------------------


(defrecord Animation
  [frame frame-buffer commands stages listen-fn plot-fn])

(defn animation
  [{:keys [stages listen-fn plot-fn]}]
  (Animation. nil blank-frame [] stages listen-fn plot-fn))

(defn buffer
  "Reset the current time and set the events which will be recorded this frame."
  [anim fired-events]
  (update anim
          :frame-buffer
          assoc
          :now 0 :timeline {} :fired-events fired-events))

(defn record
  "f should be a function of 2 arguments. Applies f to the frame buffer drawing and a
  stateful function fire!, which takes 2 arguments.

  Each time fire! is applied with an event name and a sequence of target ids, any transitions
  arising from these events are statefully scheduled to occur starting at the current time.

  Transitions arising from a particular lifecycle stage are scheduled together, irrespective
  of the event type. Transitions are thus grouped by lifecycle stage, in the order defined by
  the :stages option of the animation.

  f is expected to apply fire! zero or more times. The value of f is discarded. Returns
  the animation with an updated frame buffer.

  Returns animation unchanged if drawing has not previously been associated with the animation
  via request-frame."
  [anim f]
  (if-let [drawing (get-in anim [:frame-buffer :drawing])]
    (let [state (atom (:frame-buffer anim))
          fire! (partial swap! state fire)]
      (f drawing fire!)
      (assoc anim :frame-buffer @state))
    anim))

(defn attach-listeners
  [anim drawing]
  (reduce add-listener {} (mapcat (:listen-fn anim) ((:plot-fn anim) drawing))))

(defn request-frame
  "Returns the animation with a frame-buffer containing the given drawing. Creates
  listeners for every item in the drawing plot."
  [anim drawing]
  (update anim
          :frame-buffer
          assoc
          :drawing   drawing
          :listeners (attach-listeners anim drawing)))

(defn flush-frame
  "Returns the same animation with the current frame set to the current value of
  the frame-buffer.

  Returns animation unchanged if drawing has not previously been associated with the animation
  via request-frame."
  [{:keys [frame-buffer] :as anim}]
  (if (get-in anim [:frame-buffer :drawing])
    (assoc anim :frame frame-buffer)
    anim))

;; ---- animation state machine --------------------------------------------------

(def not-flush (complement #{[:flush]}))

(def cmd-map {:buffer buffer :record record :request-frame request-frame})

(defn run-commands
  [anim commands]
  (reduce (fn [anim [cmd & args]]
            (apply (get cmd-map cmd) anim args))
          anim
          commands))

(comment
  "first  [:buffer events]
          [:request-frame drawing]
          [:record animate-move]
          [:record animate-enter]

          [:flush]

   rest   [:buffer events]
          [:record animate-leave]

          [:flush]

          [:require-frame drawing]
          [:record animate-move]
          [:record animate-enter]

          [:flush]")

(defn step-animation
  [anim]
  (if-let [commands (seq (:commands anim))]
    (let [[frame-commands & commands] (split-with not-flush commands)]
      (-> anim
          (run-commands frame-commands)
          (assoc :commands (rest commands))
          (flush-frame)))
    anim))

(defn buffer-animation
  [anim commands]
  (update anim :commands into commands))