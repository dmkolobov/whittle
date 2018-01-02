(ns tidy-tree.timeline)

(defn starting-at
  [start & durations]
  (loop [now      start
         durs     durations
         timeline []]
    (if (seq durs)
      (recur (+ now (first durs))
             (rest durs)
             (conj timeline {:delay now :duration (first durs)}))
      timeline)))

(defn ending-at
  [end & durations]
  (loop [now      end
         durs     (reverse durations)
         timeline (list)]
    (if (seq durs)
      (recur (- now (first durs))
             (rest durs)
             (conj timeline {:delay (- now (first durs)) :duration (first durs)}))
      timeline)))