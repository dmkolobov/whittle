(ns tidy-tree.rect)

(defn center-x
  [{:keys [x width]}]
  (+ (/ width 2) x))

(defn center
  "Centers the given rects about x"
  [x {:keys [width] :as rect}]
  (assoc rect :x (- x (/ width 2))))

(defn find-baseline
  "Returns the minimum y bound of the given rects."
  [rects]
  (apply max (map #(+ (:y %) (:height %)) rects)))

(defn space
  [baseline rects]
  (loop [rects'   []
         rects    rects
         baseline baseline]
    (if (seq rects)
      (let [[r & rs] rects]
        (recur (conj rects' (assoc r :y baseline))
               rs
               (+ baseline (:height r))))
      rects')))