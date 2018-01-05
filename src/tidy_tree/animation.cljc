(ns tidy-tree.animation
  (:require [clojure.zip :as zip]
            [choreograph.core :refer [request-frame buffer record]]
            [tidy-tree.util :refer [zipper rewind fast-forward zip-seq]]))


(defn animate
  [anim events]
  (merge
    {:now         0
     :timeline    {}
     :fire-events events}))

;; --------------------------------------------------------------------------

(defn siblings
  [loc]
  (map :id
       (into (conj (zip/rights loc)
                   (zip/node loc))
             (zip/lefts loc))))

(defn animate-leave
  [loc fire!]
  (rewind (fast-forward loc) (comp (partial fire! :leave) siblings)))

(defn animate-move
  [loc fire!]
  (fire! :move (map :id (zip-seq loc))))

(defn animate-enter
  [loc fire!]
  (fast-forward loc (comp (partial fire! :enter) siblings)))

;; --------------------------------------------------------------------------




(defn step-frame
  [{:keys [frame frame-buffer commands] :as animation}]
  (if (seq commands)
    (let [[command arg] (first commands)]
      (assoc animation
        :frame        (if (= command :flush) frame-buffer frame)
        :frame-buffer (condp = command
                        :request-frame (request-frame frame-buffer arg)
                        :buffer        (buffer frame-buffer arg)
                        :record        (record frame-buffer arg)
                        :flush         frame-buffer)
        :commands     (rest commands)))))