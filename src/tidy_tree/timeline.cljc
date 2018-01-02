(ns tidy-tree.timeline
  (:require [clojure.zip :as zip]
            [tidy-tree.util :refer [zipper]]))

(defn add-enter-times
  "Given a location in the tree, the current enter, and a map of enter-times, returns
  a new map of enter-times with an entry for each sibling of the location node, including
  the location node itself."
  [loc enter-times current-enter]
  (if loc
    (recur (zip/right loc)
           (assoc enter-times (:id (zip/node loc)) current-enter)
           current-enter)
    enter-times))

(defn already-assigned?
  [timeline loc]
  (contains? timeline (:id (zip/node loc))))

(defn find-enter-times
  "Given a tidy tree, return a map where keys are node ids and values
  are node enter-times."
   [start tidy entering]
  (println entering)
   (loop [loc           (zipper tidy)
          current-enter start
          enter-times        {}]
     (if (zip/end? loc)
       enter-times
       (if (already-assigned? enter-times loc)
         (recur (zip/next loc)
                current-enter
                enter-times)
         (if (contains? entering (:id (zip/node loc)))
           (recur (zip/next loc)
                  (inc current-enter)
                  (add-enter-times loc enter-times current-enter))
           (recur (zip/next loc)
                  current-enter
                  enter-times))))))

(defn ff-sequence
  [start durations]
  (loop [now      start
         durs     durations
         timeline []]
    (if (seq durs)
      (recur (+ now (first durs))
             (rest durs)
             (conj timeline {:delay now :duration (first durs)}))
      timeline)))

(defn rw-sequence
  [end durations]
  (loop [now      end
         durs     (reverse durations)
         timeline (list)]
    (if (seq durs)
      (recur (- now (first durs))
             (rest durs)
             (conj timeline {:delay (- now (first durs)) :duration (first durs)}))
      timeline)))

(defn choreograph-entrance
  [enter-times {:keys [id children]} {:keys [stem branch]}]
  (let [enter (get enter-times id)]
    (merge (zipmap [:root :body]
                   (ff-sequence enter [0.25 0.5]))
           (when (seq children)
             (let [[{:keys [id]} & other-children] children
                   child-enter   (get enter-times id)]
               (if (seq other-children)
                 (let [slen         (:height stem)
                       blen         (:width branch)
                       tlen         (+ slen blen) ;;
                       stemdur      (* 0.25 (/ slen tlen))
                       branchdur    (* 0.25 (/ blen tlen))]
                   (zipmap [:stem :branch]
                           (rw-sequence child-enter [stemdur branchdur])))
                 {:stem {:delay (- child-enter 0.25) :duration 0.25}}))))))

(defn choreograph
  [tidy plot {:keys [entering]}]
  (let [enter-times (find-enter-times 0 tidy entering)]
    (reduce (fn [timeline [{:keys [id] :as node} parts]]
              (assoc timeline
                id (choreograph-entrance enter-times node parts)))
            {}
            plot)))