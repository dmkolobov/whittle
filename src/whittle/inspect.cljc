(ns whittle.inspect
  (:require [clojure.zip :as zip]
            [instaparse.core :as insta]
            [whittle.transform :refer [merge-meta]]
            [whittle.core :refer [whittle parse]]))

(defrecord PlaybackNode [index result])

(defn playback-node? [x] (instance? PlaybackNode x))

(defn hiccup-tree?
  "Returns true if parse-tree is a vector with a keyword first element."
  [parse-tree]
  (and (vector? parse-tree) (keyword? (first parse-tree))))

(defn tree-index
  "Given a parse tree, return it's index in the AST."
  [tree]
  (cond (playback-node? tree) (:index tree)
        (hiccup-tree? tree)   (into [(first tree)] (insta/span tree))
        :default              nil))

(defn contains-index?
  "Given a parse tree and a tree-index value, returns true if the  given
  'index' references a location inside parse tree."
  [parse-tree index]
  (let [[_ start end]         index
        [tree-start tree-end] (insta/span parse-tree)]
    (<= tree-start start end tree-end)))

(defn inspect-hook
  [changes parse-tree result]
  (swap! changes conj [(tree-index parse-tree) result])
  result)

(defn inspect
  [f source]
  (let [changes (atom [])
        g       (whittle f {:hooks {:after (partial inspect-hook changes)}})
        result  (g source)
        ast     (if-let [ast (:ast (meta result))]
                  ast
                  (parse f source))]
    {:source  source
     :changes @changes
     :ast     ast
     :result  result}))

(defn apply-change
  [hiccup-tree [index result :as change]]
  (if (= index (tree-index hiccup-tree))
    result
    (with-meta (into [(first hiccup-tree)]
                     (map (fn [child]
                            (if (and (hiccup-tree? child)
                                     (contains-index? child index))
                              (apply-change child change)
                              child)))
                     (rest hiccup-tree))
               (meta hiccup-tree))))

(defn playback
  "Given a value returned by inspect, return the changes a parse-tree
  goes through as it is transformed."
  [{:keys [ast changes]}]
  (loop [ast     ast
         tape    []
         changes (map (fn [[index result]]
                        [index (PlaybackNode. index result)])
                      changes)]
    (if (seq changes)
      (recur (apply-change ast (first changes))
             (conj tape ast)
             (rest changes))
      (conj tape ast))))

(defn tree->nodes
  [parse-tree]
  (let [index (tree-index parse-tree)]
    (conj (->> (rest parse-tree)
               (map-indexed (fn [idx child]
                              (when (not (hiccup-tree? child))
                                (if (playback-node? child)
                                  [(tree-index child) child]
                                  [(conj index idx) child]))))
               (filter some?))
          [index parse-tree])))

(defn index-tree
  [parse-tree]
  (->> parse-tree
       (tree-seq hiccup-tree? rest)
       (filter hiccup-tree?)
       (mapcat tree->nodes)
       (into {})))
