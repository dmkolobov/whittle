(ns whittle.inspect
  (:require [instaparse.core :as insta]
            [whittle.core :refer [whittle]]))

(defn tree-index
  "Given a parse tree, return it's index in the AST."
  [tree]
  (into [(first tree)] (insta/span tree)))

(defn tree-change
  "Given a parse-tree and the result of it's transformation, return
  a tuple representing this change in the entire AST."
  [parse-tree result]
  [(tree-index parse-tree) result])

(defn contains-index?
  "Given a parse tree and a tree-index value, returns true if the  given
  'index' references a location inside parse tree."
  [parse-tree index]
  (let [[_ start end]         index
        [tree-start tree-end] (insta/span parse-tree)]

    (println "tree" parse-tree "index" [start end] "tree" [tree-start tree-end])
    (<= tree-start start end tree-end)))

(defn inspect-hook
  [changes parse-tree result]
  (swap! changes conj (tree-change parse-tree result))
  result)

(defn inspect
  [f source]
  (let [changes (atom [])
        g       (whittle f {:hooks {:after (partial inspect-hook changes)}})
        result  (g source)
        ast     (:ast (meta result))]
    {:changes @changes
     :ast     ast
     :result  result}))

(defn apply-change
  [hiccup-tree [index result :as change]]
  (if (= index (tree-index hiccup-tree))
    result
    (with-meta (into [(first hiccup-tree)]
                     (map (fn [child]
                            (if (and (vector? child)
                                     (contains-index? child index))
                              (apply-change child change)
                              child)))
                     (rest hiccup-tree))
               (meta hiccup-tree))))

(defn playback
  "Given a value returned by inspect, return the changes an AST for
  the given language goes through as it is compiled."
  [{:keys [ast changes]}]
  (loop [ast     ast
         tape    []
         changes changes]
    (if (seq changes)
      (recur (apply-change ast (first changes))
             (conj tape ast)
             (rest changes))
      tape)))