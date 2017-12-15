(ns whittle.transform
  "Functions to transform parse trees. Borrowed from instaparse.transform,
  allows for transformation of nodes before and after ."
  (:require [instaparse.gll]
            [instaparse.util :refer [throw-illegal-argument-exception]]))

(defn map-preserving-meta [f l]
  (with-meta (map f l) (meta l)))

(defn do-transform
  [transform children parse-tree & {:keys [before after] :as args}]
  (cond->> children
           before (apply before)
           true   (apply transform)
           after  (after parse-tree)))

(defn merge-meta
  "This variation of the merge-meta in gll does nothing if obj is not
something that can have a metamap attached."
  [obj metamap]
  (if #?(:clj (instance? clojure.lang.IObj obj)
         :cljs (satisfies? IWithMeta obj))
    (instaparse.gll/merge-meta obj metamap)
    obj))

(defn- enlive-transform
  [transform-map parse-tree args]
  (let [transform (transform-map (:tag parse-tree))]
    (cond
      transform
      (merge-meta
        (apply do-transform
               transform
               (map #(enlive-transform transform-map % args)
                    (:content parse-tree))
               parse-tree
               args)
        (meta parse-tree))
      (:tag parse-tree)
      (assoc parse-tree :content (map #(enlive-transform transform-map % args)
                                      (:content parse-tree)))
      :else
      parse-tree)))

(defn- hiccup-transform
  [transform-map parse-tree args]
  (if (and (sequential? parse-tree) (seq parse-tree))
    (if-let [transform (transform-map (first parse-tree))]
      (merge-meta
        (apply do-transform
               transform
               (map #(hiccup-transform transform-map % args)
                    (next parse-tree))
               parse-tree
               args)
        (meta parse-tree))
      (with-meta
        (into [(first parse-tree)]
              (map #(hiccup-transform transform-map % args)
                   (next parse-tree)))
        (meta parse-tree)))
    parse-tree))

(defn transform
  "Takes a transform map and a parse tree (or seq of parse-trees).
   A transform map is a mapping from tags to
   functions that take a node's contents and return
   a replacement for the node, i.e.,
   {:node-tag (fn [child1 child2 ...] node-replacement),
    :another-node-tag (fn [child1 child2 ...] node-replacement)}"
  [transform-map parse-tree & args]
  ; Detect what kind of tree this is
  (cond
    (string? parse-tree)
    ; This is a leaf of the tree that should pass through unchanged
    parse-tree

    (and (map? parse-tree) (:tag parse-tree))
    ; This is an enlive tree-seq
    (enlive-transform transform-map parse-tree args)

    (and (vector? parse-tree) (keyword? (first parse-tree)))
    ; This is a hiccup tree-seq
    (hiccup-transform transform-map parse-tree args)

    (sequential? parse-tree)
    ; This is either a sequence of parse results, or a tree
    ; with a hidden root tag.
    (map-preserving-meta #(apply transform transform-map % args) parse-tree)

    (instance? instaparse.gll.Failure parse-tree)
    ; pass failures through unchanged
    parse-tree

    :else
    (throw-illegal-argument-exception
      "Invalid parse-tree, not recognized as either enlive or hiccup format.")))