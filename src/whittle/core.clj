(ns whittle.core
  (:require [whittle.transform :refer [transform]]
            [instaparse.core :as insta]
            [instaparse.combinators :as comb]
            [instaparse.combinators-source :refer [hidden-tag?]]))

(defn ebnf [x] (if (string? x) (comb/ebnf x) x))

(defrecord Failure [render-fns deps errors])

(defn failure? [x] (instance? Failure x))

(defn fail
  [id & data]
  (map->Failure
    {:deps       #{}
     :render-fns {}
     :errors    [(merge {:id id :stack []} (apply hash-map data))]}))

(defn attach-ast
  [ast result]
  (if (meta result)
    (with-meta result {:ast ast}) result))

(defn wrap-transforms
  [hooks transforms]
  (if hooks
    (reduce (fn [tmap [node transform]]
              (assoc tmap node (with-meta transform hooks)))
            {}
            transforms)
    transforms))

(defn compiler-fn
  [{:keys [start grammar transforms hooks] :as lang}]
  (let [parse      (insta/parser grammar
                                 :start           start
                                 :auto-whitespace :standard)
        transforms (wrap-transforms hooks transforms)]
    (with-meta (fn [template]
                 (let [ast (parse template)]
                   (if (insta/failure? ast)
                     (fail :parser-error :error ast)
                     (let [ast (insta/add-line-and-column-info-to-metadata template ast)]
                       (attach-ast ast (transform transforms ast))))))
               lang)))

(defn update-grammar
  [lang-grammar {:keys [grammar alts] :as ext}]
  (reduce (fn [grammar [node alt-grammar]]
            (let [node-grammar  (get grammar node)
                  node-grammar' (comb/alt node-grammar (ebnf alt-grammar))]
              (assoc grammar
                node (if (hidden-tag? node-grammar)
                        (comb/hide-tag node-grammar')
                        node-grammar'))))
          (merge (ebnf lang-grammar)
                 (ebnf grammar))
          alts))

(defn extend-lang
  [lang {:keys [start transforms hooks] :as ext}]
  (-> lang
      (assoc :hooks hooks)
      (update :start #(or start %))
      (update :grammar update-grammar ext)
      (update :transforms
              into
              transforms)))

(defn whittle
  [f & exts]
  (compiler-fn
    (if (fn? f)
      (reduce extend-lang (meta f) exts)
      (reduce extend-lang {:transforms {}} (conj exts f)))))
