(ns whittle.core
  (:require [whittle.transform :refer [transform]]
            [instaparse.core :as insta]
            [instaparse.combinators :as comb]
            [instaparse.combinators-source :refer [hidden-tag?]]))

(defn ebnf [x] (if (string? x) (comb/ebnf x) x))

(defn attach-ast
  [ast result]
  (if (meta result) (with-meta result {:ast ast}) result))

(defn wrap-transforms
  [hooks transforms]
  (if hooks
    (reduce (fn [tmap [node transform]]
              (assoc tmap node (with-meta transform hooks)))
            {}
            transforms)
    transforms))

(defn transform-ast
  [transforms ast template]
  (let [annotated-ast (insta/add-line-and-column-info-to-metadata template ast)]
    (attach-ast annotated-ast (transform transforms annotated-ast))))

(defn compiler-fn
  [{:keys [start grammar transforms hooks] :as lang}]
  (let [parse      (insta/parser grammar
                                 :start           start
                                 :auto-whitespace :standard)
        transforms (wrap-transforms hooks transforms)]
    (with-meta (fn [template]
                 (let [ast (parse template)]
                   (if (insta/failure? ast)
                     (throw (ex-info "Parser error" {:ast ast}))
                     (transform-ast transforms ast template))))
               lang)))

(defn keep-hidden
  [grammar node f & args]
  (update grammar
          node
          (fn [rule]
            (if (hidden-tag? rule)
              (comb/hide-tag (apply f rule args))
              (apply f rule args)))))

(defn update-grammar
  [lang-grammar {:keys [grammar alts] :as ext}]
  (reduce (fn [grammar [node alt-grammar]]
            (keep-hidden grammar node comb/alt (ebnf alt-grammar)))
          (merge (ebnf lang-grammar)
                 (ebnf grammar))
          alts))

(defn extend-lang
  [lang {:keys [start transforms hooks] :as ext}]
  (-> lang
      (assoc :hooks hooks)
      (update :start #(or start %))
      (update :grammar update-grammar ext)
      (update :transforms into transforms)))

(defn whittle
  [f & exts]
  (compiler-fn
    (if (fn? f)
      (reduce extend-lang (meta f) exts)
      (reduce extend-lang {:transforms {}} (conj exts f)))))
