(ns whittle.core
  (:require [whittle.transform :refer [transform]]
            [instaparse.core :as insta]
            [instaparse.combinators :as comb]
            [instaparse.combinators-source :refer [hidden-tag?]]))

(defn ebnf [x] (if (string? x) (comb/ebnf x) x))

(defn attach-ast
  [ast result]
  (if (meta result) (with-meta result {:ast ast}) result))

(defn wrap-before
  [f before]
  (cond (and f before) (fn [& children] (->> children (apply f) (apply before)))
        f              f
        :default       before))

(defn wrap-after
  [f after]
  (cond (and f after) #(->> %2 (f %1) (after %1))
        f             f
        :default      after))

(defn wrap-hooks
  [hooks {:keys [before after] :as new-hooks}]
  (when new-hooks
    (-> (or hooks {})
        (update :before wrap-before before)
        (update :after wrap-after after))))

(defn transform-ast
  [transforms ast template {:keys [before after]}]
  (let [annotated-ast (insta/add-line-and-column-info-to-metadata template ast)]
    (attach-ast annotated-ast (transform transforms
                                         annotated-ast
                                         :before before
                                         :after  after))))

(defn compiler-fn
  [{:keys [start grammar transforms hooks] :as lang}]
  (let [parse      (insta/parser grammar
                                 :start           start
                                 :auto-whitespace :standard)]
    (with-meta (fn [template]
                 (let [ast (parse template)]
                   (if (insta/failure? ast)
                     (throw (ex-info "Parser error" {:ast ast}))
                     (transform-ast transforms ast template hooks))))
               (assoc lang :parse parse))))

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
  (cond-> lang
          start                  (assoc :start start)
          (contains? ext :hooks) (update :hooks wrap-hooks hooks)
          true                   (update :grammar update-grammar ext)
          true                   (update :transforms into transforms)))

(defn whittle
  [f & exts]
  (compiler-fn
    (if (fn? f)
      (reduce extend-lang (meta f) exts)
      (reduce extend-lang {:transforms {}} (conj exts f)))))

(defn parse
  [f source]
  ((:parse (meta f)) source))
