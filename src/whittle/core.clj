(ns whittle.core
  (:require [instaparse.core :as insta]
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

(defn traced-fn
  [trace-fn f]
  (fn [& args]
    (try (apply f args)
         (catch Exception e
           (throw
             (ex-info (.getMessage e)
                      (-> (ex-data e)
                          (update :stack #(or % []))
                          (update :stack conj (trace-fn f)))))))))

(defn item-trace
  [node]
  [(:context (meta node)) (select-keys (meta node)
                                       [:instaparse.gll/start-index
                                        :instaparse.gll/start-line
                                        :instaparse.gll/start-column
                                        :instaparse.gll/end-index
                                        :instaparse.gll/end-line
                                        :instaparse.gll/end-column])])

(defn traced-transform
  [node transform]
  (fn [& node-v]
    (let [value (apply transform
                       (map (fn [value]
                              (if (fn? value) (traced-fn item-trace value) value))
                            node-v))]
      (if (fn? value)
        (with-meta value {:context node})
        value))))

(defn root-frame
  [ast result stack-root]
  (if (meta result)
    (with-meta (if (and stack-root (fn? result))
                 (traced-fn (constantly stack-root) result)
                 result)
               {:ast ast})
    result))

(defn compiler-fn
  [{:keys [start grammar transforms] :as lang}]
  (let [parse (insta/parser grammar
                            :start           start
                            :auto-whitespace :standard)]
    (with-meta (fn [template & {:keys [stack-root]}]
                 (let [ast (parse template)]
                   (if (insta/failure? ast)
                     (fail :parser-error :error ast)
                     (let [ast (insta/add-line-and-column-info-to-metadata template ast)]
                       (root-frame ast (insta/transform transforms ast) stack-root)))))
               lang)))

(def trace-xf
  (map (fn [[node transform]] [node (traced-transform node transform)])))

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
  [lang {:keys [start transforms] :as ext}]
  (-> lang
      (update :start #(or start %))
      (update :grammar update-grammar ext)
      (update :transforms into trace-xf transforms)))

(defn whittle
  [f & exts]
  (compiler-fn
    (if (fn? f)
      (reduce extend-lang (meta f) exts)
      (reduce extend-lang {:transforms {}} (conj exts f)))))
