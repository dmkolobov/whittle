(ns whittle.core
  (:require [instaparse.core :as insta]
            [instaparse.combinators :as comb]))

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

(defn create-compiler
  [{:keys [start grammar grammars transforms]}]
  (let [grammar (ebnf grammar)]
    (compiler-fn
      {:start      start
       :grammar    (if (seq grammars) (apply merge (map ebnf grammars)) grammar)
       :transforms (into {} trace-xf transforms)})))

(defn update-lang
  [compiler f & args]
  (compiler-fn (apply f (meta compiler) args)))

(defn combine
  [lang {:keys [start grammar transforms]}]
  (-> lang
      (update :start #(or start %))
      (update :grammar merge (ebnf grammar))
      (update :transforms into trace-xf transforms)))

(defn add-node
  [lang node grammar transform]
  (-> lang
      (update :grammar assoc node (ebnf grammar))
      (update :transforms assoc node (traced-transform node transform))))

(defn remove-node
  [lang node]
  (-> lang
      (update :grammar dissoc node)
      (update :transforms dissoc node)))

(defn alt-node
  [lang node alt-grammar & {:keys [hide?]}]
  (update-in lang
             [:grammar node]
             (fn [grammar]
               (let [grammar' (comb/alt grammar alt-grammar)]
                 (if hide? (comb/hide-tag grammar') grammar')))))