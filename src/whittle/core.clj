(ns whittle.core
  (:require [instaparse.core :as insta]
            [instaparse.combinators :as comb]))

(defrecord Failure [render-fns deps errors])

(defn failure? [x] (instance? Failure x))

(defn fail
  [id & data]
  (map->Failure
    {:deps       #{}
     :render-fns {}
     :errors    [(merge {:id id :stack []} (apply hash-map data))]}))

(defn trace-call
  [f value]
  (let [trace (f value)]
    (fn [& args]
      (try (apply value args)
           (catch Exception e
             (throw
               (ex-info (.getMessage e)
                        (-> (ex-data e)
                            (update :stack #(or % []))
                            (update :stack conj trace)))))))))

(defn item-trace
  [node]
  [(:context (meta node)) (insta/span node)])

(defn call-frame
  [node transform]
  (vector node
          (fn [& node-v]
            (with-meta (apply transform
                              (map (fn [value]
                                     (if (fn? value)
                                       (trace-call item-trace value)
                                       value))
                                   node-v))
                       {:context node}))))

(defn compiler-fn
  [{:keys [start grammar transforms] :as compiler-spec}]
  (let [parse (insta/parser grammar :start start)]
    (with-meta (fn [template & {:keys [stack-root]}]
                 (let [ast (parse template)]
                   (if (insta/failure? ast)
                     (fail :parser-error :error ast)
                     (let [result (insta/transform transforms ast)]
                       (if (meta result)
                         (with-meta (if (and stack-root (fn? result))
                                      (trace-call (constantly stack-root)
                                                  result)
                                      result)
                                    {:ast ast})
                         result)))))
               compiler-spec)))

(defn create-compiler
  [{:keys [start grammar terminals branches]
    :or   {terminals {}}}]
  (let [grammar (if (string? grammar) (comb/ebnf grammar) grammar)]
    (compiler-fn
      {:start      start
       :grammar    grammar
       :transforms (into terminals (map #(apply call-frame %)) branches)})))

(defn union
  [c1 c2]
  (let [c1-spec (meta c1)
        c2-spec (meta c2)]
    (compiler-fn
      {:start      (:start c1-spec)
       :grammar    (merge (:grammar c1-spec) (:grammar c2-spec))
       :transforms (merge (:transforms c1-spec) (:transforms c2-spec))})))

(defn extend-node
  [compiler parent-node {:keys [node transform grammar hide?]}]
  (let [node-grammar (if (string? grammar) (comb/ebnf grammar) grammar)]
    (compiler-fn
      (-> (meta compiler)
          (update :grammar
                  (fn [g]
                    (-> g
                        (update parent-node
                                (fn [parent-grammar]
                                  (let [parent-grammar' (comb/alt parent-grammar (comb/ebnf (name node)))]
                                    (if hide? (comb/hide-tag parent-grammar') parent-grammar'))))
                        (assoc node node-grammar))))
          (update :transforms
                  conj
                  (call-frame node transform))))))