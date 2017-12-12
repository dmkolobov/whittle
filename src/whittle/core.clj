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
  (let [parse (insta/parser grammar
                            :start           start
                            :auto-whitespace :standard)]
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

(defn update-lang
  [compiler f & args]
  (compiler-fn (apply f (meta compiler) args)))

(defn combine
  [spec {:keys [start grammar terminals branches]}]
  (-> spec
      (update :start #(or start %))
      (update :grammar merge (ebnf grammar))
      (update :transforms
              (fn [transforms]
                (into (merge transforms terminals)
                      (map #(apply call-frame %))
                      branches)))))

(defn add-node
  [spec node grammar transform & {:keys [trace?]}]
  (-> spec
      (update :grammar
              assoc
              node
              (if (string? grammar) (comb/ebnf grammar) grammar))
      (update :transforms
              conj
              (if trace? (call-frame node transform) [node transform]))))

(defn remove-node
  [spec node]
  (-> spec
      (update :grammar dissoc node)
      (update :transforms dissoc node)))

(defn alt-node
  [spec node alt-grammar & {:keys [hide?]}]
  (update-in spec
             [:grammar node]
             (fn [grammar]
               (let [grammar' (comb/alt grammar alt-grammar)]
                 (if hide? (comb/hide-tag grammar') grammar')))))