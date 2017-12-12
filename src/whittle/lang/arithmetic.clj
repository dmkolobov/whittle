(ns whittle.lang.arithmetic
  (:require [whittle.core :as whittle]))

(def expr-grammar
  "expr     = add-sub
  <add-sub> = mul-div | add | sub
  add       = add-sub <'+'> mul-div
  sub       = add-sub <'-'> mul-div
  <mul-div> = term | mul | div
  mul       = mul-div <'*'> term
  div       = mul-div <'/'> term
  <term>    = number | <'('> add-sub <')'>
  number    = #'[0-9]+'")

(def v1
  "Returns the value of an arithmetic expression written in infix notation."
  (whittle/create-compiler
    {:grammar   expr-grammar
     :start     :expr
     :terminals {:add    +
                 :sub    -
                 :mul    *
                 :div    /
                 :number clojure.edn/read-string
                 :expr   identity}}))

(defn applies-args
  [f]
  (fn [& args]
    (fn [ctx]
      (apply f (map #(if (fn? %) (% ctx) %) args)))))

(def v2
  "Returns a function which, when called with a map 'ctx', returns the value
   of the arithmetic expression written in infix notation, where each single-word English
   identifier is substituted with the corresponding value in 'ctx' prior to evaluation."
  (-> (whittle/create-compiler
        {:grammar   expr-grammar
         :start     :expr
         :terminals {:add    (applies-args +)
                     :sub    (applies-args -)
                     :mul    (applies-args *)
                     :div    (applies-args /)
                     :number clojure.edn/read-string
                     :expr   identity}})
      (whittle/extend-node :term
                           {:node      :symbol
                            :grammar   "#'[a-zA-Z](-|[a-zA-Z])*'"
                            :hide?     true
                            :transform (fn [name] (fn [ctx] (get ctx (keyword name))))})))

(def v3
  "The same as the language defined by 'v2', except errors such as division by zero and unbound
  identifiers are traced through the source."
  (-> (whittle/create-compiler
        {:grammar   expr-grammar
         :start     :expr
         :terminals {:number clojure.edn/read-string}
         :branches  {:add  (applies-args +)
                     :sub  (applies-args -)
                     :mul  (applies-args *)
                     :div  (applies-args /)
                     :expr (applies-args identity)}})
      (whittle/extend-node :term
                           {:node      :symbol
                            :grammar   "#'[a-zA-Z](-|[a-zA-Z])*'"
                            :hide?     true
                            :transform (fn [name]
                                         (fn [ctx]
                                           (if-let [x (get ctx (keyword name))]
                                             x
                                             (throw (Exception. (str name " is undefined"))))))})))

(def lemma-v1
  (whittle/create-compiler
    {:start    :statement
     :grammar  "statement = [<'let'> (<' '> let)+] expr
                let       = symbol <'='> expr <';'>
                expr      = 'e'
                symbol    = 's'
                number    = 'n'"
     :branches  {:statement (fn [& statements]
                              (let [let-clauses (butlast statements)
                                    expr        (last statements)]
                                (fn [ctx]
                                  (expr (reduce (fn [ctx let-node] (let-node ctx))
                                                ctx
                                                let-clauses)))))
                 :let       (fn [name expr]
                              (fn [ctx]
                                (assoc ctx name (expr ctx))))}}))

(def v4
  (whittle/union lemma-v1 v3))

