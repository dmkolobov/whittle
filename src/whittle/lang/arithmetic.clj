(ns whittle.lang.arithmetic
  (:require [instaparse.combinators :as comb]
            [whittle.core :as whittle]))

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

(defn apply-or-identity
  [ctx x]
  (if (fn? x) (x ctx) x))

(defn applies-args
  [f]
  (fn [& args]
    (fn [ctx]
      (apply f (map #(apply-or-identity ctx %) args)))))

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
      (whittle/update-lang
        (fn [spec]
          (-> spec
              (whittle/add-node :symbol
                                "#'[a-zA-s]+'"
                                keyword)
              (whittle/add-node :var-lookup
                                (comb/nt :symbol)
                                (fn [name] (fn [ctx] (get ctx name))))
              (whittle/alt-node :term (comb/nt :var-lookup) :hide? true))))))

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
      (whittle/update-lang
        (fn [spec]
          (-> spec
              (whittle/add-node :symbol
                                "#'[a-zA-s]+'"
                                keyword)
              (whittle/add-node :var-lookup
                                (comb/nt :symbol)
                                (fn [name]
                                  (fn [ctx]
                                    (if-let [value (get ctx name)]
                                      value
                                      (throw (Exception. (str name " is undefined."))))))
                                :trace? true)
              (whittle/alt-node :term (comb/nt :var-lookup) :hide? true))))))

(defn emit-statement
  [& statements]
  (let [let-clauses (butlast statements)
        expr        (last statements)]
    (fn [ctx]
      (expr (reduce (fn [ctx let-node] (let-node ctx))
                    ctx
                    let-clauses)))))

(defn emit-let
  [name expr]
  (fn [ctx]
    (assoc ctx name (expr ctx))))

(def v4
  (whittle/update-lang v3
                       whittle/combine
                       {:start    :statement
                        :grammar  "statement = [<'let'> (<' '> let)+] expr
                                   let       = symbol <'='> expr <';'>"
                        :branches  {:statement emit-statement
                                    :let       emit-let}}))


(def v5
  (whittle/update-lang v4
                       (fn [lang]
                         (-> lang
                             (whittle/remove-node :let)
                             (whittle/combine
                               {:grammar "<let>       = (var-let | fn-let) <';'>
                                          var-let     = symbol <'='> expr
                                          fn-let      = symbol fn-params <'='> fn-body
                                          fn-params   = <'('> symbol (<','> symbol)* <')'>
                                          fn-args     = <'('> expr (<','> expr)* <')'>
                                          <fn-body>   = <'{'> statement <'}'>
                                          fn-apply    = symbol fn-args"
                                :branches {:fn-params list
                                           :fn-args   list
                                           :fn-let   (fn [name params body]
                                                       (fn [ctx]
                                                         (assoc ctx
                                                           name (fn [& args]
                                                                  (body (merge ctx (zipmap params args)))))))
                                           :var-let  emit-let
                                           :fn-apply (fn [name args]
                                                       (fn [ctx]
                                                         (apply (get ctx name)
                                                                (map #(apply-or-identity ctx %) args))))}})
                             (whittle/alt-node :term (comb/nt :fn-apply) :hide? true)))))

