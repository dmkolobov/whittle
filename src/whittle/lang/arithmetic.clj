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
    {:grammar    expr-grammar
     :start      :expr
     :transforms {:add    +
                  :sub    -
                  :mul    *
                  :div    /
                  :number clojure.edn/read-string
                  :expr   identity}}))

(defn apply-or-identity
  [x & args]
  (if (fn? x) (apply x args) x))

(defn applies-args
  [f]
  (fn [& args]
    (fn [ctx]
      (apply f (map #(apply-or-identity % ctx) args)))))

(defn emit-var
  [name]
  (fn [ctx]
    (if-let [value (get ctx name)]
      value
      (throw (Exception. (str name " is undefined."))))))

(def v2
  "Returns a function which, when called with a map 'ctx', returns the value
   of the arithmetic expression written in infix notation, where each single-word English
   identifier is substituted with the corresponding value in 'ctx' prior to evaluation."
  (-> (whittle/create-compiler
        {:grammar    expr-grammar
         :start      :expr
         :transforms {:number clojure.edn/read-string
                      :add    (applies-args +)
                      :sub    (applies-args -)
                      :mul    (applies-args *)
                      :div    (applies-args /)
                      :expr   (applies-args identity)}})
      (whittle/update-lang
        (fn [spec]
          (-> spec
              (whittle/add-node :symbol "#'[a-zA-s]+'" keyword)
              (whittle/add-node :var (comb/nt :symbol) emit-var)
              (whittle/alt-node :term (comb/nt :var) :hide? true))))))

(defn emit-let-expr
  [& let-exprs]
  (let [let-clauses (butlast let-exprs)
        expr        (last let-exprs)]
    (fn [ctx]
      (expr (reduce (fn [ctx let-node] (let-node ctx))
                    ctx
                    let-clauses)))))

(defn emit-let
  [name binding]
  (fn [ctx] (assoc ctx name (apply-or-identity binding ctx))))

(def v3
  (whittle/update-lang v2
                       whittle/combine
                       {:start    :let-expr
                        :grammar  "let-expr = [<'let'> let (<','> let)* <':'>] expr
                                   let      = symbol <'='> let-expr"
                        :transforms  {:let-expr emit-let-expr
                                      :let      emit-let}}))

(defn emit-fn
  [& fn-v]
  (let [params (butlast fn-v)
        body   (last fn-v)]
    (fn [ctx]
      (fn [& args]
        (body (merge ctx (zipmap params args)))))))

(defn emit-apply
  [name & args]
  (fn [ctx]
    (apply (get ctx name)
           (map #(apply-or-identity % ctx) args))))

(def fn-grammar
  "fn-apply = symbol <'('> expr (<','> expr)* <')'>
   fn       = <'('> symbol (<','> symbol)* <')'> <'='> <'{'>
                let-expr
              <'}'>")

(def v4
  (whittle/update-lang v3
                       (fn [lang]
                         (-> lang
                             (whittle/combine
                               {:grammar    fn-grammar
                                :transforms {:fn        emit-fn
                                             :fn-apply  emit-apply}})
                             (whittle/alt-node :let "symbol fn")
                             (whittle/alt-node :term (comb/nt :fn-apply) :hide? true)))))

(def final
  (whittle/create-compiler
    {:start     :let-expr
     :grammars  ["let-expr = [<'let'> let (<','> let)* <':'>] expr

                  let       = symbol <'='> let-expr | symbol fn

                  var       = symbol

                  expr      = add-sub
                  <add-sub> = mul-div | add | sub
                  add       = add-sub <'+'> mul-div
                  sub       = add-sub <'-'> mul-div
                  <mul-div> = term | mul | div
                  mul       = mul-div <'*'> term
                  div       = mul-div <'/'> term
                  <term>    = number | <'('> add-sub <')'> | fn-apply | var

                  number    = #'[0-9]+'
                  symbol    = #'[a-zA-s]+'"
                 fn-grammar]

     :transforms {:number clojure.edn/read-string
                  :symbol keyword

                  :add  (applies-args +)
                  :sub  (applies-args -)
                  :mul  (applies-args *)
                  :div  (applies-args /)
                  :expr (applies-args identity)

                  :let-expr   emit-let-expr
                  :var        emit-var
                  :let        emit-let
                  :fn         emit-fn
                  :fn-apply   emit-apply}}))