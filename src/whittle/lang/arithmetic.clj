(ns whittle.lang.arithmetic
  (:require [instaparse.combinators :as comb]
            [whittle.core :refer [whittle]]))

(def lang
  "Returns the value of an arithmetic expression written in infix notation."
  (whittle
    {:grammar   "expr      = add-sub
                 <add-sub> = mul-div | add | sub
                 add       = add-sub <'+'> mul-div
                 sub       = add-sub <'-'> mul-div
                 <mul-div> = term | mul | div
                 mul       = mul-div <'*'> term
                 div       = mul-div <'/'> term
                 <term>    = number | <'('> add-sub <')'>
                 number    = #'[0-9]+'"
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

(def let-extension
  {:start      :let-expr

   :grammar    "let-expr = [<'let'> let (<','> let)* <':'>] expr
                let      = symbol <'='> let-expr
                var      = symbol
                symbol   = #'[a-zA-s]+'"

   :alts       {:term "var"}

   :transforms {:add      (applies-args +)
                :sub      (applies-args -)
                :mul      (applies-args *)
                :div      (applies-args /)

                :let-expr emit-let-expr
                :let      emit-let
                :var      emit-var

                :symbol   keyword}})

(def let-lang
  "Returns a function which, when called with a map 'ctx', returns the value
   of the arithmetic expression written in infix notation, where each single-word English
   identifier is substituted with the corresponding value in 'ctx' prior to evaluation."
  (whittle lang let-extension))

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

(def fn-extension
  {:grammar    "fn-apply = symbol <'('> expr (<','> expr)* <')'>
                fn       = <'('> symbol (<','> symbol)* <')'> <'='> <'{'>
                             let-expr
                           <'}'>"

   :alts       {:term "fn-apply"
                :let  "symbol fn"}

   :transforms {:fn-apply emit-apply
                :fn       emit-fn}})

(def fn-lang
  (whittle let-lang fn-extension))
