(ns whittle.lang.lisp
  (:require [whittle.core :refer [whittle]]))

;; quote, atom, eq, cons, car, cdr, cond

(defn apply-args
  [f]
  (fn [& params]
    (fn [& args]
      (apply f (map #(apply % args) params)))))

(def lisp
  (whittle
    {:start      :s-exp

     :grammar    "s-exp      = ( <'('> (quote | atom | eq | cons | car | cdr | cond) <')'> ) / (sym-lookup | apply)

                  apply      = <'('> sym-lookup s-exp* <')'>
                  sym-lookup = sym

                  sym        = #'[a-zA-s]+'
                  list       = <'('>  (sym | list)*  <')'>

                  quote      = <'quote'> (sym | list)
                  atom       = <'atom'> s-exp
                  eq         = <'eq'> s-exp s-exp
                  cons       = <'cons'> s-exp s-exp
                  car        = <'car'> s-exp
                  cdr        = <'cdr'> s-exp
                  cond       = <'cond'> (s-exp s-exp)*"

     :transforms {:s-exp      identity
                  :apply      (apply-args (fn [f & args] (apply f args)))
                  :sym-lookup (fn [sym] (fn [ctx] (get ctx sym)))

                  :list  list
                  :sym   symbol
                  :quote (fn [x] (constantly x))

                  :atom  (apply-args symbol?)
                  :eq    (apply-args =)
                  :cons  (apply-args #(conj %2 %1))
                  :car   (apply-args first)
                  :cdr   (apply-args rest)

                  :cond  (fn [& clauses]
                           (fn [ctx]
                             (when-let [true-clause (first
                                                      (filter (fn [[pred _]] (pred ctx))
                                                              (partition 2 clauses)))]
                               ((second true-clause) ctx))))}}))