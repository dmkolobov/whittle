(ns whittle.util)

(def line-and-column-keys
  [:instaparse.gll/start-index
   :instaparse.gll/start-line
   :instaparse.gll/start-column
   :instaparse.gll/end-index
   :instaparse.gll/end-line
   :instaparse.gll/end-column])

(defn fn-exceptions
  [parse-tree result]
  (if (fn? result)
    (fn [& args]
      (try (apply result args)
           (catch #?(:clj Exception :cljs js/Error) e
             (throw
               (ex-info (.getMessage e)
                        (-> (ex-data e)
                            (update :stack #(or % []))
                            (update :stack
                                    conj
                                    (assoc (select-keys (meta parse-tree)
                                                        line-and-column-keys)
                                      :node (first parse-tree)))))))))
    result))