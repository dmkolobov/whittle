(defproject whittle "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljsbuild "1.1.7"]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.8.40"]
                 [instaparse "1.4.8"]
                 [cljfmt "0.5.7"]]

  :cljsbuild {:builds [{:id "none"
                        :source-paths ["src/"]
                        :compiler {:output-to "target/js/none.js"
                                   :optimizations :none
                                   :pretty-print true}}]})
