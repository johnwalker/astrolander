(defproject astrolander "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/clojurescript "0.0-3126"]
                 [com.lucasbradstreet/cljs-uuid-utils "1.0.1"]
                 [quil "2.2.5"]
                 [cljs-http "0.1.27"]]
  :plugins [[lein-cljsbuild "1.0.5"]]
  :hooks [leiningen.cljsbuild]

  :cljsbuild
  {:builds [{:id "dev"
             :source-paths ["src"]
             :compiler
             {:output-to "js/main.js"
              :output-dir "out"
              :main "astrolander.core"
              :optimizations :none
              :pretty-print true}}
            {:id "prod"
             :source-paths ["src"]
             :compiler
             {:output-to "prod/main.js"
              :output-dir "prod"
              :main "astrolander.core"
              :optimizations :advanced
              :pretty-print false}}]})
