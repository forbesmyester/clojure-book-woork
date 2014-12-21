(defproject fireplace "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT" :url "http://opensource.org/licenses/MIT"}
  :dependencies [
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2356"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [org.clojure/core.typed "0.2.72"]
                 ]
  :plugins [ [lein-cljsbuild "1.0.3"] [lein-npm "0.4.0"] ]
  :cljsbuild {
              :builds [{
                        :source-paths ["src-cljs"]
                        :compiler {
                                   :output-to "war/javascripts/main.js"
                                   :optimizations :simple
                                   :target :nodejs
                                   :pretty-print true
                                   }
                        }
                       ]
              }
  :main ^:skip-aot fireplace.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
