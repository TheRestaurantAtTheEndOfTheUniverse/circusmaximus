(defproject circusmaximus "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [ring-server "0.5.0"]
                 [reagent "0.7.0"]
                 [reagent-utils "0.3.1"]
                 [ring "1.6.3"]
                 [ring/ring-defaults "0.3.1"]
                 [metosin/compojure-api "2.0.0-alpha5"]
                 [hiccup "1.0.5"]
                 [yogthos/config "1.1.1"]
                 [org.clojure/clojurescript "1.10.238"
                  :scope "provided"]
                 [org.clojure/core.async "0.4.474"]
                 [com.layerware/hugsql "0.4.8"]
                 [org.postgresql/postgresql "42.1.4"]
                 [com.taoensso/timbre "4.10.0"]
                 [clj-time "0.13.0"]
                 [com.andrewmcveigh/cljs-time "0.4.0"]
                 [cheshire "5.8.0"]
                 [prismatic/schema "1.1.9"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [environ "1.1.0"]
                 [cljs-ajax "0.7.3"]
                 [re-frame "0.10.5"]
                 [re-com "2.1.0"]
                 [day8.re-frame/http-fx "0.1.6"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [com.cemerick/url "0.1.1"]
                 [metrics-clojure "2.10.0"]
                 ]

  :plugins [[lein-environ "1.1.0"]
            [lein-cljsbuild "1.1.7"]
            [lein-asset-minifier "0.2.7"
             :exclusions [org.clojure/clojure]]]

  :ring {:handler      circusmaximus.handler/app
         :uberwar-name "circusmaximus.war"}

  :min-lein-version "2.5.0"
  :uberjar-name "circusmaximus.jar"
  :main circusmaximus.server
  :clean-targets ^{:protect false}
  [:target-path
   [:cljsbuild :builds :app :compiler :output-dir]
   [:cljsbuild :builds :app :compiler :output-to]]

  :source-paths ["src/clj" "src/cljc"]
  :resource-paths ["resources" "target/cljsbuild"]

  :minify-assets
  {:assets
   {"resources/public/css/site.min.css" "resources/public/css/site.css"}}

  :cljsbuild
  {:builds {:min
            {:source-paths ["src/cljs" "src/cljc" "env/prod/cljs"]
             :compiler
             {:output-to     "target/cljsbuild/public/js/app.js"
              :output-dir    "target/cljsbuild/public/js"
              :source-map    "target/cljsbuild/public/js/app.js.map"
              :optimizations :advanced
              :pretty-print  false}}
            :app
            {:source-paths ["src/cljs" "src/cljc" "env/dev/cljs"]
             :figwheel     {:on-jsload "circusmaximus.core/mount-root"}
             :compiler
             {:main          "circusmaximus.dev"
              :asset-path    "/js/out"
              :output-to     "target/cljsbuild/public/js/app.js"
              :output-dir    "target/cljsbuild/public/js/out"
              :source-map    true
              :optimizations :none
              :pretty-print  true}}
            :test
            {:source-paths ["src/cljs" "src/cljc" "test/cljs"]
             :compiler     {:main          circusmaximus.doo-runner
                            :asset-path    "/js/out"
                            :output-to     "target/test.js"
                            :output-dir    "target/cljstest/public/js/out"
                            :optimizations :whitespace
                            :pretty-print  true}}


            }
   }


  :figwheel
  {:http-server-root "public"
   :server-port      3449
   :nrepl-port       7002
   :nrepl-middleware ["cemerick.piggieback/wrap-cljs-repl"
                      "cider.nrepl/cider-middleware"
                      "refactor-nrepl.middleware/wrap-refactor"
                      ]
   :css-dirs         ["resources/public/css"]
   :ring-handler     circusmaximus.handler/app}



  :profiles {:dev {:repl-options {:init-ns          circusmaximus.repl
                                  :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]
                                  :timeout          120000 }

                   :dependencies [[binaryage/devtools "0.9.9"]
                                  [ring/ring-mock "0.3.2"]
                                  [ring/ring-devel "1.6.3"]
                                  [prone "1.5.1"]
                                  [figwheel-sidecar "0.5.15"]
                                  [org.clojure/tools.nrepl "0.2.13"]
                                  [com.cemerick/piggieback "0.2.2"]
                                  [pjstadig/humane-test-output "0.8.3"]

                                  ]

                   :source-paths ["env/dev/clj"]
                   :plugins      [[lein-figwheel "0.5.15"]
                                  [lein-doo "0.1.10"]
                                  [cider/cider-nrepl "0.15.1"]
                                  [org.clojure/tools.namespace "0.3.0-alpha4"
                                   :exclusions [org.clojure/tools.reader]]
                                  [refactor-nrepl "2.3.1"
                                   :exclusions [org.clojure/clojure]]
                                  ]

                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]

                   :env {:dev true}}

             :uberjar {:hooks        [minify-assets.plugin/hooks]
                       :source-paths ["env/prod/clj"]
                       :prep-tasks   ["compile" ["cljsbuild" "once" "min"]]
                       :env          {:production true}
                       :aot          :all
                       :omit-source  true}})
