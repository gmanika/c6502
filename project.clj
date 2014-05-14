(defproject c6502 "1.0.3"
  :description "6502 emulator in ClojureScript"
  :source-paths ["src-clj"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2173"
                  :exclusions [org.apache.ant/ant]]
                 [compojure "1.1.6"]
                 [hiccup "1.0.4"]]
  :plugins [[lein-cljsbuild "1.0.2"]
            [lein-ring "0.8.7"]]
  :cljsbuild {
    :builds [{:id "nes"
              :source-paths ["src-cljs"]
              :compiler {:output-to "nes.js"
                         :output-dir "out"
                         :optimizations :none
                         :pretty-print true
                         :source-map true}}]}
  :ring {:handler example.routes/app})
