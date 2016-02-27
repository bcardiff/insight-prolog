(defproject insight-prolog "0.0.1"
  :decription "Get some insight about how prolog works"
  :url "http://github.com/bcardiff/insight-prolog"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]]

  :plugins [[lein-cljsbuild "1.1.2"]]

  :hooks [leiningen.cljsbuild]

  :cljsbuild {
    :test-commands {
      "unit" ["node" "site/javascripts/unit-test.js"] }

    :builds {
      :dev {
        :source-paths ["src-cljs"]
        :compiler {
          :output-to "site/javascripts/main.js"
          :optimizations :whitespace
          :pretty-print true}}

      :test {
        :source-paths ["src-cljs" "test-cljs"]
        :compiler {
            :output-to "site/javascripts/unit-test.js"
            :optimizations :simple
            :pretty-print true
            :target :nodejs}}
      }}

  :clean-targets ^{:protect false} ["site/javascripts" :target-path])
