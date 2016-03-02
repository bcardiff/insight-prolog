(defproject insight-prolog "0.0.1"
  :decription "Get some insight about how prolog works"
  :url "http://github.com/bcardiff/insight-prolog"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [com.lucasbradstreet/instaparse-cljs "1.4.1.0"]]

  :plugins [[lein-cljsbuild "1.1.2"]
            [lein-simpleton "1.3.0"]
            [lein-cooper "1.2.1"]]

  :hooks [leiningen.cljsbuild]

  :cooper {"cljs" ["lein" "cljsbuild" "auto"]
           "web"  ["lein" "simpleton" "8000" "file" ":from" "site"]}

  :cljsbuild {
    :test-commands {
      "unit" ["node" "test-cljs/runner.js"] }

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
            :output-to "out/unit-test.js"
            :output-dir "out"
            :optimizations :none
            :pretty-print true
            :target :nodejs}}
      }}

  :clean-targets ^{:protect false} ["out" "site/javascripts" :target-path])
