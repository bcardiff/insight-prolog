# Insight Prolog

Get some insight about how prolog works

# Develop

* Install [leiningen](http://leiningen.org/)
* Install dependencies `lein deps`
* To compile and start a server for static assets `lein cooper`.
  * To just compile sources `lein cljsbuild once` or for continuous compilation `lein cljsbuild auto`
* Open [http://localhost:8000](http://localhost:8000/) to open files from `./site`
* Run clojure scripts tests `lein cljsbuild test`
  * Continuous running tests `ls -d out/* | entr node test-cljs/runner.js` together with `lein cljsbuild auto`
