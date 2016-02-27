(ns insight-prolog.test
  (:require [insight-prolog.core :as c]))

(def success 0)

(defn core-test []
  (assert (= (c/lorem) "ipsum!!!")))

(defn -main []
  (.log js/console "insight-prolog tests started.")
  (core-test)
  success)

(set! *main-cli-fn* -main)
