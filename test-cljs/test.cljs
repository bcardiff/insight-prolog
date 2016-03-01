(ns insight-prolog.test
  (:require [insight-prolog.core :as c]))

(def success 0)

(def A (c/PVar. "A"))
(def B (c/PVar. "B"))
(def a (c/PAtom. "a"))
(def b (c/PAtom. "b"))
(defn f [& args] (c/PFun. "f" args))
(defn g [& args] (c/PFun. "g" args))

(defn assert_unify [terms s] (assert (= (c/unify terms) s)))

(defn core-test []
  (assert (= (c/lorem) "ipsum!!!"))

  (assert (= (:name A)  "A"))
  (assert (= (:name a)  "a"))

  (assert (= (c/subst A []) A))
  (assert (= (c/subst (f A) [[A b]]) (f b)))
  (assert (= (c/subst (f A B) [[B A]]) (f A A)))
  (assert (= (c/subst a [[B A]]) a))

  (assert_unify [[a a]] [])
  (assert_unify [[a b]] nil)
  (assert_unify [[A b]] [[A b]])
  (assert_unify [[b A]] [[A b]])
  (assert_unify [[A A]] [])
  (assert_unify [[A B]] [[A B]])
  (assert_unify [[B A]] [[B A]])

  (assert_unify [[B A] [B A]] [[B A]])

  (assert_unify [[(f A) (f b)]] [[A b]])
  (assert_unify [[(f A) (f b b)]] nil)
  (assert_unify [[(f A B) (f b a)]] [[B a] [A b]])
  )

(defn -main []
  (.log js/console "\n\ninsight-prolog tests started.")
  (core-test)
  (.log js/console "insight-prolog tests finished.\n\n")
  success)

(set! *main-cli-fn* -main)
