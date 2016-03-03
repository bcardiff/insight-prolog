(ns insight-prolog.test
  (:require [insight-prolog.core :as c]
            [cljs.test :refer-macros [deftest is]]))

(defn i [a] (println a) a)

(def A (c/PVar. "A"))
(def B (c/PVar. "B"))
(def a (c/PAtom. "a"))
(def b (c/PAtom. "b"))
(defn f [& args] (c/PFun. "f" args))
(defn g [& args] (c/PFun. "g" args))

(defn assert_unify [terms s] (is (= (c/unify terms) s)))
(defn assert_axiom [input exp] (is (= (:lhs (first (c/parse input))) exp)))

(deftest core-test
  (assert (= (c/lorem) "ipsum!!!"))

  (assert (= (:name A)  "A"))
  (assert (= (:name a)  "a"))

  (assert (= (c/subst A []) A))
  (assert (= (c/subst (f A) [[A b]]) (f b)))
  (assert (= (c/subst (f A B) [[B A]]) (f A A)))
  (assert (= (c/subst a [[B A]]) a))

  (assert (= (c/vars B) #{B}))
  (assert (= (c/vars (f A B)) #{A B}))

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
  (assert_unify [[(f (f A)) (f B)]] [[B (f A)]])
  (assert_unify [[(f A) (g B)]] nil)
  (assert_unify [[(f (f A)) (f A)]] nil) ; occurs check
  (assert_unify [[(f A) (f (f A))]] nil) ; occurs check

  (assert_axiom "a." a)
  (assert_axiom "b." b)
  (assert_axiom "f(a)." (f a))
  (assert_axiom "f(a,b)." (f a b))
  (assert_axiom "f(A)." (f A))
  )

