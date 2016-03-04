(ns insight-prolog.test
  (:require [insight-prolog.core :as c]
            [cljs.test :refer-macros [deftest is]]
            [instaparse.core :as insta] ))

(defn i [a] (println a) a)

(def A (c/PVar. "A"))
(def A_1 (c/PVar. "A₁"))
(def A_2 (c/PVar. "A₂"))
(def B (c/PVar. "B"))
(def B_2 (c/PVar. "B₂"))
(def a (c/PAtom. "a"))
(def b (c/PAtom. "b"))
(defn f [& args] (c/PFun. "f" args))
(defn g [& args] (c/PFun. "g" args))

(defn assert_unify [terms s] (is (= (c/unify terms) s)))
(defn assert_parse [input]
  (let [r (c/parse input)]
    (is (= false (insta/failure? r)) (insta/get-failure r)) r))
(defn assert_axiom [input lhs] (is (= (:lhs (first (assert_parse input))) lhs)))
(defn assert_rule [input lhs rhs]
  (let [p (assert_parse input)]
    (is (= (:lhs (first p)) lhs))
    (is (= (:rhs (first p)) rhs)))
    )

(deftest core-test
  (is (= (c/lorem) "ipsum!!!"))
  (is (= (:name A)  "A"))
  (is (= (:name a)  "a")))

(deftest core-subst
  (is (= (c/subst A []) A))
  (is (= (c/subst (f A) [[A b]]) (f b)))
  (is (= (c/subst (f A B) [[B A]]) (f A A)))
  (is (= (c/subst a [[B A]]) a))
  (is (= (c/subst (f A B) [[A a] [B b]]) (f a b)))
  (is (= (c/subst (f A B) [[A a]]) (f a B)))

  (is (= (c/subst (c/PRule. (f A) [(f B) (g a) (g A)]) [[A a]])
             (c/PRule. (f a) [(f B) (g a) (g a)])))
  )

(deftest core-vars
  (is (= (c/vars B) #{B}))
  (is (= (c/vars (f A B)) #{A B}))
  (is (= (c/vars (c/PRule. (f A) [(f B) (g a) (g A)]) #{A B})))
  )

(deftest core-unify
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
  )

(deftest core-parse
  (assert_axiom "a." a)
  (assert_axiom "b." b)
  (assert_axiom "f(a)." (f a))
  (assert_axiom "f(a,b)." (f a b))
  (assert_axiom "f(A)." (f A))
  (assert_rule "f(A):-g(A)." (f A) [(g A)])
  (assert_rule "f(A):-f(B),g(B,A)." (f A) [(f B) (g B A)])
  (assert_rule "   f( A ) :-   f( B ),
               g( B , A )   ." (f A) [(f B) (g B A)])

  (is (= (assert_parse "
                       f(a).
                       f(f(B)) :- f(B).
                       ")
         [
          (c/PRule. (f a) [])
          (c/PRule. (f (f B)) [(f B)])
          ]))
  )

(deftest core-incarnation
  (is (= (c/incarnation "A" 1) "A₁"))
  (is (= (c/incarnation "A" 456) "A₄₅₆"))

  (is (= (c/rule-incarnation (c/PRule. (f A) [(f B) (g a) (g A)]) 2)
         (c/PRule. (f A_2) [(f B_2) (g a) (g A_2)])))
  )

(deftest core-is-candidate-rule
  (is (c/is-candidate-rule [(f a)] (c/PRule. (f A) [])))
  (is (not (c/is-candidate-rule [(f a)] (c/PRule. (g a) []))))
  )

(deftest core-one-step
  (let [p (c/prepare-program "
                           g(a).
                           f(A) :- g(A).
                           f(b).
                           ")]
      (let [goal (c/parse "?- f(B).") [goals n] (c/one-step p goal)]
        (is (= n 2))
        (is (= goals [
                [[[B A_1]] [(g A_1)]]
                [[[B b]]   []]
                ]))
      )
    ))
