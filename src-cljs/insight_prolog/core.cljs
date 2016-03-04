(ns insight-prolog.core
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(enable-console-print!)

(defn ^:export lorem []
  "ipsum!!!")

(defrecord PAtom [name])    ; Atoms
(defrecord PVar [name])     ; Variables
(defrecord PFun [name args]); Functions

(defrecord PRule [lhs rhs]) ; Rules

(defrecord PRuntime [program incarnation])

(def grammar
  (insta/parser
    "<S> = (<whitespace>? | axiom | rule | goal)*
     axiom = clause <'.'>
     rule = clause <':-'> comma_clause <'.'>
     <goal> = <'?-'> comma_clause <'.'>
     <comma_clause> = clause (<','> comma_clause)?

     <clause> = term
     <term> = <whitespace>? ( var | atom | fun ) <whitespace>?
     fun = atom_ident <'('> comma_term <')'>
     <comma_term> = term (<','> comma_term)?
     atom = atom_ident
     var = var_indent

     <atom_ident> = #'[a-z]+'
     <var_indent> = #'[A-Z]+'

     whitespace = #'\\s+'
     "))

(defn parse [str]
  (insta/transform {
    :atom (fn [ident] (PAtom. ident))
    :var (fn [ident] (PVar. ident))
    :fun  (fn [name & args] (PFun. name args))
    :axiom (fn [lhs] (PRule. lhs []))
    :rule (fn [lhs & rhs] (PRule. lhs rhs))
    } (grammar str)))

(defmulti subst (fn [term substs] [(type term)]))

(defmethod subst [PersistentVector] [s substs]
  (mapv (fn [arg] (subst arg substs)) s))

(defmethod subst [ISeq] [s substs]
  (mapv (fn [arg] (subst arg substs)) s))

(defmethod subst [LazySeq] [s substs]
  (mapv (fn [arg] (subst arg substs)) s))

(defmethod subst [PAtom] [a _]
  a)

(defmethod subst [PVar] [a substs]
  (if (empty? substs)
    a
    (let [s (first (filter (fn [e] (= (first e) a)) substs))]
      (if (nil? s) a (second s)))))

(defmethod subst [PFun] [{:keys [name args]} substs]
  (PFun. name (mapv (fn [arg] (subst arg substs)) args)))

(defmethod subst [PRule] [{:keys [lhs rhs]} substs]
  (PRule. (subst lhs substs) (subst rhs substs)))

(defmulti vars (fn [term] (type term)))
(defmethod vars PAtom [_] #{})
(defmethod vars PVar [a] #{a})
(defmethod vars PFun [f] (set (flatten (map seq (map vars (:args f))))) )
(defmethod vars PRule [r] (remove nil? (set (concat (vars (:lhs r)) (flatten (map seq (map vars (:rhs r))))))))

(defn unify_vars [v other g]
  (if ((vars other) v)
    nil
    (cons [v other] (unify (subst g [[v other]])))))

(defn unify [terms]
  (if (empty? terms)
    []
    (let [ts (peek terms) g (pop terms)]
      (let [a (first ts) b (second ts) ta (type a) tb (type b)]

        (if (= a b)
          (unify g) ; if same term, remove it
          (if (= ta tb PAtom) nil ; if both are atoms at this point, they differ
            (if (= ta PVar) (unify_vars a b g)
              (if (= tb PVar) (unify_vars b a g)
                (if (and (= ta tb) (= (:name a) (:name b)))
                  ; types are the same now, neither are variables
                  ; and their names are the same
                  (if (and (= ta PFun) (= (count (:args a)) (count (:args b))))
                    (unify (vec (concat g (map vector (:args a) (:args b)))))
                    nil ; if types differ, there is no chance
                  ))))
          ))))))


(defn incarnation [name num]
  (+ name (str/replace (.toString num) #"." (fn [d] (nth "₀₁₂₃₄₅₆₇₈₉" (int d))))) )

(defn rule-incarnation [rule num]
  (let [vs (vars rule)
        rule-subst (vec (map (fn [v] [v (PVar. (incarnation (:name v) num))]) vs))
        ]
    (subst rule rule-subst)
    ))

(defn is-candidate-rule [goal rule]
  (= (:name (:lhs rule)) (:name (first goal))))

(defn prepare-program [input]
  (PRuntime. (parse input) 0))

