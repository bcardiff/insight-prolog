(ns insight-prolog.core)

(enable-console-print!)

(defn ^:export lorem []
  "ipsum!!!")

(defrecord PAtom [name]) ; Atoms
(defrecord PVar [name]) ; Variables
(defrecord PFun [name args]); Functions

(defmulti subst (fn [term substs] [(type term)]))

(defmethod subst [PersistentVector] [s substs]
  (mapv (fn [arg] (subst arg substs)) s))

(defmethod subst [ISeq] [s substs]
  (mapv (fn [arg] (subst arg substs)) s))

(defmethod subst [PAtom] [a _]
  a)

(defmethod subst [PVar] [a substs]
  (if (empty? substs)
    a
    (let [[[v t] othersubsts] substs]
      (if (= a v) t (subst a othersubsts)))))

(defmethod subst [PFun] [{:keys [name args]} substs]
  (PFun. name (mapv (fn [arg] (subst arg substs)) args)))

(defmulti vars (fn [term] (type term)))
(defmethod vars PAtom [_] #{})
(defmethod vars PVar [a] #{a})
(defmethod vars PFun [f] (set (flatten (map seq (map vars (:args f))))) )


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

(defn unify_vars [v other g]
  (if ((vars other) v)
    nil
    (cons [v other] (unify (subst g [[v other]])))))
