(ns insight-prolog.core)

(enable-console-print!)

(defn ^:export lorem []
  "ipsum!!!")

(defrecord PAtom [name]) ; Atoms
(defrecord PVar [name]) ; Variables
(defrecord PFun [name args]); Functions

(defmulti subst (fn [term substs] [(type term)]))

(defmethod subst [EmptyList] [_ _]
  [])

(defmethod subst [IndexedSeq] [s substs]
  (mapv (fn [arg] (subst arg substs)) s))

(defmethod subst [PersistentVector] [s substs]
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

(defn unify [terms]
  (if (empty? terms)
    []
    (let [ts (peek terms) g (rest terms)]
      (let [a (first ts) b (second ts) ta (type a) tb (type b)]

        (if (= a b)
          (unify g) ; if same term, remove it
          (if (= ta tb PAtom) nil ; if both are atoms at this point, they differ
            (if (= ta PVar) (cons [a b] (unify (subst g [[a b]])))
              (if (= tb PVar) (cons [b a] (unify (subst g [[b a]]))) nil))
          ))))))
