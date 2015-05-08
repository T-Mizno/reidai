(declare unify)
(declare unify-variable)
(declare prove)
(declare prove-all)
(declare foldr)

(defrecord D [car cdr]) ; dot pair
(defn mcons [h t] (D. h t)) 
(defn is-dot-pair? [c] 
  (cond (nil? c) false
        true (.equals (class c) D)))
(def NIL "mNIL")
(defn is-mcons-nil? [c] (.equals NIL c))
(defn seq2mcons [ls] (foldr (fn [x acc] (if (seq? x) (mcons (seq2mcons x) acc) (mcons x acc))) NIL ls))
(defn mlist 
  ([] NIL)
  ([x & xs]
     (cond (nil? x) NIL
           (seq? x) (if (empty? x) 
                      NIL
                      (mcons (apply mlist x) (apply mlist xs)))
           (empty? xs) (mcons x NIL)
           true (mcons x (apply mlist xs)))))

(defn mcons2str [cs] 
  (if (is-dot-pair? cs)
    (str "("(mcons2str (:car cs))"." (mcons2str (:cdr cs)) ")" )
    (str cs)))
(defn mcons-deep-map [f cs]
  (if (is-dot-pair? cs)
    (mcons (mcons-deep-map f (:car cs)) (mcons-deep-map f (:cdr cs)))
    (f cs)))

(defn equal? [x y]
  (cond
   (and (number? x) (number? y)) (== x y)
   (and (string? x) (string? y)) (.equals x y)
   (and (nil? x) (nil? y)) true
   (and (seq? x) (seq? y)) (if (and (empty? x) (empty? y))
                               true
                               (and (equal? (first x) (first y)) (equal? (rest x) (rest y))))
   (and (map? x) (map? y)) (and (equal? (key (first x)) (key (first y)))
                                (equal? (val (first x)) (val (first y)))
                                (equal? (rest x) (rest y)))
   (and (is-dot-pair? x) (is-dot-pair? y)) (and (equal? (:car x) (:car y))
                                                (equal? (:cdr x) (:cdr y)))
   true false))

(defn debug [op]
  (let [resut op]
    (do
      (println "++++++++++++++++++++++++++++")
      (println op)
      (println)
    op)))

(def fail '{"unify-failure" "NG"})
(def no-bindings '{"no-bindings-unify-success" "OK"})

(defn is-fail? [b]  (equal? b fail))
(defn is-no-bindings? [b] (equal? b no-bindings))

(def flg-do-occurs-check (ref false))
(defn do-occurs-check?[] (deref flg-do-occurs-check))
(defn set-occurs-check! [flg]
  (dosync (ref-set flg-do-occurs-check flg)))

(defn is-atom? [x] (or (string? x) (number? x) (symbol? x)))
(defn is-variable? [x] (and (is-atom? x) (Character/isUpperCase (get (str x) 0))))

(defn lookup-var-value [var bindings]
  (get bindings var fail))

(defn extend-bindings [var value bindings]
  (if (is-no-bindings? bindings)
    {var value}
    (assoc bindings var value)))


(defn occurs-check [var x bindings]
  (cond (equal? var x) true
	(and (is-variable? x) (contains? bindings x)) (occurs-check var (lookup-var-value x bindings) bindings)
        (and (seq? x) (not (seq? x))) (or (occurs-check var (first x) bindings)
                                             (occurs-check var (rest x) bindings))
        (and (is-dot-pair? x) (not (is-dot-pair? x))) (or (occurs-check var (:car x) bindings)
                                                          (occurs-check var (:cdr x) bindings))
        true false))

(defn subst-bindings [bindings x]
  (cond (is-fail? bindings) fail
	(is-no-bindings? bindings) x
	(and (is-variable? x) (contains? bindings x))	 (subst-bindings bindings (lookup-var-value x bindings))
	(is-atom? x) x
	(equal? '() x) '()
        (seq? x) (cons (subst-bindings bindings (first x))
                       (subst-bindings bindings (rest x)))
        (is-dot-pair? x) (mcons (subst-bindings bindings (:car x))
                                (subst-bindings bindings (:cdr x)))
	true x))

(defn unify 
  ([x y]
     (unify x y no-bindings))
  ([x y bindings]
     (cond (is-fail? bindings) fail
           (equal? x y) bindings
           (is-variable? x) (unify-variable x y bindings)
           (is-variable? y) (unify-variable y x bindings)
           (and (seq? x) (seq? y)) (unify (rest x) (rest y) (unify (first x) (first y) bindings))
           (and (is-dot-pair? x) (is-dot-pair? y)) (unify (:cdr x) (:cdr y) (unify (:car x) (:car y) bindings))
           true fail)))


(defn unify-variable [var x bindings]
  (cond (is-fail? bindings) bindings
	(and (is-variable? var) (contains? bindings var)) (unify (lookup-var-value var bindings) x bindings)
        (and (is-variable? x) (contains? bindings x)) (unify var (lookup-var-value x bindings) bindings)
	(and (do-occurs-check?) (occurs-check var x bindings)) fail
	true (extend-bindings var x bindings)))


(defn unifier [x y] (subst-bindings (unify x y) x))


(defn clause-head [clause] (first clause))
(defn clause-body [clause] (rest clause))

(defn get-clause [pred db-pred-clauses]
  (get db-pred-clauses pred '()))

(defn predicate [relation] (:car relation))

(defn add-db [key val db]
  (merge db {key (cons val (get-clause key db))}))

(defn add-clause [clause db-pred-clauses]
  (let [pred (predicate (clause-head clause))]
    (when (not (is-variable? pred))
	  (add-db pred clause db-pred-clauses))))
(defn foldr [f init xs]
  (cond (empty? xs) init
        true (f  (first xs) (foldr f init (rest xs)))))

;(defn join [lls] (foldr into '() lls))
(defn join [lls] (foldr concat '() lls))

(defn add-clauses [clauses db]
  (foldr (fn [c adb] (add-clause c adb)) db clauses))

(defn variables-in [exp]
  (set
   (cond 
    (symbol? exp) '()
    (and (seq? exp) (empty? exp)) '()
    (is-variable? exp) (cons exp '())
    (seq? exp) (into (variables-in (first exp)) (variables-in (rest exp)))
    (is-dot-pair? exp) (into (variables-in (:car exp)) (variables-in (:cdr exp)))
    true '())))

(defn subst-list [bindings x]
  (cond (empty? bindings) x
        (is-no-bindings? bindings) x
        (is-fail? bindings) x
	(is-atom? x) x
	(and (is-variable? x) (contains? bindings  x)) (subst-bindings bindings (lookup-var-value x bindings))
        (is-dot-pair? x) (mcons (subst-bindings bindings (:car x))
                                (subst-bindings bindings (:cdr x)))
	(seq? x) (cons (subst-bindings bindings (first x))
                       (subst-bindings bindings (rest x)))
        true x))

(defn rename-variables [exp postfixnum]
  (subst-list  (apply merge (map (fn [v] {v  (str v "-" (.toString postfixnum))})(variables-in exp))) exp))


(defn built-in-2args-math-logic-op [op args bindings db env]
  (let [n1 (:car args)
        n2 (:car (:cdr args))]
    (if (and (number? n1) (number? n2) (op n1 n2))
      (list (list bindings))
      (list (list fail))
      )))
(defn built-in-gt [args bindings db env]
  (built-in-2args-math-logic-op > args bindings db env))


(defn built-in-2args-math-op [op args bindings db env]
  (let [n1 (:car args)
        n2 (:car (:cdr args))
        result (:car (:cdr (:cdr args)))]
    (if (and (number? n1) (number? n2) (is-variable? result))
      (list (list (extend-bindings result (op n1 n2) bindings)))
      (list (list fail))
      )))

(defn built-in-2args-math-add [args bindings db env]
  (built-in-2args-math-op + args bindings db env))
(defn built-in-2args-math-sub [args bindings db env]
  (built-in-2args-math-op - args bindings db env))
(defn built-in-2args-math-multi [args bindings db env]
  (built-in-2args-math-op * args bindings db env))
(defn built-in-2args-math-rem [args bindings db env]
  (built-in-2args-math-op rem args bindings db env))

(defn built-in-prove [args bindings db env]
  (prove args bindings db env))

(def built-ins 
  '{"add" built-in-2args-math-add
    ,"sub" built-in-2args-math-sub
    ,"multi" built-in-2args-math-multi
    ,"mod" built-in-2args-math-rem
    ,"prove" built-in-prove
    ,">" built-in-gt
    })

(defn is-built-in? [relation]
  (contains? built-ins (predicate relation)))

(defn dispatch-built-in [relation bindings db env]
  (let [f (get built-ins (predicate relation) fail)]
    (if (is-fail? f)
      fail
      ((eval f) (subst-bindings bindings (:cdr relation)) bindings db env))))


(defn prove-all [goals bindings db env]
 (cond
  (is-fail? bindings) (list bindings)
;  (empty? bindings) (list fail)
  (empty? goals) (list bindings)
  (is-mcons-nil? goals) (list bindings)
  true (join (map (fn [solution] (prove-all (rest goals) solution db env))
              (filter (fn [b] (not (or (is-fail? b) (empty? b))))
                      (join (prove (first goals) bindings db  env)))))))

(defn prove [goal bindings db env]
  (if (is-built-in? goal)
    (dispatch-built-in goal bindings db env)
    (map (fn [clause]
           (let* [new-clause (rename-variables clause (gensym))
                  new-terget (clause-head new-clause)
                  new-binds (unify goal new-terget bindings)]
                 (do
                   (prove-all (clause-body new-clause) new-binds db (+ env 1)))))
           (get-clause (predicate goal) db))))


;;; test
(defn testPrim [str p]
  (print str)
  (print "  ==>  ")
  (println (if (is-dot-pair? p) (mcons2str p) p))
  )
(defn testUnify3 []
  (testPrim "PAIP p.336-1" (unify (seq2mcons '("X" "Y")) (seq2mcons '("Y" "X"))))
  (testPrim "PAIP p.336-2" (unify (seq2mcons '("X" "Y" "a")) (seq2mcons '("Y" "X" "X"))))
  (set-occurs-check! false)
  (testPrim "PAIP p.336-3" (unify "X" (seq2mcons '("f" "X"))))
  (set-occurs-check! true)
  (testPrim "PAIP p.336-3 with occur check" (unify "X" (seq2mcons '("f" "X"))))
  (testPrim "PAIP p.338-1" (unifier (seq2mcons '("X" "Y" "a")) (seq2mcons '("Y" "X" "X"))))
  (testPrim "PAIP p.338-2" (unifier (seq2mcons '(("a" "*" "X" "^" "2") "+" ("B" "*" "X") "+" "C")) (seq2mcons '("Z" "+" ( "4" "*" "5") "+" "3"))))
  (set-occurs-check! false)
  (testPrim "PAIP p.338-3" (unify "X" (seq2mcons '("f" "X"))))
  (testPrim "PAIP p.338-4" (unify (seq2mcons '("X" "Y")) (seq2mcons '(("f" "Y") ("f" "X")))))
  (testPrim "PAIP p.338-5" (unify (seq2mcons '("X" "Y" "Z")) (seq2mcons '(("Y" "Z") ("X" "Z") ("X" "Y")))))
  (testPrim "PAIP p.338-2" (unify (seq2mcons '(("a" "*" "X" "^" "2") "+" ("B" "*" "X") "+" "C")) (seq2mcons '("Z" "+" ( "4" "*" "5") "+" "3"))))
)

(def q3 (list (seq2mcons '(("a" "*" "X" "^" "2") "+" ("B" "*" "X") "+" "C"))))

(defn print-ans [bindings]
  (cond
   (empty? bindings) "end"
   true (do (println "ans: " (first bindings)) (print-ans (rest bindings)))))

(defn print-bindings [q db]
  (print-ans
   (prove-all q no-bindings (add-clauses db {}) 0)))

(defn print-test [q db]
  (println (map mcons2str q))
  (print-ans
   (let [vars (variables-in q)]
     (println vars)
     (map (fn [bindings]
	    (map (fn [v] {v 
                          (let [val (subst-bindings bindings v)]
                            (if (is-dot-pair? val) (mcons2str val) val))}) vars))
	  (prove-all q no-bindings (add-clauses db {}) 1)))))


(def c40
  '(
    (("likes" "kim" "robin"))
    (("likes" "sandy" "lee"))
     )
)
(def c4
  (list
   (list (seq2mcons '("likes" "kim" "robin")))
   (list (seq2mcons '("likes" "sandy" "lee")))
   (list (seq2mcons '("likes" "sandy" "kim")))
   (list (seq2mcons '("likes" "robin" "cats")))
   (list (seq2mcons '("likes" "sandy" "X"))(seq2mcons '("likes" "X" "cats")))
   (list (seq2mcons '("likes" "kim" "X")) (seq2mcons '("likes" "X" "lee"))(seq2mcons '("likes" "X" "kim")))
   (list (seq2mcons '("likes" "X" "X")))
     )
)
;(def q40 '(("likes" "kim" "X") ("likes" "X" "lee") ("likes" "X" "kim")))
(def q4 (list (seq2mcons '("likes"  "sandy" "Who"))))
(def q41 (list (seq2mcons '("likes" "Who" "sandy"))))


(def c6
  (list
   (list (mlist "member" "Item" (mcons "Item" "Rest")))
   (list (mlist "member" "Item" (mcons "X" "Rest"))
         (mlist "member" "Item" "Rest"))
   (list (mlist "member2" "X" "Ys")(mlist "append" "As" (mcons "X" "Xs") "Ys")) ; TAP p.61
   (list (mlist "append" NIL "Ys" "Ys")) ; TAP p.60 3.15
   (list (mlist "append" (mcons "X" "Xs") "Ys" (mcons "X" "Zs"))
         (mlist "append" "Xs" "Ys" "Zs"))
   (list (mlist "prefix" "Xs" "Ys")
         (mlist "append" "Xs" "As" "Ys")) ; TAP p.61
   (list (mlist "suffix" "Xs" "Ys")
         (mlist "append" "As" "Xs" "Ys")) ; TAP p.61
   (list (mlist "reverse" NIL NIL)) ; TAP p.61 3.16
   (list (mlist "reverse" (mcons "X" "Xs") "Zs")
         (mlist "reverse" "Xs" "Ys") 
         (mlist "append" "Ys" (mcons "X" NIL) "Zs"))
   (list (mlist "sublist" "Xs" "Ys")
         (mlist "prefix" "Ps" "Ys")
         (mlist "suffix" "Xs" "Ps")) ; TAP p.60 3.14 a
   (list (mlist "length" NIL 0))
   (list (mlist "length" (mcons "X" "Xs") "L1")
         (mlist "length" "Xs" "L")
         (mlist "add" "L" 1 "L1"))
   (list (mlist "sumlist" NIL 0)) ;; TAP p.157 8.6a
   (list (mlist "sumlist" (mcons "I" "Is") "Sum")
         (mlist "sumlist" "Is" "IsSum") 
         (mlist "add" "I" "IsSum" "Sum"))
   (list (mlist "fold" "Op" "Init" NIL "Init"))
   (list (mlist "fold" "Op" "Init" (mcons "I" "Is") "Acc")
         (mlist "fold" "Op" "Init" "Is" "IsAcc")
         (mlist "prove" "Op" "I" "IsAcc" "Acc"))
   (list (mlist "sumlist2" "Ns" "Sum")
         (mlist "fold" "add" 0 "Ns" "Sum"))
   (list (mlist "prodlist" "Ns" "Prod")
         (mlist "fold" "multi" 1 "Ns" "Prod"))
   (list (mlist "factorial" "N" "F")
         (mlist "factorial" "N" 1 "F"))
   (list (mlist "factorial" 0 "F" "F")) ; TAP p.156 8.4
   (list (mlist "factorial" "N" "T" "F")
         (mlist ">" "N" 0)
         (mlist "multi" "T" "N" "T1")
         (mlist "sub" "N" 1 "N1")
         (mlist "factorial" "N1" "T1" "F"))
   ;;  (("factorial2" "N" "F") (">=" "N" "1") ("rangelist" "1" "N" "L") ("prodlist" "L" "F"))
   ;;  (("between" "I" "J" "I"))  ; TAP p.157 8.5
   ;;  (("between" "I" "J" "K") ("<" "I" "J") ("add" "I" "1" "I1") ("between" "I1" "J" "K"))
   (list (mlist "gcd" "I" 0 "I")) ; TAB p.152 8.2
   (list (mlist "gcd" "I" "J" "Gcd")
         (mlist ">" "J" 0)
         (mlist "mod" "I" "J" "R")
         (mlist "gcd" "J" "R" "Gcd"))
))

;(def q60 (list (mlist "append" "X" "Y" '("a" "b" "c")) (mlist "prefix" "P" "X")))
;(def q60 (list (mlist "append" "X" "Y" '("a" )) (mlist "prefix" "P" "X")))
;(def q60 (list (mlist "prefix" "P" '("a"))))
(def q60 (list (mlist "append" "X" "Y" (range 1 10))))
;; (def q60 (list (seq2mcons '("append" "X" "Y" ( ("a" "b" "c"))))))
(def q61 (list (mlist "reverse"  '("a" "b" "c") "Z")))
(def q62 (list (mlist "prefix" "X" '("a" "b" "3" "4"))))
(def q63 (list (mlist "suffix" "Y" '("a" "b" "c"))))
(def q64 (list (mlist "sublist" "S" '("a" "b" "3" "4"))))
(def q65 (list (mlist "member" "X" (range 1 11))))
(def q66 (list (mlist "member2" "X" (range 1 8))))
(def q68 (list (mlist "length" '("a" "b" "c" "d" "e") "L")))
(def q6a (list (mlist "sumlist" (range 1 11) "Sum")))
(def q6a1 (list (mlist "sumlist2" (range 1 11) "Sum")))
(def q6a2 (list (mlist "prodlist" (range 1 11) "Prod")))
(def q6c (list (mlist "factorial" 10 "F")))
(def q6e (list (mlist "gcd" 1071 1029 "X")))

(def q6z (list (mlist "prefix" "Xs" (range 1 10))
               (mlist "prodlist" "Xs" "Prod")))
;(def q6z (list (list "." (list "." "b" '()) "a")))
;(def q6z (list (list "prefix" "X" (list "." "a" (list "." "b" '()))) (list "member" "E" "X")))
;(def q6z '(("prefix" "X" ("a" ("b" ()))) ("member" "E" "X")))
;(def q6z (list (list "member" "X" (list "." "a" (list "." "b" '())))))

;(print-ans (map (fn [b] (subst-bindings b q6z)) (prove-all q6z no-bindings (add-clauses c6 {}) 0)))

