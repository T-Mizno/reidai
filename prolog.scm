(use srfi-1) ;; delete-duplicates
(use srfi-1) ;; iota

;; PAIP p.332-
(define fail '("unify-failure"))
(define (is-fail? b)  (equal? fail b))

(define no-bindings '( "no-bindings-unify-success" ) )
(define (is-no-bindings? b)  (equal? no-bindings b))

(define *do-occurs-check* #t)

(define (is-atom? x) (not (or (pair? x) (null? x))))

(define (is-variable? x)
  (and (string? x) (char-upper-case? (string-ref (x->string x) 0))))

(define (get-binding var bindings)
  (assoc var bindings))

(define (lookup-var-value var bindings)
  (cdr (get-binding var bindings)))

(define (extend-bindings var value bindings)
  (cons (cons var value)
	(if (is-no-bindings? bindings)
	    '()
	    bindings)))

(define (unify x y . aBindings)
  (if (not (pair? aBindings))
      (unify x y no-bindings)
      (let ((bindings (car aBindings)))
	(cond ((is-fail? bindings) fail)
	      ((equal? x y) bindings)
	      ((is-variable? x) (unify-variable x y bindings))
	      ((is-variable? y) (unify-variable y x bindings))
	      ((and (pair? x) (pair? y))
	       (unify (cdr x) (cdr y) (unify (car x) (car y) bindings)))
	      (else fail)))))
  
(define (unify-variable var x bindings)
  (cond ((is-fail? bindings) bindings)
	((pair? (get-binding var bindings))
	 (unify (lookup-var-value var bindings) x bindings))
	((and (is-variable? x) (get-binding x bindings))
	 (unify var (lookup-var-value x bindings) bindings))
	((and *do-occurs-check* (occurs-check var x bindings)) fail)
	(else (extend-bindings var x bindings))))

(define (occurs-check var x bindings)
  (cond ((equal? var x) #t)
	((and (is-variable? x) (get-binding x bindings))
	 (occurs-check var (lookup-var-value x bindings) bindings))
	((pair? x)
	 (or (occurs-check var (car x) bindings)
	     (occurs-check var (cdr x) bindings)))
	(else #f)))

(define (reuse-cons x y xy)
  (if (and (equal? x (car xy)) (equal? y (cdr xy)))
      xy
      (cons x y)))

(define (subst-bindings bindings x)
  (cond ((is-fail? bindings) fail)
	((is-no-bindings? bindings) x)
	((and (is-variable? x) (pair? (get-binding x bindings)))
	 (subst-bindings bindings (lookup-var-value x bindings)))
	((is-atom? x) x)
	((equal? '() x) '())
	(else (reuse-cons (subst-bindings bindings (car x))
			  (subst-bindings bindings (cdr x))
			  x))))

(define (unifier x y)
  (subst-bindings (unify x y) x))

(define (clause-head clause) (car clause))
(define (clause-body clause) (cdr clause))

(define (get-clause pred db-pred-clauses) 
  (let ((pred-cs (assoc pred db-pred-clauses)))
    (cond ((not pred-cs) '())
	  ((pair? pred-cs) (cdr pred-cs))
	  ( else '()))))

(define (predicate relation) (car relation))

(define (add-db key val db)
  (cond
   ((not (pair? db)) (cons (cons key (cons val '())) '()))
   ((equal? key (car (car db))) (cons (cons key (cons val (cdr (car db)))) (cdr db)))
   (else (cons (car db) (add-db key val (cdr db))))))

(define (add-clause clause db-pred-clauses)
  (let ((pred (predicate (clause-head clause))))
    (when (not (is-variable? pred))
	  (add-db pred clause db-pred-clauses))))

(define (add-clauses clauses db)
  (fold-right (lambda (c adb) (add-clause c adb)) db clauses))

(define (variables-in exp)
  (delete-duplicates
   (cond 
    ((null? exp) '())
    ((is-variable? exp) (cons exp '()))
    ((pair? exp) 
     (append (variables-in (car exp)) (variables-in (cdr exp))))
    (else '()))))

(define (subst-list bindings x)
  (cond ((null? bindings) x)
	((and (is-variable? x) (pair? (get-binding x bindings)))
	 (subst-bindings bindings (lookup-var-value x bindings)))
	((is-atom? x) x)
	(else (cons (subst-bindings bindings (car x))
		    (subst-bindings bindings (cdr x))))))

(define (rename-variables exp postfixnum)
  (subst-list (map (lambda (v) (cons v  (string-append v "-" (x->string postfixnum))))(variables-in exp)) exp))

(define (prove-all goals bindings db env)
 (cond
   ((is-fail? bindings) (list fail))
   ((null? goals) (list bindings))
   (else  (join (map (lambda (solution) (prove-all (cdr goals) solution db env))
	       (delete fail (join (prove (car goals) bindings db  env))))))))

(define (prove goal bindings db env)
  (if (is-built-in? goal)
      (list (list (dispatch-built-in goal bindings db env)))
      (map (lambda (clause)
	     (let* (
		    (new-env (gensym))
		    (new-clause (rename-variables clause new-env)))
	       (prove-all (clause-body new-clause) (unify goal (clause-head new-clause) bindings) db new-env)))
	   (get-clause (predicate goal) db))))

(define (join lls) (apply append lls))

(define (is-built-in? relation)
  (cond ((not (pair? relation)) (= 0 1))  ; return false
	(else (assoc (car relation) built-ins))))

(define (dispatch-built-in relation bindings db env)
  (let ((op-fn (car (cdr (assoc (car relation) built-ins))))
	(args (cdr relation)))
    (if (not op-fn) fail
	(op-fn args bindings db env))))

(define (built-in-equal args bindings db env)
  (cond ((null? args) fail)
	((null? (cdr args)) fail)
	( else (unify (car args) (car (cdr args)) bindings))))
(define (built-in-set args bindings db env)
  (cond ((null? args) fail)
	((null? (cdr args)) fail)
	( else (extend-bindings (car args) (car (cdr args)) bindings))))
(define (built-in-2arg-math-op op args bindings db env)
  (cond ((< (length args) 3) fail)
	(else
	 (let ((a1 (subst-bindings bindings (car args)))
	       (a2 (subst-bindings bindings (car (cdr args))))
	       (a3 (subst-bindings bindings (car (cdr (cdr args))))))
	   (if (and (not (is-variable? a1)) (not (is-variable? a2)) (is-variable? a3))
	       (let ((num1 (x->integer a1))
		     (num2 (x->integer a2)))
		 (extend-bindings a3 (x->string (op num1 num2)) bindings))
	       fail)))))
(define (built-in-add args bindings db env)
  (built-in-2arg-math-op + args bindings db env))
(define (built-in-sub args bindings db env)
  (built-in-2arg-math-op - args bindings db env))
(define (built-in-multi args bindings db env)
  (built-in-2arg-math-op * args bindings db env))
(define (built-in-div args bindings db env)
  (built-in-2arg-math-op div args bindings db env))
(define (built-in-mod args bindings db env)
  (built-in-2arg-math-op mod args bindings db env))
(define (built-in-rangelist args bindings db env)
  (cond ((< (length args) 3) fail)
	(else
	 (let ((a1 (subst-bindings bindings (car args)))
	       (a2 (subst-bindings bindings (car (cdr args))))
	       (a3 (subst-bindings bindings (car (cdr (cdr args))))))
	   (if (and (not (is-variable? a1)) (not (is-variable? a2)) (is-variable? a3))
	       (let ((num1 (x->integer a1))
		     (num2 (x->integer a2)))
		 (extend-bindings a3 (upto num1 num2) bindings))
	       fail)))))
(define (upto start end) 
  (if (> start end)
      '()
      (map (lambda (n) (x->string n))(iota (+ (- end start) 1) start))))

(define (built-in-math-logic-op op args bindings db env)
  (cond ((< (length args) 2) fail)
	(else
	 (let ((a1 (subst-bindings bindings (car args)))
	       (a2 (subst-bindings bindings (car (cdr args)))))
	   (if (and (not (is-variable? a1)) (not (is-variable? a2)))
	       (let ((num1 (x->integer a1))
		     (num2 (x->integer a2)))
		 (if (op num1 num2)
		     bindings
		     fail)))))))
(define (built-in-gt args bindings db env)
  (built-in-math-logic-op > args bindings db env))
(define (built-in-gte args bindings db env)
  (built-in-math-logic-op >= args bindings db env))
(define (built-in-lt args bindings db env)
  (built-in-math-logic-op < args bindings db env))
(define (built-in-lte args bindings db env)
  (built-in-math-logic-op <= args bindings db env))

(define built-ins
  (list
   (list "is"  built-in-equal)
   (list "set" built-in-set)
   (list "add" built-in-add)
   (list "sub" built-in-sub)
   (list "multi" built-in-multi)
   (list "div" built-in-div)
   (list "mod" built-in-mod)
   (list ">"  built-in-gt)
   (list ">="  built-in-gte)
   (list "<"  built-in-lt)
   (list "<="  built-in-lte)
   (list "rangelist"  built-in-rangelist)
   ))


;;
;; for test
;;

(define (testUnify3)
  (define (testPrim str p)
    (format #t str)
    (format #t "  ==>  ")
    (print p)
    )
  (testPrim "PAIP p.336-1" (unify '("X" "Y") '("Y" "X")))
  (testPrim "PAIP p.336-2" (unify '("X" "Y" "a") '("Y" "X" "X")))
  (set! *do-occurs-check* #f)
  (testPrim "PAIP p.336-3" (unify "X" '("f" "X")))
  (set! *do-occurs-check* #t)
  (testPrim "PAIP p.336-3 with occur check" (unify "X" '("f" "X")))
  (testPrim "PAIP p.338-1" (unifier '("X" "Y" "a") '("Y" "X" "X")))
  (testPrim "PAIP p.338-2" (unifier '(("a" "*" "X" "^" "2") "+" ("B" "*" "X") "+" "C") '("Z" "+" ( "4" "*" "5") "+" "3")))
  (set! *do-occurs-check* #f)
  (testPrim "PAIP p.338-3" (unify "X" '("f" "X")))
  (testPrim "PAIP p.338-4" (unify '("X" "Y") '(("f" "Y") ("f" "X"))))
  (testPrim "PAIP p.338-5" (unify '("X" "Y" "Z") '(("Y" "Z") ("X" "Z") ("X" "Y"))))
)


(define (print-ans bindings)
  (cond
   ((null? bindings) "end")
   (else (print "ans: " (car bindings)) (print-ans (cdr bindings)))))

(define (print-bindings q db)
  (print-ans
   (prove-all q no-bindings (add-clauses db '()) 0)))

(define (print-test q db)
  (print q)
  (print-ans
   (let ((vars (variables-in q)))
     (map (lambda (bindings) 
	    (map (lambda (v) (cons v (subst-bindings bindings v))) vars))
	  (prove-all q no-bindings (add-clauses db '()) 0)))))

(define c4
  '(
    (("likes" "kim" "robin"))
    (("likes" "sandy" "lee"))
    (("likes" "sandy" "kim"))
    (("likes" "robin" "cats"))
    (("likes" "sandy" "X") ("likes" "X" "cats"))
    (("likes" "kim" "X") ("likes" "X" "lee") ("likes" "X" "kim"))
    (("likes" "X" "X"))
     )
)
(define q4 '(("likes"  "sandy" "Who")))
(define q41 '(("likes" "Who" "sandy")))

(define c6
  '(
    (("member" "Item" ("Item" . "Rest")))
    (("member" "Item" ("X" . "Rest")) ("member" "Item" "Rest"))
    (("member2" "X" "Ys")("append" "As" ("X" . "Xs") "Ys")) ; TAP p.61
    (("append" () "Ys" "Ys")) ; TAP p.60 3.15
    (("append" ("X" . "Xs") "Ys" ("X" . "Zs")) ("append" "Xs" "Ys" "Zs"))
    (("prefix" "Xs" "Ys")("append" "Xs" "As" "Ys")) ; TAP p.61
    (("suffix" "Xs" "Ys")("append" "As" "Xs" "Ys")) ; TAP p.61
    (("reverse" () ())) ; TAP p.61 3.16
    (("reverse" ("X" . "Xs") "Zs") ("reverse" "Xs" "Ys") ("append" "Ys" ("X" . ()) "Zs"))
    (("sublist" "Xs" "Ys")("prefix" "Ps" "Ys")("suffix" "Xs" "Ps")) ; TAP p.60 3.14 a
    (("length" () "0"))
    (("length" ("X" . "Xs") "L1") ("length" "Xs" "L") ("add" "L" "1" "L1"))
    (("sumlist" () "0")) ;; TAP p.157 8.6a
    (("sumlist" ("I" . "Is") "Sum") ("sumlist" "Is" "IsSum") ("add" "I" "IsSum" "Sum"))
    (("prodlist" () "1"))
    (("prodlist" ("I" . "Is") "Prod") ("prodlist" "Is" "IsProd") ("multi" "I" "IsProd" "Prod"))
    (("factorial" "N" "F") ("factorial" "N" "1" "F"))
    (("factorial" "0" "F" "F")) ; TAP p.156 8.4
    (("factorial" "N" "T" "F")(">" "N" "0") ("multi" "T" "N" "T1") ("sub" "N" "1" "N1") ("factorial" "N1" "T1" "F"))
    (("factorial2" "N" "F") (">=" "N" "1") ("rangelist" "1" "N" "L") ("prodlist" "L" "F"))
    (("between" "I" "J" "I"))  ; TAP p.157 8.5
    (("between" "I" "J" "K") ("<" "I" "J") ("add" "I" "1" "I1") ("between" "I1" "J" "K"))
    (("gcd" "I" "0" "I")) ; TAB p.152 8.2
    (("gcd" "I" "J" "Gcd") (">" "J" "0") ("mod" "I" "J" "R")("gcd" "J" "R" "Gcd"))
    )
)
(define q60 '(("append" "X" "Y" ("a" "b" "c"))))
(define q61 '(("reverse"  ("a" "b" "c") "Z")))
(define q62 '(("prefix" "X" ("a" "b" "c"))))
(define q63 '(("suffix" "Y" ("a" "b" "c"))))
(define q64 '(("sublist" "X" ("a" "b" "c" "d" "e"))))
(define q65 '(("member" "X" ("1" "2" "3" "4" "5" "6" "7"))))
(define q66 '(("member2" "X" ("1" "2" "3" "4" "5" "6" "7"))))
(define q67 '(("add" "9" "8" "X")))
(define q68 '(("length" ("a" "b" "c" "d" "e") "L")))
(define q69 '(("is" "X" "1")))
(define q6a '(("sumlist" ("0" "1" "2" "3" "4") "Sum")))
(define q6b '((">" "0" "1") "Sum"))
(define q6c '(("factorial" "4" "F")))
(define q6d '(("between" "0" "5" "X")))
(define q6e '(("gcd" "1071" "1029" "X")))
(define q6f '(("rangelist" "0" "10" "L")("prefix" "P" "L") ("sumlist" "P" "Sum")))
(define q6g '(("rangelist" "1" "10" "L")("prefix" "P" "L") ("prodlist" "P" "Prod")))
(define q6h '(("factorial2" "10" "F")))

(define (test6)
  (map (lambda (q) (print-test q c6))
       (list q60 q61 q62 q63 q64 q65 q66 q67 q68 q69 q6a q6b q6c q6d q6e q6f)))

(define q8 '(("set" "X" "8")))

