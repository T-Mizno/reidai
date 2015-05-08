(defpackage :myzLinear
  (:use :common-lisp))

;;(load "C:/Users/<anypath>/myzLinear.lisp")
;;(usepackage :myzLinear)
;(load "C:/Users/myzno/Desktop/20131113s-00workBench/myzLinear.lisp")

;; spec
(defconstant NUMTYPE 'double-float)
(defconstant epsillon 1.0e-6)
(defun near? (x y)
  (< (abs (- x y)) epsillon))
(defun isZero? (x)
  (near? x (coerce 0.0 NUMTYPE)))

;; util
(defun upto (start end)
  (if (> start end ) 
      '()
      (cons start (upto (+ start 1) end))))

(defun downto (start end)
  (if (< start end)
      '()
      (cons start (downto (- start 1) end))))

(defun hasElements? (vs)
  (not (null vs)))

(defun directProduct (xs ys)
  (apply #'concatenate 'list (mapcar (lambda (x) (mapcar (lambda (y) (list x y)) ys)) xs)))

(defun drop (vs n)
  (if (<= n 0)
      vs
      (drop (cdr vs) (- n 1))))

(defun sqr (x) (* x x))

;; round
(defun myRound (number)
  (values (floor (+ 0.5 number))))

;; fold
(defun foldl (f init vs)
  (if (hasElements? vs)
      (foldl f (funcall f  init (car vs)) (cdr vs))
      init))
(defun foldlUpto (f init start end)
  (if (<= start end)
      (foldlUpto f (funcall f  init start) (+ start 1) end)
      init))

;; filter
(defun filter (p xs)
  (cond ((null xs) nil)
	((funcall p (car xs))
	 (cons (car xs) (filter p (cdr xs))))
	(t (filter p (cdr xs)))))
(defun in? (i is)
  (if (hasElements? is)
    (if (eq i (car is))
	T
	(in? i (cdr is)))
    NIL))
  
;; matrix
(defclass matrix ()
  (iStart iEnd jStart jEnd values rowIndices colIndices indices))
  
(defun newMatrix (is ie js je)
  (let ( (mat (make-instance 'matrix)) )
    (setf (slot-value mat 'iStart) is)
    (setf (slot-value mat 'iEnd) ie)
    (setf (slot-value mat 'jStart) js)
    (setf (slot-value mat 'jEnd) je)
    (setf (slot-value mat 'values) (make-array (list (+ (- ie is) 1)  (+ (- je js) 1)) :element-type NUMTYPE))
    (setf (slot-value mat 'rowIndices) (upto is ie))
    (setf (slot-value mat 'colIndices) (upto js je))
    (setf (slot-value mat 'indices) (directProduct (upto is ie) (upto js je)))
    mat))

(defun newMatrixFromList- (is ie js je vs)
  (setList- (newMatrix is ie js je) vs))
(defun newMatrixFromList@ (is ie js je vs)
  (setList@ (newMatrix is ie js je) vs))
(defun newMatrixFromList (is ie js je vs)
  (setList (newMatrix is ie js je) vs))

(defun m (mat) (car (array-dimensions (slot-value mat 'values))))
(defun n (mat) (car (cdr (array-dimensions (slot-value mat 'values)))))

(defun iStart (mat) (slot-value mat 'iStart))
(defun iEnd (mat) (slot-value mat 'iEnd))
(defun jStart (mat) (slot-value mat 'jStart))
(defun jEnd (mat) (slot-value mat 'jEnd))

(defun rowIndices (mat) (slot-value mat 'rowIndices))
(defun colIndices (mat) (slot-value mat 'colIndices))
(defun indices (mat) (slot-value mat 'indices))

(defun array (mat) (slot-value mat 'values))

(defun id-i (id) (car id))
(defun id-j (id) (car (cdr id)))

(defun i-zi (mat i) (- i (iStart mat)))
(defun j-zj (mat j) (- j (jStart mat)))

(defun inRange? (mat i j) (and (<= (iStart mat) i) (<= i (iEnd mat)) (<= (jStart mat) j) (<= j (jEnd mat))))
(defun inRange@? (mat id) (inRange? mat (id-i id) (id-j id)))

(defun at (mat i j) (aref (array mat) (i-zi mat i) (j-zj mat j)))
(defun setVal (mat i j v) (when (inRange? mat i j)(setf (aref (array mat) (i-zi mat i) (j-zj mat j)) (coerce v NUMTYPE)))) ; with range check
;(defun setVal (mat i j v) (setf (aref (array mat) (i-zi mat i) (j-zj mat j)) (coerce v NUMTYPE))) ; with out range check
(defun at@ (mat id) (at mat (id-i id) (id-j id)))
(defun setVal@ (mat id v) (setVal mat (id-i id) (id-j id) v))

;; fill
(defun matFill (mat v)
  (loop for id in (indices mat)
       do (setVal@ mat id v)))

;; set list ((i j) v)* form
(defun setList@ (mat vs)
  (when (hasElements? vs)
    (setVal@ mat (car (car vs)) (car (cdr (car vs))))
    (setList@ mat (cdr vs)))
  mat)

;; set list (i j v)* form
(defun setList (mat vs)
  (when (hasElements? vs)
    (setVal mat (car (car vs)) (car (cdr (car vs))) (car(cdr(cdr(car vs)))))
    (setList mat (cdr vs)))
  mat)
;; set list (v)* form
(defun setList- (mat vs)
  (_setList- mat (indices mat) vs)
  mat)
(defun _setList- (mat ids vs)
  (when (and (hasElements? ids) (hasElements? vs))
    (setVal@ mat (car ids) (car vs))
    (_setList- mat (cdr ids) (cdr vs))))

(defun foreachList (vs proc)
  (loop for v in vs
     do (apply proc (list v))))
(defun forMap (vs proc) (mapcar proc vs))
(defun foreachElements (mat proc)
    (foreachList (indices mat) (lambda (id) (apply proc (list mat (id-i id) (id-j id))))))
(defun foreachElements@ (mat proc)
    (foreachList (indices mat) (lambda (id) (apply proc (list mat id)))))

(defun copyTo (fromMat toMat)
  (loop for id in (indices fromMat)
       do (setVal@ toMat id (at@ fromMat id))))

(defun copy (fromMat)
  (let ((mat (newMatrix (iStart fromMat) (iEnd fromMat) (jStart fromMat) (jEnd fromMat))))
    (copyTo fromMat mat)
    mat))

(defun averageRow (mat)
  (let ( (x (newMatrix 0 0 (jStart mat) (jEnd mat))) )
    (loop for j in (colIndices mat)
	 do (setVal x 0 j
		    (/
		     (foldl (lambda (init k) (+ init (at mat k j))) 0.0 (rowIndices mat))
		     (M mat))))
  x))

(defun norm1OfColumn (mat j)
  (apply #'+ (mapcar (lambda (i) (abs (at mat i j))) (rowIndices mat))))
(defun norm2OfColumn (mat j)
  (sqrt (transProduct mat j mat j)))
(defun normalizeColumn! (mat j)
  (let ((denom (norm1OfColumn mat j)))
    (loop for i in (rowIndices mat)
       do (setVal mat i j (/ (at mat i j) denom)))))

(defun transpose (mat)
  (let ( (rMat (newMatrix (jStart mat) (jEnd mat) (iStart mat) (iEnd mat))) )
    (foreachElements rMat (lambda (aMat i j) (setVal aMat i j (at mat j i))))
    rMat))

;; operations
(defun add! (mat1 mat2 that)
  (foreachElements mat1 (lambda (mat i j) (setVal that i j (+ (at mat i j) (at mat2 i j)))))
  that)
(defun add (mat1 mat2) (add! mat1 mat2 (copy mat1)))

(defun sub! (mat1 mat2 that)
  (foreachElements mat1 (lambda (mat i j) (setVal that i j (- (at mat i j) (at mat2 i j)))))
  that)
(defun sub (mat1 mat2) (sub! mat1 mat2 (copy mat1)))

(defun multiScalar! (v mat1 that)
  (foreachElements mat1 (lambda (mat i j) (setVal that i j (* v (at mat i j)))))
  that)
(defun multiScalar (v mat) (multiScalar! v mat (copy mat)))

(defun inProduct (mat1 i mat2 j)
  (let ( (ks (colIndices mat1)) )
    (apply #'+ (mapcar (lambda (k) (* (at mat1 i k) (at mat2 k j))) ks))))
(defun multi! (mat1 mat2 that)
  (let ((ijs (directProduct (rowIndices mat1) (colIndices mat2))))
    (foreachList ijs (lambda (ij) (setVal that (car ij) (car (cdr ij)) (inProduct mat1 (car ij) mat2 (car (cdr ij)))))))
  that)
(defun multi (mat1 mat2)
  (let ( (mat (newMatrix (iStart mat1) (iEnd mat1) (jStart mat2) (jEnd mat2))) )
    (multi! mat1 mat2 mat)))

(defun transProduct (mat1 j1 mat2 j2)
  (apply #'+ (mapcar (lambda (i) (* (at mat1 i j1) (at mat2 i j2))) (rowIndices mat1))))

;; standard out
(defun stdout (mat)
  (format T "   ")
  (foreachList (colIndices mat) (lambda (j) (format T "   ~D" j)))
  (format T "~%")
  (foreachElements mat
	  (lambda (aMat i j)
	    (if (eq j (jStart aMat))(format T "~D: " i))
	    (format T "  ~D" (at aMat i j))
	    (if(eq j (jEnd aMat)) (format T "~%"))
      )))

;; represent permutation
(defun newP (mat) (newMatrixFromList- (iStart mat) (iEnd mat) 0 0 (rowIndices mat)))

(defun P> (p i) (myRound (at P i 0) ))
(defun swapP (P i k)
  (let ((tmp (at P i 0)))
    (setVal P i 0 (at P k 0))
    (setVal P k 0 tmp)))

(defun atP (p mat i j) (at mat (P> p i) j))

;; pivots
(defun newPivots () '())
(defun addPivots (pivots i j) (cons (cons i j) pivots))
(defun pivotsRowIndices (pivots) (mapcar (lambda (ij) (car ij)) pivots))
(defun pivotsColIndices (pivots) (mapcar (lambda (ij) (cdr ij)) pivots))
(defun pivotsI2J (pivots i)  (cdr (find-if (lambda (ij) (eq (car ij) i)) pivots)))
(defun pivotsJ2I (pivots j)  (car (find-if (lambda (ij) (eq (cdr ij) j)) pivots)))

;;
;; gaussian elimination
;;
(defun maxIofFI (from to f tmpMaxI tmpMaxV)
  (if (> from to)
      tmpMaxI
      (let ( (v (apply f (list from))) )
	(if (> v tmpMaxV)
	    (maxIofFI (+ from 1) to f from v)
	    (maxIofFI (+ from 1) to f tmpMaxI tmpMaxV)))))

(defun searchMaxI (P A fromi j)
  (maxIofFI fromi (iEnd A) (lambda (i) (abs (atP P A i j)))
	    fromi (abs (atP P A fromi j))))

(defun updateP (P A p-i p-j)  (swapP P p-i (searchMaxI P A p-i p-j)))

(defun forwardOneLine (P A b pivotI pivotJ i)
  (when (isZero? (atP P A pivotI pivotJ)) (abort "gauss forward one line : pivot is zero." ))
  (let* ((pvi (P> P pivotI))
	 (pci (P> P i))
	 (pv (/ (at A pci pivotJ) (at A pvi pivotJ)))
	 )
    (loop for j from (+ pivotJ 1) to (jEnd A) 
	 do (setVal A pci j
		    (- (at A pci j)
		       (* pv (at A pvi j)))))
    (setVal b pci 0
	  (- (at b pci 0)
	     (* pv (at b pvi 0))))
    (setVal A pci pivotJ pv)))

(defun forwardRows (P A b pivotI pivotJ)
  (loop for i from (+ pivotI 1) to (iEnd A)
     do (forwardOneLine P A b pivotI pivotJ i)))

(defun everyUpto (pred start end)
  (let ( (flg T) )
    (loop for i from start to end
       do (when (not (apply pred (list i)))
	    (setf flg Nil)
	    (return)))
    flg))
(defun underColumnsAreZero? (P A i j)
  (everyUpto (lambda (k) (isZero? (atP P A k j))) i (iEnd A)))

(defun nextPivotj (P A pivotI apivotJ)
  (loop for pivotJ from apivotJ to (jEnd A)
     do (if (not (inRange? A pivotI pivotJ))
	    (return NIL)
	    (progn
	      (updateP P A pivotI pivotJ)
	      (if (underColumnsAreZero? P A pivotI pivotJ)
		  (setf pivotJ (+ pivotJ 1))
		  (return pivotJ))))))

(defun forward (P A b apivotI aj apivots)
  (let ((j aj)
	(pivots apivots))
    (loop for pivotI from apivotI to (iEnd A)
       do (if (not (inRange? A pivotI j))
	      (return pivots)
	      (let ( (pivotJ (nextPivotj P A pivotI j)) )
		(if (not pivotJ)
		    (return pivots)
		    (progn
		      (forwardRows P A b pivotI pivotJ)
		      (setf j (+ pivotJ 1))
		      (setf pivots (addPivots pivots pivotI pivotJ)))))))
    pivots))

(defun freeColIndices (A pivots)
  (filter (lambda (j) (not (in? j (mapcar (lambda (pv) (cdr pv)) pivots)))) (colIndices A)))
(defun freeRowIndices (A pivots)
  (filter (lambda (i) (not (in? i (mapcar (lambda (pv) (car pv)) pivots)))) (rowIndices A)))

(defun backwardOneLine (P A b i j x)
  (let ( (pci (P> P i))
	 )
    (setVal x j 0
	 (/
	  (- (at b pci 0)
	     (foldlUpto (lambda (init k) (+ init (* (at A pci k) (at x k 0)))) 0.0 (+ j 1) (jEnd A)))
	  (at A pci j)
	  ))))

(defun backward (P A b pivots x)
  (foreachList (reverse (sort (pivotsColIndices pivots) #'<))
       (lambda (j) (backwardOneLine P A b (pivotsJ2I pivots j) j x))))

(defun solvable? (P A b pivots)
  (every  (lambda (i)  (isZero? (atP P b i 0 ))) (freeRowIndices A pivots)))

(defun gauss (A b)
  (let ((P (newP A))
	(DU (copy A))
	(DUb (copy b)))
    (let ( (pivots (forward P DU DUb (iStart A) (jStart A) (newPivots)))
	   (x (transpose (averageRow A)))
	   (xs '()) )
      (if (not (solvable? P DU DUb pivots))
	  (list NIL P DU DUb pivots x xs)
	  (progn
	    (matFill x 0)
	    (backward  P DU DUb pivots x)
	    (foreachList (freeColIndices A pivots)
			 (lambda (j)
			   (let ((nx (copy x)) 
				 (nb (copy b)) )
			     (matFill nx 0)
			     (matFill nb 0)
			     (setVal nx j 0 1)
			     (backward P DU nb pivots nx)
			     (setq xs (cons nx xs))
			     )))
	    (list T P DU DUb pivots x xs))))))

(defun resultSolvable (result) (nth 0 result))
(defun resultP (result) (nth 1 result))
(defun resultDU (result) (nth 2 result))
(defun resultDUB (result) (nth 3 result))
(defun resultPivots (result) (nth 4 result))
(defun resultX (result) (nth 5 result))
(defun resultXs (result) (nth 6 result))

;;
;; rank of matrix
;;
(defun rank (A)
  (let* ( (b (newMatrix (iStart A) (iEnd A) 0 0)) 
	  (result (gauss A b)) )
    (length (resultPivots result))))

;;
;; least square method by gaussian elimination
;;
(defun leastSquare (A b)
  (gauss (multi (transpose A) A) (multi (transpose A) b)))

;;
;; power method
;;
(defun powerMethod (A itrMax)
  (if (not (equal (colIndices A) (rowIndices A))) (abort "power method : col idx and row idx are different."))

  (let ( (x (transpose (averageRow A)))
	 (prex (transpose (averageRow A))) )
    (fp A 0 itrMax x prex)))

(defun  fp (aA aItr itrMax ax aprex) 
  (copyTo ax aprex)
  (multi! aA aprex ax)
  (let ( (eigenValue (/ (at ax (iStart ax) 0) (at aprex (iStart aprex) 0))) )
    (normalizeColumn! ax 0)
    (if (or (>= aItr itrMax) (isZero? (matError ax aprex)))
	(progn
	  (format T "itr: ~d, eigenValue: ~f, error: ~f~%" aItr (+ eigenValue 0.0)(+ (matError ax aprex) 0.0))
	  (stdout ax)
	  )
	(fp aA (+ aItr 1) itrMax ax aprex))))

;;
;; samples
;;
(defparameter A20 (newMatrixFromList- -1 1 3 5 '(
2 1 1
4 1 0
-2 2 1
)))
(defparameter b20 (newMatrixFromList- -1 1 0 0 '(1 -2 7)))

(defparameter  A58 (newMatrixFromList- -1 1 -2 1 '(
1 3 3 2
 2 6 9 5
 -1 -3 3 0
)))
(defparameter b58 (newMatrixFromList- -1 1 0 0 '(1 2 -1)))

(defparameter A132 (newMatrixFromList- -2 1 3 4 '(
					    1 0
					      1 1
					      1 3
					      1 4)))
(defparameter b132 (newMatrixFromList- -2 1 0 0 '(0 1 2 5)))

(defParameter P2 (newMatrixFromList- -4 1 -4 1 '(
 87  270 -12  -49 -276  40
-14 -45   6  10   46  -4
-50 -156  4  25  162 -25
94   294 -5  -47 -306 49
  1    1  3   1   0   2
 16   48  1  -6  -48  8
)))




;;
;; test
;;
(defconstant tolerantSize 100)

(defconstant randIntMax 10000)
(defconstant randRealMax 1000.0)

(defun isSufficientZero? (x) (< (abs (- x 0)) 1e-5))

(defun matErrorXs (a xs)
  (if (null xs)
      0.0
      (+ (norm1OfColumn (multi a (car xs)) 0) (matErrorXs a (cdr xs)))))

(defun matError (mat1 mat2)
  (apply #'+ (forMap (indices mat1)
		     (lambda (id) (abs (- (at@ mat1 id) (at@ mat2 id)))))))

(defun randSign ()
    (let ((r (mod (random 2) 2)) )
      (if (= r 0)
	  -1
	  1)))
(defun randomInt ()
  (* (randSign) (random randIntMax)))
(defun randomReal ()
  (* (randSign) (* (random 1.0) randRealMax)))

(defun randListN (n)
  (if (eq n 0)
      '()
      (cons (randomInt) (randListN (- n 1)))))
(defun randListR (n)
  (if (= n 0)
      '()
      (cons (randomReal) (randListR (- n 1)))))
(defun randomMatrix (iS iE jS jE)
  (let ((mat (newMatrix iS iE jS jE)))
    (foreachList (indices mat) (lambda (id) (setVal@ mat id (randomReal))))
    mat))

(defun check_gauss ()
  (let ((m (+ 1 (mod (randomInt) tolerantSize)))
	(n (+ 1 (mod (randomInt) tolerantSize))))
    (check_gauss_mn m n)))

(defun check_gauss_mn (m n)
  (let* ((iS (randomInt))
	 (jS (randomInt))
	 (A (randomMatrix iS (- (+ iS m) 1) jS (- (+ jS n) 1)))
	 (realx (randomMatrix jS (- (+ jS n) 1) 0 0))
	 (b (multi a realx))
	 (result (gauss a b))
	 (flgSolvable (resultSolvable result))
	 (x  (resultX result))
	 (xs (resultXs result))
	 (ax (multi a x))
	 (diffB (/ (matError b ax) (* m n)))
	 (diffXs (matErrorXs a xs))
	 (freeDim (length xs))
	 )
    (format T "solvable:~a, m:~d, n:~d, freeDim:~d, diffB:~f, diffXs:~f ~%"  flgSolvable m n freeDim diffB diffXs)
    (if (and flgSolvable (isSufficientZero? (+ diffB diffXs)))
	T
	(progn
	  (stdoutSystem a b result)
	  (format T "solvable:~a, m:~d, n:~d, freeDim:~d, diffB:~f, diffXs:~f ~%"  flgSolvable m n freeDim diffB diffXs)
	  NIL))))

(defun stdoutSystem (a b result)
  (print "a")
  (stdout a)
  (print "b")
  (stdout b)
  (print "u")
  (stdout (resultDU result))
  (print "ub")
  (stdout (resultDUb result))
  (print "solvable : " (resultSolvable result))
  (print "p : ")
  (stdout (resultP result))
  (format T "pivots : ~a~%" (resultPivots result))
  (print "x, xs")
  (stdout (resultX result))
  (mapcar (lambda (v) (stdout v)) (resultXs result)))

(defun itr_gauss (n)
  (if (> n 0)
      (if (check_gauss)
	  (itr_gauss (- n 1)))))
