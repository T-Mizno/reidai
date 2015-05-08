;;
;; Gaussの消去法
;;   水野 (of.mizno@gmail.com)
;;   2013/10/15
;;   2013/10/23
;;       検証用の関数を追加
;;   2013/11/12
;;       モジュール名を myzLinear に変更。
;;

;; usage :
;; (require "./myzLinear.scm")
;; (iimport myzLinear)


(define-module myzLinear
  (export-all))
(select-module myzLinear)  

(use srfi-1) ;; iota
(use srfi-27) ;; for random
(use gauche.array)

(define epsillon 0.00001)
(define (near? x y) (< (abs (- x y)) epsillon))
(define (isZero? x) (near? x 0.0))

(define (upto start end) 
  (if (> start end)
      '()
      (iota (+ (- end start) 1) start)))

(define (downto start end) 
  (if (< start end)
      '()
      (reverse (upto end start))))

(define (directProductList xs ys)
  (concatenate (map (lambda (x) (map (lambda (y) (list x y)) ys)) xs)))

(define (for list proc) (for-each proc list))
(define (forMap f list) (map list f))

(define (sqr x) (* x x))

; ここから gauche.array 依存

(define (newMatrix iStart iEnd jStart jEnd) (make-array (shape iStart (+ iEnd 1) jStart (+ jEnd 1)) 0))

(define (M matrix) (array-length matrix 0)) ; 0 corresponds to Row
(define (N matrix) (array-length matrix 1)) ; 1 corresponds to Column

(define (iStart matrix) (array-start matrix 0)) ; 0 corresponds to Row
(define (iEnd matrix) (- (array-end matrix 0) 1))
(define (jStart matrix) (array-start matrix 1)) ; 1 corresponds to Column
(define (jEnd matrix) (- (array-end matrix 1) 1))

(define (at matrix i j) (array-ref matrix i j))
(define (set matrix i j v) 
  (if (idInRange? matrix i j) (array-set! matrix i j v)))

; ここまで gauche.array 依存


(define (newMatrixFromList- iStart iEnd jStart jEnd xs)
  (let ( (matrix (newMatrix iStart iEnd jStart jEnd))  )
    (setList- matrix xs)
    matrix))

(define (newMatrixFromList@ iStart iEnd jStart jEnd xs)
  (let ( (matrix (newMatrix iStart iEnd jStart jEnd))  )
    (setList@ matrix xs)
    matrix))

(define (stdout matrix)
  (format #t "   ")
  (for (colIds matrix) (lambda (j) (format #t "   ~D" j)))
  (format #t "\n")
  (matFor matrix
	  (lambda (aMat i j)
	    (if (eq? j (jStart aMat))(format #t "~D: " i))
	    (format #t "  ~D" (at aMat i j))
	    (if(eq? j (jEnd aMat)) (format #t "\n"))
      )))

(define (at@ matrix id) (at matrix (id-i id) (id-j id)))
(define (set@ matrix id v) (set matrix (id-i id) (id-j id) v))

(define (cell@ id v) (list id v))
(define (cell i j v) (cell@ (list i j) v))

(define (id2sid matrix i j)
  (+ (* (- i (iStart matrix)) (+ (- (jEnd matrix) (jStart matrix)) 1)) (- j (jStart matrix))))
(define (id2sid@ matrix id) (id2sid matrix (id-i id) (id-j id)))

(define (setList- matrix xs)
  (for (ids matrix) 
       (lambda (id) 
	 (let ((k (id2sid@ matrix id)))
	   (if (and (<= 0 k) (< k (length xs))) (set@ matrix id (list-ref xs k)))))))

(define (setList@ matrix idxs)
  (for idxs (lambda (idx)
	      (set@ matrix (id-i idx) (id-j idx)))))

(define (idInRange? matrix i j)
  (and (and (<= (iStart matrix) i) (<= i (iEnd matrix)))
       (and (<= (jStart matrix) j) (<= j (jEnd matrix)))))
(define (idInRange?@ matrix id) (idInRange? matrix (first id) (second id)))
(define (in? i is) (any (lambda (k) (= i k)) is))
(define (rowIds matrix) (upto (iStart matrix) (iEnd matrix)))
(define (colIds matrix) (upto (jStart matrix) (jEnd matrix)))
(define (ids matrix) (directProductList (rowIds matrix) (colIds matrix)))
(define (id-i id) (first id))
(define (id-j id) (second id))

(define (nthColumn mat j) 
  (let ((col (newMatrix (iStart mat) (iEnd mat) (jStart mat) (jStart mat))))
    (map (lambda (i) (set col i (jStart mat) (at mat i j))) (rowIds mat))
    col
    ))

(define (matMap@ f@ fromMat)
  (newMatrixFromList@ (iStart fromMat) (iEnd fromMat) (jStart fromMat) (jEnd fromMat)
		      (map (lambda (id) (cell@ id (f@ id (at@ fromMat id)))) (ids fromMat))))

(define (matFor@ mat proc@ )
  (for (ids mat) (lambda (id) (proc@ mat id ))))
(define (matFor mat proc)
  (for (ids mat) (lambda (id) (let ((i (id-i id)) (j (id-j id))) (proc mat i j )))))

(define (matFill mat v) (matFor@ mat (lambda (aMat id) (set@ aMat id v))))
(define (copyTo fromMat toMat)
  (matFor@ fromMat (lambda (aFromMat id) (set@ toMat id (at@ aFromMat id)))))
(define (copy fromMat)
  (let ((mat (newMatrix (iStart fromMat) (iEnd fromMat) (jStart fromMat) (jEnd fromMat))))
    (copyTo fromMat mat)
    mat))

(define (addScalar mat x) (matMap@ (lambda (id val) (+ x val)) mat))
(define (addScalar! mat x) (setList@ mat (forMap (ids mat)
						 (lambda (id) (cell@ id (+ (at@ mat id) x))))))
(define (subScalar mat x) (matMap@ (lambda (id val) (- x val)) mat))
(define (subScalar! mat x) (setList@ mat (forMap (ids mat)
						 (lambda (id) (cell@ id (- (at@ mat id) x))))))

(define (addMat mat1 mat2)
  (matMap@ (lambda (id val) (+ (at@ mat2 id) val)) mat1))
(define (subMat mat1 mat2)
  (matMap@ (lambda (id val) (- val (at@ mat2 id))) mat1))

(define (multiScalar x mat) (matMap@ (lambda (id val) (* x val)) mat))

(define (multi mat1 mat2)
  (if (not (equal? (colIds mat1) (rowIds mat2))) (error "Multiplication error : row Indices check."))
  (let ((rMat (newMatrix (iStart mat1) (iEnd mat1) (jStart mat2) (jEnd mat2))))
    (multi! mat1 mat2 rMat)
    rMat))
(define (multi! mat1 mat2 rMat)
  (if (not (equal? (colIds mat1) (rowIds mat2))) (error "Multiplication error : row Indices check."))
  (matFor rMat (lambda (aMat i j) (set aMat i j (apply + (map (lambda (k) (* (at mat1 i k) (at mat2 k j))) (colIds mat1)))))))

(define (norm1OfColumn mat j)
  (apply + (map (lambda (k) (abs (at mat k j))) (rowIds mat))))
(define (norm2OfColumn mat j)
  (sqrt (apply + (map (lambda (k) (sqr (at mat k j))) (rowIds mat)))))
(define (normalizeColumn! mat j)
  (let ( (deno (norm1OfColumn mat j)) )
    (for (rowIds mat) (lambda (i) (set mat i j (/ (at mat i j) deno))))))

(define (averageRow mat)
  (let ( (x (newMatrix (jStart mat) (jEnd mat) 0 0)) )
    (for (colIds mat)
	 (lambda (j)
	   (set x j 0
		(/
		 (apply + (map (lambda (k) (at mat k j)) (rowIds mat)))
		 (M mat)))))
    x))

(define (matError mat1 mat2)
  (apply + (map (lambda (id) (abs (- (at@ mat1 id) (at@ mat2 id)))) (ids mat1))))

(define (transpose mat)
  (let ( (rMat (newMatrix (jStart mat) (jEnd mat) (iStart mat) (iEnd mat))) )
    (matFor rMat (lambda (aMat i j) (set aMat i j (at mat j i))))
    rMat))


;; for Permutation
(define (newP mat) (newMatrixFromList- (iStart mat) (iEnd mat) 0 0 (rowIds mat)))
(define (swapP P i k)
  (let ((tmp (at P i 0)))
    (set P i 0 (at P k 0))
    (set P k 0 tmp)))
(define (Pi P i) (at P i 0))
(define (Pid P i j) (list (Pi P i) j))
(define (at*  P mat i j) (at@  mat (Pid P i j)))
(define (Pid@ P id) (Pid (id-i id) (id-j id)))
(define (at@* P mat id)  (at@ mat (Pid P (id-i id) (id-j id))))
(define (set*  P mat i j v) (set@  mat (Pid P i j) v))
(define (set@* P mat id v)  (set@  mat (Pid P (id-i) (id-j id)) v))

(define (forwardOneLine P A b pi pj i)
  (if (isZero? (at* P A pi pj)) (error "gauss forward one line : pivot is zero."))
  (let* ((pvi (Pi P pi))
	 (pci (Pi P i))
	 (pv (/ (at A pci pj) (at A pvi pj)))
	 )
;    (setList@ A (forMap (upto (+ pj 1) (jEnd A))
;			(lambda (j)
;			  (cell@ (list pci j)
;				 (- (at A pci j)
;				    (* pv (at A pvi j)))))))
    (let loop ( (j (+ pj 1)) )
      (when (<= j (jEnd A))
	    (set A pci j (- (at A pci j) (* pv (at A pvi j))))
	    (loop (+ j 1))))
    (set b pci 0
	  (- (at b pci 0)
	     (* pv (at b pvi 0))))
    (set A pci pj pv)))

(define (updateP P A pi pj)  (swapP P pi (searchMaxI P A pi pj)))
(define (maxi tmpiv ivs)
  (if (null? ivs) 
      (first tmpiv)
      (if (> (second (car ivs)) (second tmpiv))
	  (maxi (car ivs) (cdr ivs))
	  (maxi tmpiv (cdr ivs)))))
		
(define (searchMaxI P A fromi j)
  (let ( (ivs (map (lambda (i) (list i (abs (at* P A i j)))) (upto fromi (iEnd A)))) )
    (maxi (car ivs) (cdr ivs))))

(define (forwardRows P A b pi pj)
;  (for (upto (+ pi 1) (iEnd A))
;       (lambda (i) (forwardOneLine P A b pi pj i))))
  (let loop ( (i (+ pi 1)) )
    (when (<= i (iEnd A))
	  (forwardOneLine P A b pi pj i)
	  (loop (+ i 1)))))

(define (underColumnsAreZero? P A i j)
  (every isZero? (map (lambda (k) (at* P A k j)) (upto i (iEnd A)))))
(define (isValidPivot? P A pi pj)
  (stdout A)
  (if (not (idInRange? A pi pj ))
      #f
      (if (underColumnsAreZero? P A pi pj)
	  #f
	  #t))
)
(define (nextPivotj P A pi pj)
  (if (not (idInRange? A pi pj))
      #f
      (begin
	(updateP P A pi pj)
	(if (underColumnsAreZero? P A pi pj)
	    (nextPivotj P A pi (+ pj 1))
	    pj))))

(define (forward P A b pi j pivots)
  (if (not (idInRange? A pi j))
      pivots
      (let ( (pj (nextPivotj P A pi j)) )
	(if (not pj)
	    pivots
	    (begin
	      (forwardRows P A b pi pj)
	      (forward P A b (+ pi 1) (+ pj 1) (cons (list pi pj) pivots )))))))

(define (baseColIds pivots) (map (lambda (pv) (id-j pv)) pivots))
(define (baseRowids pivots) (map (lambda (pv) (id-i pv)) pivots))
(define (freeColIds A pivots)
  (filter (lambda (j) (not (in? j (map (lambda (pv) (id-j pv)) pivots)))) (colIds A)))
(define (freeRowIds A pivots)
  (filter (lambda (i) (not (in? i (map (lambda (pv) (id-i pv)) pivots)))) (rowIds A)))
(define (pivotj2i pivots j)
  (first (car (filter (lambda (pv) (= j (id-j pv))) pivots))))

(define (backwardOneLine P A b i j x)
  (let ( (pci (Pi P i))
	 )
    (set x j 0
	 (/
	  (- (at b pci 0)
	     (apply + (map (lambda (k) (* (at A pci k) (at x k 0))) (upto (+ j 1) (jEnd A)))))
	  (at A pci j)
	  ))))

(define (backward P A b pivots x)
  (for (reverse (sort (baseColIds pivots)))
       (lambda (j) (backwardOneLine P A b (pivotj2i pivots j) j x))))
  
(define (solvable? P A b pivots)
  (every  (lambda (i)  (isZero? (at* P b i 0 ))) (freeRowIds A pivots)))

(define (gauss A b)
  (let ((P (newP A))
	(DU (copy A))
	(DUb (copy b)))
    (let ( (pivots (forward P DU DUb (iStart A) (jStart A) '()))
	   (x (averageRow A))
	   (xs '()) )
      (if (not (solvable? P DU DUb pivots))
	  (newGaussAnswer #f P DU DUb pivots x xs)
	  (begin
	    (matFill x 0)
	    (backward  P DU DUb pivots x)
	    (for (freeColIds A pivots)
		 (lambda (j)
		   (let ( (nx (copy x)) 
			  (nb (copy b)) )
		     (matFill nx 0)
		     (matFill nb 0)
		     (set nx j 0 1)
		     (backward P DU nb pivots nx)
		     (set! xs (cons nx xs))
		     )))
	    (newGaussAnswer #t P DU DUb pivots x xs))))))

;; 
(define (newGaussAnswer aFlg aP aDU aDUb aPivots aX aXs)
  (list aFlg aP aDU aDUb aPivots aX aXs))
(define (gaussSolvable aGaussAns) (list-ref aGaussAns 0))
(define (gaussP aGaussAns) (list-ref aGaussAns 1))
(define (gaussDU aGaussAns) (list-ref aGaussAns 2))
(define (gaussDUb aGaussAns) (list-ref aGaussAns 3))
(define (gaussPivots aGaussAns) (list-ref aGaussAns 4))
(define (gaussX aGaussAns) (list-ref aGaussAns 5))
(define (gaussXs aGaussAns) (list-ref aGaussAns 6))
(define (gaussXs-ref aGaussAns i) (list-ref (gaussXs aGaussAns) i))
(define (gaussNullRank aGaussAns) (length (gaussXs aGaussAns)))
(define (gaussRank aGaussAns) (length (gaussPivots aGaussAns)))

;;
;; rank of matrix
;;
(define (rank A)
  (let* ( (b (newMatrix (iStart A) (iEnd A) 0 0)) 
	  (result (gauss A b)) )
    (gaussRank result)))
;    (length (list-ref result 4))))

;;
;; least square method by gaussian elimination
;;
(define (leastSquare A b)
  (gauss (multi (transpose A) A) (multi (transpose A) b)))

;;
;; power method
;;
(define (powerMethod A itrMax)
  (if (not (equal? (colIds A) (rowIds A))) (error "power method : col idx and row idx are different."))

  (let ( (x (averageRow A))
	 (prex (averageRow A)) )
    (fp A 0 itrMax x prex)))

(define  (fp aA aItr itrMax ax aprex) 
  (copyTo ax aprex)
  (multi! aA aprex ax)
  (let ( (eigenValue (/ (at ax (iStart ax) 0) (at aprex (iStart aprex) 0))) )
    (normalizeColumn! ax 0)
    (if (or (>= aItr itrMax) (isZero? (matError ax aprex)))
	(begin
	  (print aItr)
	  (print (+ eigenValue 0.0))
	  (print (+ (matError ax aprex) 0.0))
	  (matFor ax (lambda (mat i j) (set mat i j (+ (at mat i j) 0.0))))
	  (stdout ax)
	  (list (* eigenValue 1.0) ax)
	  )
	(fp aA (+ aItr 1) itrMax ax aprex))))




;; examples
(define A20 (newMatrixFromList- -1 1 3 5 '(
2 1 1
4 1 0
-2 2 1
)))
(define b20 (newMatrixFromList- -1 1 0 0 '(
1 -2 7
)))

(define AA (newMatrixFromList- 3 5 -10 -9 '(
3 4 
5 6
7 8)))

(define A58 (newMatrixFromList- -1 1 -2 1 '(
1 3 3 2
 2 6 9 5
 -1 -3 3 0)))

(define b58 (newMatrixFromList- -1 1 0 0 '(1 2 -1)))


(define A132 (newMatrixFromList- -2 1 3 4 '(
					    1 0
					      1 1
					      1 3
					      1 4)))

(define b132 (newMatrixFromList- -2 1 0 0 '(0 1 2 5)))

(define  matP2 (newMatrixFromList- -4 1 -4 1 '(
 87  270 -12  -49 -276  40
-14 -45   6  10   46  -4
-50 -156  4  25  162 -25
94   294 -5  -47 -306 49
  1    1  3   1   0   2
 16   48  1  -6  -48  8
)))


;; for test
(define tolerantSize 100)

(define randIntMax 10000)
(define randRealMax 10000.0)

(define (randSign)
  (let ((r (mod (random-integer 2) 2)) )
    (if (= r 0)
	-1
	1)))
(define (randRatio)
  (let ( (n (random-integer randIntMax))
	 (d (random-integer randIntMax)))
    (if (= d 0) 
	(/ n 1)
	(/ n d))))
(define (randListN n)
  (if (= n 0)
      '()
      (cons (* (randSign) (random-integer randIntMax)) (randListN (- n 1)))))
(define (randListR n)
  (if (= n 0)
      '()
      (cons (* (randSign) (* (random-real) randRealMax)) (randListR (- n 1)))))
(define (randListRatio n)
  (if (= n 0)
      '()
      (cons (* (randSign) (randRatio)) (randListRatio (- n 1)))))

(define (matErrorXs a xs)
  (if (null? xs)
      0
      (+ (norm2OfColumn (multi a (car xs)) 0) (matErrorXs a (cdr xs)))))

(define (isSufficientZero? x)
  (< (abs (- x 0)) 1e-5))

(define (check_gauss)
  (let* ( (mns (randListN 4))
	 (m (+ 1 (mod (abs (list-ref mns 0)) tolerantSize)))
	 (n (+ 1 (mod (abs (list-ref mns 1)) tolerantSize))))
    (check_gauss_mn m n)))

(define (check_gauss_mn m n)
  (let* ( (mns (randListN 4))
	  (iStart (list-ref mns 2))
	  (jStart (list-ref mns 3))
	  (vals (randListR (+ (* m n) n)))
;	  (vals (randListRatio (+ (* m n) n)))
	  (a (newMatrixFromList- iStart (- (+ iStart m) 1) jStart (- (+ jStart n) 1) vals))
	  (realx (newMatrixFromList-  jStart (- (+ jStart n) 1) 0 0 (drop vals (* m n))))
	  (b (multi a realx))
	  (result (gauss a b))
	  (flgSolvable (gaussSolvable result))
	  (x (gaussX result))
	  (xs (gaussXs result))
	  (ax (multi a x))
	  (diffB (/ (matError b ax) (* m n)))
	  (diffXs (matErrorXs a xs))
	  (freeDim (length xs))
	  )
    (print "(m, n, freeDim, diffB, diffXs) = " (list m n freeDim diffB diffXs))
    (if (and flgSolvable (isSufficientZero? (+ diffB diffXs)))
	#t
	(begin
	  (stdoutSystem a b result)
	  (print "(m, n, solvable, freeDim, diffB, diffXs) = " (list m n flgSolvable freeDim diffB diffXs))
	  #f))))

(define (itr_gauss n)
  (if (> n 0)
      (if (check_gauss)
	  (itr_gauss (- n 1)))))

(define (stdoutSystem a b result)
  (let ( (flgSolvable (list-ref result 0))
	 (p (list-ref result 1))
	 (u (list-ref result 2))
	 (ub (list-ref result 3))
	 (pivots (list-ref result 4))
	 (x  (list-ref result 5))
	 (xs (list-ref result 6))
	 )
    (print "a, b")
    (stdout a)
    (stdout b)
    (print "u, ub")
    (stdout u)
    (stdout ub)
    (print "solvable : " flgSolvable)
    (print "p : ")
    (stdout p)
    (print "pivots : " pivots)
    (print "x, xs")
    (stdout x)
    (map (lambda (v) (stdout v)) xs)
    ))

(provide "myzLinear")

;; end of myzLinear
