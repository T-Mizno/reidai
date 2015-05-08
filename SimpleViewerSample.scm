;;
;; Demo : to use gl.simple.viewer
;;  2011/01/15
;;

(use gl)
(use gl.glut)
(use gl.simple.viewer)

;(use util.list)
(use srfi-42) ; do-ec

;unit x
(define *u_x_max* 5.5)
(define *u_x_min* -5.5)

;unit y
(define *u_y_max* 5)
(define *u_y_min* -5)

;unit z
(define *u_z_max* 5)
(define *u_z_min* -5)

;real x
(define *r_x_max* 10.5)
(define *r_x_min* -10.5)

;real y
(define *r_y_max* 10.5)
(define *r_y_min* -10.5)

;real z
(define *r_z_max* 10.5)
(define *r_z_min* -10.5)

(define (r2u r r_max r_min u_max u_min)
  (+ (* (/ (- r r_min) (- r_max r_min))  (- u_max u_min) ) u_min)
)

(define (my-gl-vertex x y z)
  (gl-vertex
   (r2u x *r_x_max* *r_x_min* *u_x_max* *u_x_min*)
   (r2u y *r_y_max* *r_y_min* *u_y_max* *u_y_min*)
   (r2u z *r_z_max* *r_z_min* *u_z_max* *u_z_min*)
   )
)

(define (iter f ls)
  (cond
    ((null? ls)) ; do nothing
    ((pair? ls)
     (f (car ls))
     (iter f (cdr ls)))
    (else (error "Cannot iterate over a non-list"))))

(define (myDrawString x y z s)
  (gl-raster-pos
   (r2u x *r_x_max* *r_x_min* *u_x_max* *u_x_min*)
   (r2u y *r_y_max* *r_y_min* *u_y_max* *u_y_min*)
   (r2u z *r_z_max* *r_z_min* *u_z_max* *u_z_min*))
  (iter (lambda (c) (glut-bitmap-character GLUT_BITMAP_8_BY_13 (char->integer c))) (string->list s))
  )

(define (myGrid)
  (gl-color 0.5 0.5 0.5 0.0)
  (gl-line-width 1.0)
  (gl-begin* GL_LINES
	     (begin
	       (my-gl-vertex (min 0 *r_x_min*) 0 0)
	       (my-gl-vertex *r_x_max* 0 0)
	       )
	     )
  (gl-begin* GL_LINES
	     (begin
	       (my-gl-vertex 0 (min 0 *r_y_min*) 0)
	       (my-gl-vertex 0 *r_y_max* 0)
	       )
	     )
  (gl-begin* GL_LINES
	     (begin
	       (my-gl-vertex 0 0 (min 0 *r_z_min*))
	       (my-gl-vertex 0 0 *r_z_max*)
	       )
	     )
  (myDrawString 0 0 0 "Test")
)

(define (myTest)
  (gl-color 0.5 0.5 0.5 0.0)
  (gl-line-width 1.0)
  ( (lambda (dotNum)
      (gl-begin* GL_LINE_STRIP
		 (do-ec (: i 0 (+ dotNum 1))
			(begin
			  (let* (( _x (+ (* (/ (- *r_x_max* *r_x_min*) dotNum) i) *r_x_min*))
				 (_y (* (cos (* 6.28 (/ _x (- *r_x_max* *r_x_min*)))) 10))
				 )
			    (my-gl-vertex _x _y 0)
			    )
			  )))) 100
			       )
)

(define (main args)
  (glut-init args)
;  (simple-viewer-display (lambda () (glut-wire-sphere 2.0 10 8)))
  (simple-viewer-display myTest)
  (simple-viewer-grid myGrid)
  (simple-viewer-window 'demo)
  (simple-viewer-run)
  0)
