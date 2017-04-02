;;; Gaussian Quadrature, quad-double precision
;;; See quad-ga.lisp for basic ideas and comments
;;; quad-oqd.lisp for older versions.
;;; Jan 10, 2007.

;;;  by Richard Fateman

;; Legendre_pd returns legendre_p(k,x) and its derivative.
;;Note that d/dx P[n](x) =  (1/(x^2-1))*n*(x*P[n](x)-P[n-1](x)) except for x=+-1
;; This program is used with Newton iteration to refine roots of P[n](x).

(in-package :qd)

(eval-when (compile load)
  (declaim (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))
  (load "qd.dll") 
  )

(defun legendre_pd (k x)
  (declare(double-float x) (fixnum k) (optimize (speed 3)(safety 0)))
  (assert (and (typep k 'fixnum)(<= 0 k)))
  (case k 
    (0 (values 1.0d0 0.0d0))
    (1 (values x 1.0d0))
    (otherwise
     (let ((t0 1.0d0)
	   (t1 x)
	   (ans 0.0d0))
       (declare(double-float t0 t1 ans x))
       (declare (fixnum k))
       (loop for i from 2 to k  do
	     (setf ans     (cl::/ (cl::- (cl::* (cl::1- (* 2.0d0 i)) x t1) (cl::* (cl::1- i) t0)) i)
		   t0 t1
		   t1 ans))
       (values        t1
		      ;; (1/(x^2-1))*k*(x*P[k](x)-P[k-1](x))
		      ;; except if abs(x)=1 then use
		      (if (= 1 (abs x))
			  ;; 1/2*k*(k+1)*x^(k+1)
			  (/ (* k (1+ k) (if (oddp k) 1 x)) 2)
			(cl::/ (cl::* k (cl::- (cl::* x t1)t0))  (cl::1-  (cl::* x x)))))))))

;;qdfloat version
(defun legendre_pdbf (k x)
  (assert (and (typep k 'fixnum)(<= 0 k)))
  (case k 
    (0 (values 1 0))
    (1 (values (into x) 1))
    (otherwise
     (let ((t0 (into 1))
	   (t1 (into x))
	   (ans (into 0))
	   (i (into 1)))
       (declare (fixnum k))
       (loop for ii from 2 to k  do
	     (dsetv i  (+ i 1))
	     (setf ans  (with-temps(/(- (* (- (* 2 i)1) x t1) (* (- i 1) t0)) i)))
	     (dsetv t0 t1)
	     (dsetv t1 ans))
       (values        t1
		      ;; (1/(x^2-1))*k*(x*P[k](x)-P[k-1](x))
		      ;; except if abs(x)=1 then use
		      (if (= 1 (abs x))
			  ;; 1/2*k*(k+1)*x^(k+1)
			   (/ (* k (1+ k) (if (oddp k) 1 x)) 2)
			(with-temps (/ (* (into k) (- (* x t1)t0))  (1-  (* x x))))
			))))))


;; the float version
(defun improve_lg_root(n guess)
  (multiple-value-bind (val deriv)(legendre_pd n guess)
    (if (= deriv 0) (if (= val 0) 
			val 
		      (error "newton iteration failed at ~s"guess))
      ;;new_guess= guess-f(x)/f'(x)
      (- guess  (/ val deriv)))))

;; Here is a Newton iteration to converge to a root of legendre_p, bigfloat.
(defun improve_lg_rootbf(n guess)
	   (multiple-value-bind (val deriv)(legendre_pdbf n guess)
	   ;;new_guess= guess-f(x)/f'(x)
	   ;; dsetv alters array in place
	   (dsetv guess  (- guess (/ val deriv)))) )

;;The weights are w[j]:=  -2/ ( (n+1)* P'[n](x[j])*P[n+1](x))
;; this program computes one of them 

(defun legendre_pdwt (k x &aux (kf (into k)))		; compute -2 / (  k * P'[k-1](x)*P[k](x))
  (assert (and (typep k 'fixnum)(< 1 k))) ; k>=2
  (setf x (into x))
  (/ -2 
     (* kf
	(let 
	    ((t0 (into 1))
	     (t1 x)
	     (ans 0)
	     (i (into 0)))
	  (declare (fixnum k))
	  (loop for ii from 2 to (1- k)  do
		(setf i (into ii))
		(setf ans   
		  ;; make a new number here
		    (/ (with-temps (- (* (1- (* 2 i)) x t1) (* (1- i) t0))) i)
		      t0 t1
		      t1 ans))
	  (*				;deriv of p[k-1]
	    (if (= 1 (abs x)) ;not going to happen..
		 (/ (* kf (1- kf) (if (evenp k) 1 x)) 2)
	      (with-temps(/ (* (- kf 1) (- (* x t1)t0)) (- (* x x) 1))))
	   ;; legendre_p[k](x)
	    (with-temps (/ (- (* (- (* 2 kf) 1) x t1) (* (- kf 1) t0)) kf)))))     ))


#|
:pa :qd
(int_gs_l4bf #'exp 40)
;; the next 2 lines are about as expensive as the previous one
(multiple-value-setq (*abs* *vals*) (ab_and_wts 40)) ;; compute constants
(int_gs_l3bf #'exp *abs* *vals*) ;; run the integration formula
;; but running the integration formula repeatedly is very cheap, after weights etc computed.
;; compare to correct answer 

(int_gs_l5bf #'exp 40) ; combines both ideas: remembers the weights, once they are computed.

|#

(defun right ()(let ((k (exp (into 1)))) (- k (/ 1 k))))

;; Yet another approach, assuming you can pre-compute arrays of
;; abscissae and weights. Then you can compute the function values and
;; add everything together.

(defun int_gs_l3bf(fun abscissae weights)  ;; bigfloats
  ;; compute the sum.
  (let ((sum (into 0))
	(n (1-(length abscissae))))
    
    (loop for i from 0 to n do (setf sum 
				 (+ sum(*(funcall fun (aref abscissae i))
					 (aref weights i)))))
    sum))




#| ;; version 4
;;  to save allocation time and space we 
;;   0. initialize s to 0. For i= 0 to n/2 do lines 1-5:  ;; assume n is even.
;;   1. compute one abscissa x=x[i]
;;   2. compute one weight   w=w[i]
;;   3. compute two values    v=f(x)+f(-x))
;;   4. set s:= s+w*v.
;;   5. reusing x,w,v storage, repeat until n.
;; depending on whether n was even or odd, there may be one more point, at f(0)
|#

(defun int_gs_l4bf(fun n) 
  (let* ((np1 (+ n 1))
	 (v nil)
	 (fv #.(qd::into 0))
	 (sum (into 0))
	 (nph (/ 1.0d0(+ n 0.5d0)) )
	 (halfn (ash n -1)))
    ;; we step from point 1 to point n, summing along the way
    (loop for j from 1 to halfn do
	  (setf v (cos(* pi (- j 0.25d0) nph))) ;initial approx.
	  (loop for k from 1 to 4 do ;; 4 iterations in double-float, from init
		(setf v (improve_lg_root n v )))
	  (setf fv (into v))
	  ;; 4 iterations in qd
	 (loop for j from 1 to 4 do
		(dsetv fv (improve_lg_rootbf n fv)))
	  ;; now that v is accurate enough put it into weighted sum

	  (dsetv sum (+ sum (* (legendre_pdwt np1 fv) 
					  (+ (funcall fun fv)
					     (funcall fun (- fv))))))
	  )
	  (if (oddp n)
	      (+ sum (* (funcall fun 0)(legendre_pdwt np1 0)))
	    sum)))

;; yet other posible improvements.  Only compute half the data and
;; generate the symmetric parts when needed.  Also, for the middle
;; weight of an odd number of terms, realize that the sum of all the
;; weights will be 2. So if we compute S, the sum weights up to n/2,
;; the remaining weight will be 2*(1 - S).

;; Also we can perhaps skip the full weight computation n is even,
;; since one can be deduced by subtraction.


;; look for precomputed guys, else compute new ones
(defparameter *legendabs* (make-hash-table))
(defparameter *legendwts* (make-hash-table))


;; double-up on weights and abscissae

(defun int_gs_l5bf(fun n)		; memoize the abscissae and weights
  
  (let ((abscissae (gethash n *legendabs*))
	(weights nil))
    (cond (abscissae (setf weights (gethash n *legendwts* )))
	  (t;; compute them now.
	   (multiple-value-setq (abscissae weights)(ab_and_wts n))
	   (setf (gethash n *legendabs*) abscissae)
	   (setf (gethash n *legendwts*) weights)))
    ;; compute the sum.
    (let ((sum (into 0))	 
	  (halfn (ash n -1)))
      (loop for i from 0 to (1- halfn) do (dsetv sum 
					    (+ sum (*(+ (funcall fun (aref abscissae i))
							(funcall fun (with-temps (-(aref abscissae i)))))
						     (aref weights i)))))
      (if (oddp n)
	   (dsetv sum (+ sum (* (aref weights halfn)(funcall fun (into 0))))))
      sum)))


;; pre-compute abscissae and weights
(defun ab_and_wts (n );precision is somewhat less than 215, qd. Stores only half the values (maybe +1)
  (let ((a 0)
	(v 0.0d0)
	(np1 (+ n 1))
	(nph (/ 1.0d0(+ n 0.5d0)))
	(halfn (ash n -1))
	(myprec 38)
	(abscissae (make-array (ceiling n 2)))
	(weights  (make-array (ceiling n 2))))
    (loop for i from 0 to (1- halfn) do
	  (setf v (cos(* pi (- (cl::1+ i) 0.25) nph)))
	  (loop for i from 1 to 4  do	; 3 is almost enough.
		(multiple-value-bind (val deriv)(legendre_pd n v)
		  (declare (double-float val deriv))
		   ;;new_guess= guess-f(x)/f'(x)
		   (setf v (cl::- v (cl::/ val deriv)))))
	 (setf  a (into v))
        ;; compute more accurate abscissa.
	  ;;  we build up gradually to goal precision.
	  (setf myprec 38)
	  ;;takes a while
	  (loop while (< myprec 208) do
	      (setf myprec (* 2 myprec) )
	  (multiple-value-bind (val deriv)(legendre_pdbf n a)
	   ;;new_guess= guess-f(x)/f'(x)
	   ;; dsetv alters a in place
	    (dsetv a  (- a (/ val deriv)))   ))
	  (setf (aref abscissae i) a)
	  (setf (aref weights i) (legendre_pdwt np1 a))	)
    (cond ((oddp n)
	   (setf (aref abscissae  halfn) (into 0))
	   (setf (aref weights  halfn) 
	     (legendre_pdwt np1 (aref abscissae  halfn)))))
    (values abscissae weights)))


(defun clear-leg-hash()			; call to deallocate storage from hash tables for legendre polys
  (clrhash *legendabs*)
  (clrhash *legendwts*))


