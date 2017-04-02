;;; Gaussian Quadrature, quad-double precision / OCT
;;; See quad-ga.lisp for basic ideas and comments
;;; quad-oqd.lisp for older versions.
;;; Jan 10, 2007.
;;; 10/29/2007  
;;;  by Richard Fateman

;; Legendre_pd returns legendre_p(k,x) and its derivative.
;; Note that d/dx P[n](x) =  (1/(x^2-1))*n*(x*P[n](x)-P[n-1](x)) except for x=+-1
;; This program is used with Newton iteration to refine roots of P[n](x).

(in-package :oct)

(eval-when (compile load)
  (declaim (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))
  (load "octi") 
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
	     (setf ans     (cl:/ (cl:- (cl:* (cl:1- (* 2.0d0 i)) x t1) (cl:* (cl:1- i) t0)) i)
		   t0 t1
		   t1 ans))
       (values        t1
		      ;; (1/(x^2-1))*k*(x*P[k](x)-P[k-1](x))
		      ;; except if abs(x)=1 then use
		      (if (= 1 (abs x))
			  ;; 1/2*k*(k+1)*x^(k+1)
			  (/ (* k (1+ k) (if (oddp k) 1 x)) 2)
			(cl:/ (cl:* k (cl:- (cl:* x t1)t0))  (cl:1-  (cl:* x x)))))))))

;;octfloat version
(defun legendre_pdbf (k x)
  (assert (and (typep k 'fixnum)(<= 0 k)))
  (case k 
    (0 (values (into 1)(into 0)))
    (1 (values (into x) (into 1)))
    (otherwise
     (let ((t0 (into 1))
	   (t1 (into x))
	   (ans (into 0))
	   (i (into 1)))
       (declare (fixnum k))
       (loop for ii from 2 to k  do
	     (dsetv i  (+ 1d0 i))
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

;; Here is a Newton iteration to converge to a root of legendre_p, the float version.
(defun improve_lg_root(n guess)
  (multiple-value-bind (val deriv)(legendre_pd n guess)
    (if (= deriv 0) (if (= val 0) 
			val 
		      (error "newton iteration failed at ~s"guess))
      ;;new_guess= guess-f(x)/f'(x)
      (- guess  (/ val deriv)))))

;; Here is a Newton iteration to converge to a root of legendre_p, bigfloat version.
(defun improve_lg_rootbf(n guess)
	   (multiple-value-bind (val deriv)(legendre_pdbf n guess)
	   ;;new_guess= guess-f(x)/f'(x)
	   ;; dsetv alters guess value in place
	   (dsetv guess  (- guess (/ val deriv)))) )


;; where x= a zero of P[k], m_wt computes the corresponding Legendre weight.
;; Lots of different forms for this; perhaps this is more stable than many others.

(defun m_wt (k x &aux (kf (into k)))		; compute -2 / (  k+1) * P'[k](x)*P[k+1](x))
  (assert (and (typep k 'fixnum)(< 1 k))) ; k>=2
  (setf x (into x)) 
	(let 
	    ((t0 (into 1))
	     (t1 x)
	     (ans 0)
	     (i (into 0))
	     (diffpk nil)(pkp1 nil))
	  (declare (fixnum k))
	  (loop for ii from 2 to k  do
		(setf i (into ii))
		(setf ans   
		  ;; make a new number here
		    (/  (with-temps(- (* (1- (* 2 i)) x t1) (* (1- i) t0))) i)
		      t0 t1
		      t1 ans))
	  ;; t0  is now legendre_p[k-1](x)
	  ;; ans is now legendre_p[k](x).
	  ;;deriv of p[k]  is k/(1-x^2) * p[k-1](x)-x*p[k](x)
	  (setf diffpk (with-temps (/ (*  kf (- t0 (* x t1))) (- 1 (* x x)))))
	  ;; compute  p[k+1](x)
	  (setf kf(+ kf 1))
	;;  (format t "~%kf=~s" kf)
	  ;; (setf pkp1  (/ (- (* (- (* 2 kf) 1) x t1) (* (- kf 1) t0)) kf)); 
	   (setf pkp1 (with-temps (/ (- (* (1- (* 2 kf)) x t1) (* (1- kf) t0)) kf)) )
;;	  (format t "~% diffpkm1=~s  pk+1=~s kf=~s" diffpkm1 pkp1 kf)
	 ;; (list diffpkm1 pkp1 kf (/ -2 (*(* kf diffpk) pkp1)))
	  (/ -2 (* kf diffpk pkp1))))

#|
:pa :oct
;; running the integration formula repeatedly is very cheap after
;; abscissae and weights are computed.  This  version computes
;; these items once as needed, then remembers them. Timing is much
;; faster after the first run.
(time (int_gl #'exp 20))
should give almost the same answer as
(defun right ()(let ((k (exp (into 1)))) (- k (/ 1 k)))) ; (right) is answer to above integral
|#

;; version 4
;;  to save allocation time and space we 
;;   0. initialize s to 0. For i= 0 to n/2 do lines 1-5:  ;; assume n is even.
;;   1. compute one abscissa x=x[i]
;;   2. compute one weight   w=w[i]
;;   3. compute two values    v=f(x)+f(-x))
;;   4. set s:= s+w*v.
;;   5. reusing x,w,v storage, repeat until n.
;; depending on whether n was even or odd, there may be one more point, at f(0)

;; Another improvement.  For the middle weight of an odd
;; number of terms, realize that the sum of all the weights will be
;; 2. So if we compute S, the sum weights up to n/2, the remaining
;; weight will be 2*(1 - S).

;; Also we can perhaps skip the full weight computation if n is even,
;; since one can be deduced by subtraction.

;; here's where we store precomputed wts and abscissae
(defparameter *legendabs* (make-hash-table))
(defparameter *legendwts* (make-hash-table))

;; integrate using gauss-legendre from -1 to 1
(defun int_gl(fun n)		; memoize the abscissae and weights
  (let ((abscissae (gethash n *legendabs*))
	(weights nil))

    (cond (abscissae (setf weights (gethash n *legendwts* )))
	  (t;; not found, so compute and remember them now.
		   (multiple-value-setq (abscissae weights)(ab_and_wts n))
	   (setf (gethash n *legendabs*) abscissae)
	   (setf (gethash n *legendwts*) weights)))
    ;; compute the sum.
    (let ((sum (into 0))	 
	  (halfn (ash n -1)))
      (loop for i from 0 to (1- halfn) do 
	    (dsetv sum 
		   (+ sum (*(+ (funcall fun (aref abscissae i))
			       (funcall fun (with-temps (-(aref abscissae i)))))
			    (aref weights i))))	 )
      (if (oddp n) 
	  (dsetv sum (+ sum (* (aref weights halfn)(funcall fun (into 0))))) )
      sum)))

;; pre-compute abscissae and weights
(defun ab_and_wts (n) ;precision is somewhat less than 215, qd. Stores only half the values (maybe +1)
  (let ((a 0)
	(v 0.0d0)
	(nph (/ 1.0d0(+ n 0.5d0)))
	(halfn (ash n -1))
	(abscissae (make-array (ceiling n 2)))
	(weights  (make-array (ceiling n 2))))
    (loop for i from 0 to (1- halfn) do
	  (setf v (cos(* pi (- (cl:1+ i) 0.25) nph)))
	  ;;	  (format t "~% compute cos(~s )=~s" (* pi (- (cl:1+ i) 0.25) nph) v)
	  (loop for i from 1 to 4  do	; 3 is almost enough.
		(multiple-value-bind (val deriv)(legendre_pd n v)
		  (declare (double-float val deriv))
		   ;;new_guess= guess-f(x)/f'(x)
		  (setf v (cl::- v (cl::/ val deriv)))))
	  ;; 	  (format t "~% --- improved v=~s" v)
	 (setf  a (into v))
        ;; compute more accurate abscissa.
	  (dotimes (k 4)
	  (multiple-value-bind (val deriv)(legendre_pdbf n a)
	    ;;new_guess= guess-f(x)/f'(x)
	    ;;  (format t "~% --- improved a=~s" a)
	    ;; dsetv alters a in place
	     (dsetv a  (- a (/ val deriv))) ))
	  
	  (setf (aref abscissae i) a)
	  (setf (aref weights i) (m_wt n a)))
    (cond ((oddp n)
	   (setf (aref abscissae  halfn) (into 0))
	   (setf (aref weights  halfn) 
      ;; fill in the middle element of an odd list by subtracting the others off
	     (let ((sum (into 0)))
		 (loop for i from 0 to (1- halfn)  do (dsetv sum (+ sum (aref weights i ))))
		 (dsetv sum (- 2 (+ sum sum)))))	   ))
    (values abscissae weights)))

(defun clear-leg-hash()			; call to deallocate storage from hash tables for legendre polys
  (clrhash *legendabs*)
  (clrhash *legendwts*))




