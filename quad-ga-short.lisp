(in-package :ga)
;;; Gaussian Quadrature, arbitrary precision,
;;; Same as quad-ga.lisp, but omitting parts that were written but no longer used 3/22/07.

;;; written in lisp by Richard Fateman, Jan 4, 2007
;;; inspired in part by QuadGS version 1.0 of June 26,2004, by Sherry Li
;;; refer to http://crd.lbl.gov/~xiaoye/quadrature.pdf

;;; Some comment lines  are taken verbatim from quad-gs.h or quad-gs.cpp
#|
   This Gaussian Quadrature scheme computes an integral from [-1,1] as
   the sum(w[j]*f(x[j]), j=0,n-1) where the values of the abscissas
   x[j] are roots of the nth degree Legendre polynomial P[n](x) on
   [-1,1], and the weights w[j] are -2/(n+1)/P'[n](x[j])/P[n+1](x[j]).

   To find the x[j], compute a starting value of cos(pi*(j-1/4)/(n+1/2)). 
   Then iterate until converged at full precision using Newton iteration.
   The iteration can be computed in modest precision, doubling at each step.

   The value of a Legendre polynomial P[n] can be computed recursively by
   P[0](x)=0, P[1](x)=1, P[k+1](x)= 1(k+1)* ((2k+1)*x*P[k](x)-k*P[k-1](x)).

   the derivative P'[n](x) = n (x*P[n](x)-P[n-1](x))/(x^2-1).

   This formula is poor if there is a singularity at an endpoint.
   |#

#| Compared to Sherry Li's code,  the work needed for  "ARPREC", 
involving storage stuff, is unnecessary.

In the part not yet written, we could go
through some number of levels of abscissa-weight pairs, doubling the
number of pairs at each level. After obtaining a result at the first
level, we apply additional levels until we are satisfied or else have to
go back and compute more abscissa-weight pairs.

We can also dynamically increase the working precision of the
evaluation of the integrand f and the abscissa-weights, 
saving some time at lower precision if we are not even close.

The description of NIntegrate in Mathematica says that their default
for GaussPoints is floor(workingprecision/3) {presumably in decimal}.
The default for PrecisionGoal is usually equal to WorkingPrecision-10, 
which is usually MachinePrecision, 15.95.
Mathematica has 7 alternative integration methods, not all of them adaptive.|#

;; This program legendre_p will compute the P[k](x) Legendre
;; polynomials. k is a fixnum. x is any kind of floating point number:
;; single, double, qd, MPFR.  It is not actually needed, but it
;; explains what is going on in the next program, legendre_pd.
#+ignore
(defun legendre_p (k x)
  ;; P[0](x)=0, P[1](x)=1,
  ;; P[k](x)= 1/k* ((2k-1)*x*P[k-1](x)-(k-1)*P[k-2](x)).
  ;; or
  ;;      (/ (- (* (1- (* 2 k)) x t1) (* (1- k) t0)) k)
  (assert (and (typep k 'fixnum)(<= 0 k)))
  (case k 
    (0 1)
    (1 x)
    (otherwise
     (let ((t0 1)
	   (t1 x)
	   (ans 0))
       (declare (fixnum k))
       (loop for i from 2 to k  do
	     (setf ans     (/ (- (* (1- (* 2 i)) x t1) (* (1- i) t0)) i)
		   t0 t1
		   t1 ans))
       t1))))

;; legendre_pd returns legendre_p(k,x) and its derivative.
;;Note that d/dx P[n](x) =  (1/(x^2-1))*n*(x*P[n](x)-P[n-1](x)) except for x=+-1
;; This program is used with Newton iteration to refine roots of P[n](x).

(defun legendre_pd (k x)
  (assert (and (typep k 'fixnum)(<= 0 k)))
  (case k 
    (0 (values 1 0))
    (1 (values x 1))
    (otherwise
     (let ((t0 1)
	   (t1 x)
	   (ans 0))
       (declare (fixnum k))
       (loop for i from 2 to k  do
	     (setf ans     (/ (- (* (1- (* 2 i)) x t1) (* (1- i) t0)) i)
		   t0 t1
		   t1 ans))
       (values        t1
		      ;; (1/(x^2-1))*k*(x*P[k](x)-P[k-1](x))
		      ;; except if abs(x)=1 then use
		      (if (= 1 (abs x))
			  ;; 1/2*k*(k+1)*x^(k+1)
			  (/ (* k (1+ k) (if (oddp k) 1 x)) 2)
			(/ (* k (- (* x t1)t0))  (1-  (* x x)))))))))



;; The weights are w[j]:=  -2/ ( (n+1)* P'[n](x[j])*P[n+1](x)), where x is a root of P[n].
;; This program computes the weights. 

(defun legendre_pdwt (k x)		; compute -2 / (  k * P'[k-1](x)*P[k](x))
  (assert (and (typep k 'fixnum)(< 1 k)))	; k>=2
  (/ -2 
     (* k
	(let 
	    ((t0 1)
	     (t1 x)
	     (ans 0) )
	  (declare (fixnum k))
	  (loop for i from 2 to (1- k)  do
		(setf ans     (/ (- (* (1- (* 2 i)) x t1) (* (1- i) t0)) i)
		      t0 t1
		      t1 ans))
	  (*				;deriv of p[k-1]
	   (let((k (1- k))) ;use formula above..
	     (if (= 1 (abs x))
		 (/ (* k (1+ k) (if (oddp k) 1 x)) 2)
	       (/ (* k (- (* x t1)t0)) (1- (* x x)))))
	   ;; legendre_p[k](x)
	   (/ (- (* (1- (* 2 k)) x t1) (* (1- k) t0)) k))))     ))


;; to make this really work for arbitrary precision: Assume user has
;; set a goal accuracy.  Guess some related goal precision.  Do a
;; couple of integrations and see if the answers are close enough: has
;; the accuracy criterion (probably) been met? If not, there are two
;; ways to improve. Increase (e.g. double) the number of points, or
;; increase the precision.

;; The driver may also have to transform some other integral limits,
;; including limits at infinity, to -1,1.

;;And a super-good driver using symbolic tools will look at the
;;   integrand and remove singularities, detect symmetries,
;;   oscillatory functions, etc.  And compile the function as needed.

;; RJF Jan 6, 2007.
;; RJF Mar 23, 2007


;; example... 
#|
:pa :ga
(mfpr::set-prec 200) ; bits of fraction
(int_gs_l1bf #'exp 10)			; 10 points
(int_gs_l1bf #'exp 20)			; 20 points
(int_gs_l1bf #'exp 40)

;; compare to correct answer 
(let ((k (exp (mpfr::into 1)))) (- k (/ 1 k)))

;; There are many ways to improve the efficiency, including using the
;; symmetry of the legendre polynomials to compute only half the roots
;; and weights, either not storing the abscissae and weights (but
;; computing on the fly), or computing them and saving them
;; "memoizing" for subsequent calls. These improvements are in the file
;; quad-fast.lisp and quad-qd.lisp
|#


#| going further, to find some kind of error estimate:
Let S[k] be the computed integral for level k. Compute k=3, 6, 12, ... m doublings, e.g. m=9

Estimated error E[r] is computed this way in the referenced work
d1=log(S[r]-S[r-1]) base 10
d2=log(S[r]-S[r-2]) base 10
d3=log(eps* {max abs value of f(x) for some abscissa x in level r)}) base 10
eps= "machine epsilon" of the multiprecision system
d= min(0, max (d1^2/d2, 2*d1, d3))

The error then is E[r]=  if  r<=2 then 1 else 0 if S[r]=S[r-1] else 10^d

|#


;;;; memoizing stuff...
;; here's where we store precomputed wts and abscissae
(defparameter *legendabs* (make-hash-table))
(defparameter *legendwts* (make-hash-table))
(defparameter *legendprec* (make-hash-table))

;; call (clear-leg-hash) to deallocate storage from hash tables for
;; legendre polys in case you think you need it for something else.

(defun clear-leg-hash()			
  (clrhash *legendabs*)
  (clrhash *legendwts*)
  (clrhash *legendprec*))

(defun gaussunit(fun n) ;; fun is a lisp numeric function taking an MPFR 
    ;;int_gs_l5bf(fun n)
  ;; memoize the abscissae and weights
  (let ((prec (or (gethash n *legendprec*) 0))
	;;Did we compute these weights previously?
	(weights nil)
	(abscissae nil))
    
    (cond ((cl::>= prec (mpfr::get-prec))	
	   ;; Is enough precision already computed for this number of terms?
	   (setf abscissae (gethash n *legendabs*));use previous version
	   (setf weights (gethash n *legendwts*)))
	  (t				
	   ;; not found or insufficient precision, so compute and remember them now.
	   (multiple-value-setq (abscissae weights)(ab_and_wts n))
	   (setf (gethash n *legendabs*) abscissae)
	   (setf (gethash n *legendwts*) weights)
	   (setf (gethash n *legendprec*) (mpfr::get-prec))))
    ;; compute the sum.
    (let ((sum (mpfr::into 0))	 
	  (halfn (ash n -1)))
      (loop for i from 0 to (1- halfn) do 
	    (setf sum 
		   (+ sum (*(+ (funcall fun (aref abscissae i))
			       (funcall fun (-(aref abscissae i))))
			    (aref weights i)))))
      (if (oddp n) (setf sum (+ sum (* (aref weights halfn)(funcall fun (mpfr::into 0))))))
      sum)))

;; Here we use a Newton iteration to converge to a root of
;; legendre_p[n](x) starting from a particular guess.
;; The answer's precision will depend on the generic arithmetic
;; in  (- guess (/ val deriv))

(defun ab_and_wts (n)
  ;;mpfr precision. compute abscissae and weights.
  (let ((a 0)
	(v 0.0d0)
	(np1 (+ n 1))
	(nph (/ 1.0d0(+ n 0.5d0)))
	(halfn (ash n -1))
	(abscissae  (make-array (ceiling n 2)))
	(weights   (make-array (ceiling n 2))))
    (declare (double-float v nph))
    (loop for i from 0 to (1-    halfn) do
	  (setf v (cos(* pi (- (cl::1+ i) 0.25) nph)))
	  (loop for i from 1 to 4  do	; 3 is almost enough.
		(multiple-value-bind (val deriv)(legendre_pd n v)
		  (declare (double-float val deriv))
		  ;;new_guess= guess-f(x)/f'(x)
		  (setf v (- v (/ val deriv)))))
	  (setf  a (mpfr::into v))

	  (loop for k from 5 to (ceiling (log (get-prec) 2)) do
		;;(format t "~% globalprec=~s current prec= ~s" globalprec (get-prec))
		(multiple-value-bind (val deriv)(legendre_pd n a)
		  ;;new_guess= guess-f(x)/f'(x)
		;;   (format t "~%approx=~s" a)
		  (setf a  (- a (/ val deriv))) ))
	  ;; enough precision.
	  (setf (aref abscissae i) a)
	  (setf (aref weights i) (legendre_pdwt np1 a)) )
    (cond ((oddp n)
	   (setf (aref abscissae  halfn) (mpfr::into 0))
	   (setf (aref weights  halfn)	; fill in middle element by subtraction 2-2*(sum of others)
		 (let ((sum (mpfr::into 0)))
		   (loop for i from 0 to (1- halfn)  
		       do (setf sum (+ sum (aref weights i ))))
		   (setf sum (- 2 (+ sum sum)))))))
       (values abscissae weights)))

(defun gaussab (fun lo hi n) ;; integrate from lo to hi
  (let ((a (* 1/2 (- hi lo)))
	(b (* 1/2 (+ hi lo)))
	)
    (* a (gaussunit #'(lambda(r)(funcall fun (+ b (* a r))))
		    n))))

(defun gauss0inf (fun n) ;; integrate from 0 to inf, not a great way to sample.
  (gaussunit #'(lambda(x)
		 (let ((d (- 1 x)))
		   (/ (funcall fun (/ (+ x 1) d)) (* d d))))
	     n))

;; tanh/sinh method. This also integrates from -1 to 1.

(defun quadts(fun n)
  (let*((piby2(/(mpfr::mppi) 2))
	(h (mpfr::into(/ 4 n)))
	(sum (* piby2 (funcall fun 0)))
	(he (exp h))
	(relerr(mpfr::into (expt 2 (- 3 (mpfr::get-prec)))))
	(t2 1)t3 t4 ab cor (oldsum (mpfr::into 0)))
    (loop for j from 1 to (* 2 n) do
	  (setf t2 (* t2 he))
	  (setf t3 (exp (* piby2 (/ (- (* t2 t2) 1)
				    (* t2 2)))))
	  (setf ab (* t3 t3))
	  (setf t4 (/ (+ ab 1)(* 2 t3)))
	  (setf ab (/ (- ab 1)(* 2 t3 t4)))
	  (setf cor 
		(*(/ (* piby2 (+ (* t2 t2) 1))
		     (* 2 t2 t4 t4))
		  (+ (funcall fun ab)(funcall fun (- ab)))))
	  (setf oldsum sum)
	  (setf sum (+ sum cor)) 
	  ;;(print sum)
	  (if (= oldsum 0) (if (= sum 0) (return 0)))
	  (if (<(abs(/ (- sum oldsum) 
		       (if (= 0 oldsum)sum oldsum )))
		relerr)
	      (return (* sum h))))
    ;; fell out of loop
    (* sum h)))

(defun quadtsab (fun lo hi n) ;; integrate from lo to hi
  (let ((a (* 1/2 (- hi lo)))
	(b (* 1/2 (+ hi lo))))
    (* a (quadts #'(lambda(r)(funcall fun (+ b (* a r))))
		 n))))

;;evaluate fun(args) and return appx value and uncertainty, heuristic.

(defun uncert(fun args); list of args. 
  (let((ll 0)(hh 0)(oldprec (mpfr::get-prec)))
    (setf ll (funcall fun args)) ;assume this returns a bigfloat.
    (mpfr::set-prec (+ 10 oldprec))	; 10 bits more
    (setf hh (funcall fun args)) ;assume this returns a more accurate bigfloat
    (setf dd (abs (- hh ll)))
    (mpfr::set-prec oldprec)
    (values (into hh) (into dd)))) ;; a function value and uncertainty
    
(defun gaussunit_e (fun n) ;; integrate from -1 to 1, return val and error
  (let* ((ufun `(lambda(r)
		   (cadr (multiple-value-list (uncert ,fun  r)))))
	 (thevalue  (gaussunit fun n))
	 ;; this is wasteful, redoes stuff..
	 (theerror  (gaussunit ufun n)))
	(values thevalue theerror)))

(defun gaussab_e (fun lo hi n) ;; integrate from lo to hi
  (let ((a (* 1/2 (- hi lo)))
	(b (* 1/2 (+ hi lo))))
    (* a (gaussunit #'(lambda(r)(funcall fun (+ b (* a r))))
		 n))))    


