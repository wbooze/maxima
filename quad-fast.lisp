;;; Gaussian Quadrature, arbitrary precision.
;;; See quad-ga.lisp for basic ideas and comments
;;; This file is hacked to run faster and/or use less temp storage.

;;;  Richard Fateman  Jan 14, 2007.

(eval-when (compile load)
  (declaim (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))
  (load "mpfr.fasl"))

(in-package :mpfr)
;; legendre_pd returns legendre_p(k,x) and its derivative.
;;Note that d/dx P[n](x) =  (1/(x^2-1))*n*(x*P[n](x)-P[n-1](x)) except for x=+-1
;; This program is used with Newton iteration to refine roots of P[n](x).

(defun legendre_pd (k x) ;; use for float or generic version
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

;;bigfloat version
(defun legendre_pdbf (k x)(legendre_pd k x))

(defun legendre_pdbf (k x)
  (assert (and (typep k 'fixnum)(<= 0 k)))
  (setf x (mpfr::into x))		;make sure it's a bigfloat
  (case k 
    (0 (values 1 0))
    (1 (values x 1))
    (otherwise
     (let ((t0 (mpfr::into 1))
	   (t1 x); (mpfr::into x))
	   (ans (mpfr::into 0))
	   (i (mpfr::into 1)))
       (declare (fixnum k))
       (loop for ii from 2 to k  do
	     (dsetv i  (+ i 1)) ;; i is bigfloat version of ii
	     ;; make a new copy for ans, so t1 and ans are different!
	     (setf ans  (with-temps(/ (- (* (1- (* 2 i)) x t1) (* (1- i) t0)) i)))
	     (setf t0 t1)
	     (setf t1 ans) )
       (values  t1
		;; (1/(x^2-1))*k*(x*P[k](x)-P[k-1](x))
		;; except if abs(x)=1 then use
		(if (= 1 (abs x))
		    ;; 1/2*k*(k+1)*x^(k+1)
		    (/ (* k (1+ k) (if (oddp k) 1 x)) 2)
		  (with-temps (/ (* (into k) (- (* x t1)t0))  (1-  (* x x))))))))))

;; Starting from a point that approximates a root of a Legendre polynomial, 
;; improve it.

(defun improve_lg_root(n guess)
  (multiple-value-bind (val deriv)(legendre_pd n guess)
    (if (= deriv 0) (if (= val 0) val (error "newton iteration failed at ~s"guess))
      ;;new_guess= guess-f(x)/f'(x)
      (- guess (/ val deriv)))))

;; the bigfloat version
;; Here is a Newton iteration to converge to a root of legendre_p, bigfloat.
(defun improve_lg_rootbf(n guess)
  (multiple-value-bind (val deriv)(legendre_pdbf n guess)
    (if (= deriv 0) (if (= val 0) val (error "newton iteration failed at ~s"guess))
      ;;new_guess= guess-f(x)/f'(x)
      ;;(- guess (/ val deriv)) ; example of open-coding mpfr calls.
      (progn
	(mpfr_div (gmpfr-f val)(gmpfr-f val)(gmpfr-f deriv) 0) ;;val:=val/deriv
	(mpfr_sub (gmpfr-f guess)(gmpfr-f guess)(gmpfr-f val) 0)
      ;; this places the answer where the previous guess was.
      ;; we return it too, just for debugging.
	guess))))

;;The weights are w[j]:=  -2/ ( (n+1)* P'[n](x[j])*P[n+1](x))
;; this program computes them. 

(defun legendre_pdwt (k x &aux (kf (into k)))
  ;; compute -2 / (  k * P'[k-1](x)*P[k](x))
  (assert (and (typep k 'fixnum)(< 1 k))); k>=2
  (setf x (into x))
  (/ -2 
     (* kf
	(let 
	    ((t0 (into 1))
	     (t1 x)
	     (ans 0)
	     (i (mpfr::into 0)))
	  (declare (fixnum k))
	  (loop for ii from 2 to (1- k)  do
		(setf i (into ii))
		(setf ans   
		  ;; make a new number here
		      (/ (with-temps (- (* (1- (* 2 i)) x t1) (* (1- i) t0))) i)
		      t0 t1
		      t1 ans))
	  (*				;deriv of p[k-1]
	   (if (= 1 (abs x))		;not going to happen..
	       (/ (* kf (- kf 1) (if (evenp k) 1 x)) 2)
	     (with-temps(/ (* (- kf 1) (- (* x t1)t0)) (- (* x x) 1))))
	   ;; legendre_p[k](x)
	   (with-temps (/ (- (* (1- (* 2 kf)) x t1) (* (1- kf) t0)) kf))
	   )))))

#|
 :pa :mpfr
(mpfr::set-prec 200) ; bits of fraction
(int_gs_l2bf #'exp 40)
;; this one is twice as fast:
(int_gs_l4bf #'exp 40)
;; running the integration formula repeatedly is very cheap, after weights etc computed.
;; compare to correct answer 
(time (int_gs_l5bf #'exp 40))
(time (int_gs_l5bf #'exp 40))  ;; much faster 2nd time
;; the answer to this integration is computed by  (right)
(defun right ()(let ((k (exp (mpfr::into 1)))) (- k (/ 1 k)))))
|#

;;  To save allocation time and space for the array of weights etc, we do this:
;;  0. initialize s to 0. For i= 0 to n do lines 1-5:
;;   1. compute one abscissa x=x[i]
;;   2. compute one weight   w=w[i]
;;   3. compute one value    v=f(x[i])
;;   4. set s:= s+w*v.
;;   5. reusing x,w,v storage, repeat until n.

(defun int_gs_l2bf(fun n)
  (let* ((np1 (+ n 1))
	 (v nil)
	 (fv (mpfr::into 0))
	 (myprec 38) ; might be 53, but maybe not all correct
	 (sum (mpfr::into 0))
	 (goal-precision (mpfr::get-prec))
	 (nph (/ 1.0d0(+ n 0.5d0)) ))
    ;; we step from point 1 to point n, summing along the way
    (loop for j from 1 to n do
	  (setf v (cos(* pi (- j 0.25d0) nph))) ;initial approx. to a root of p[n]
	  (loop for k from 1 to 4 do ;; 4 iterations in double-float, from init refines it
		(setf v (improve_lg_root n v )))
	  ;;  do this refinement some number of times more in bigfloat
	  (setf fv (mpfr::into v))
	  (setf myprec 38) 
	  (loop while (< myprec goal-precision) do
		(mpfr::set-prec (min goal-precision (setf myprec (* 2 myprec))))
		(dsetv fv (improve_lg_rootbf n fv)))
	  ;; now that fv is accurate enough put it into weighted sum
	  (dsetv sum (with-temps(+ sum (* (legendre_pdwt np1 fv) (funcall fun fv))))))
    sum))

(defun ab_and_wts (n &optional (precision (get-prec)))		;mpfr precision. compute abscissae and weights.
  (let ((a 0)
	(v 0.0d0)
	(np1 (+ n 1))
	(nph (/ 1.0d0(+ n 0.5d0)))
	(halfn (ash n -1))
	(myprec 38)
	(globalprec (get-prec))
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
	  (setf  a (into v))
	  ;; compute more accurate abscissa.
	  ;;  we build up gradually to goal precision.
	  (setf myprec 38)
	  (loop while (< myprec precision) do
		
		(mpfr::set-prec  (min globalprec (setf myprec 
						   (* 2 myprec)  )))
		
		
		(multiple-value-bind (val deriv)(legendre_pdbf n a)
		  ;;new_guess= guess-f(x)/f'(x)
		  ;; dsetv alters a in place
		 ;; (format t "~%approx=~s" a)
		  (setf a  (- a (/ val deriv))) ))

	  ;; enough precision.
	  (setf (aref abscissae i) a)
	  (setf (aref weights i) (legendre_pdwt np1 a)) )
    (cond ((oddp n)
	   (setf (aref abscissae  halfn) (into 0))
	   (setf (aref weights  halfn)	; fill in middle element by subtraction 2-2*(sum of others)
		
 (let ((sum (into 0)))
		   (loop for i from 0 to (1- halfn)  do (dsetv sum (+ sum (aref weights i ))))
		   (dsetv sum (- 2 (+ sum sum)))))))
    	  (mpfr::set-prec globalprec)
    (values abscissae weights)))

;; This is a version of quadrature computing weights etc as we go, like l2,  but using symmetry.
(defun int_gs_l4bf(fun n) 
  (let* ((np1 (+ n 1))
	 (v nil)
	 (fv #.(mpfr::into 0))
	 (sum (into 0))
	 (myprec 38)
	 (nph (/ 1.0d0(+ n 0.5d0)) )
	 (goal-precision (mpfr::get-prec))
	 (halfn (ash n -1)))
    ;; we step from point 1 to point n, summing along the way
    (loop for j from 1 to halfn do
	  (setf v (cos(* pi (- j 0.25d0) nph))) ;initial approx.
	  (loop for k from 1 to 4 do ;; 4 iterations in double-float, from init
		(setf v (improve_lg_root n v ))	)
	  (setf fv (into v))
	  (setf myprec 38)
	 (loop while (< myprec goal-precision) do
	       (mpfr::set-prec  (min goal-precision  (setf myprec (* 2 myprec))))
		(dsetv fv (improve_lg_rootbf n fv)))
	  ;; now that v is accurate enough put it into weighted sum

	  (dsetv sum (with-temps(+ sum (* (legendre_pdwt np1 fv) 
					  (+ (funcall fun fv)
					     (funcall fun (- fv)))))))	  )
	  (if (oddp n)
	      (+ sum (* (funcall fun 0)(legendre_pdwt np1 0)))
	    sum)))

;;;; memoizing stuff...
;; here's where we store precomputed wts and abscissae
(defparameter *legendabs* (make-hash-table))
(defparameter *legendwts* (make-hash-table))
(defparameter *legendprec* (make-hash-table))

;; call (clear-leg-has) to  deallocate storage from hash tables for legendre polys
;; in case you think you need it for something else.

(defun clear-leg-hash()			
  (clrhash *legendabs*)
  (clrhash *legendwts*)
  (clrhash *legendprec*))

(defun int_gs_l5bf(fun n)
  ;; memoize the abscissae and weights
  (let ((prec (or (gethash n *legendprec*) 0))
	;;did we compute these weights previously
	(weights nil)
	(abscissae nil))
    
    (cond ((cl::>= prec (get-prec))	
	   ;; Is enough precision already computed for this number of terms?
	   (setf abscissae (gethash n *legendabs*));use previous version
	   (setf weights (gethash n *legendwts*)))
	  (t				
	   ;; not found or insufficient precision, so compute and remember them now.
	   (multiple-value-setq (abscissae weights)(ab_and_wts n (get-prec)))
	   (setf (gethash n *legendabs*) abscissae)
	   (setf (gethash n *legendwts*) weights)
	   (setf (gethash n *legendprec*) (get-prec))))
    ;; compute the sum.
    (let ((sum (into 0))	 
	  (halfn (ash n -1)))
      (loop for i from 0 to (1- halfn) do 
	    (dsetv sum 
		   (+ sum (*(+ (funcall fun (aref abscissae i))
			       (funcall fun (with-temps (-(aref abscissae i)))))
			    (aref weights i)))))
      (if (oddp n) (dsetv sum (+ sum (* (aref weights halfn)(funcall fun (into 0))))))
      sum)))

(defun gaussunit(fun n)  (int_gs_l5bf fun n)) ;; integrate from -1 to 1

;;; alternative calling functions, based on versions in macsyma
;;; see quad-maxima.lisp comments.

;;;gaussab(%gg,lo,hi,n):= 
;;; block([a:(hi-lo)/2, b:(hi+lo)/2], 
;;; a* gq1(lambda([x],%gg(a*x+b)),n,ab_and_wts[n,fpprec])),

(defun gaussab(fun lo hi n)  ;; integrate from lo to hi, using n points.
  (let ((a (into (/ (- hi lo) 2)))
	(b (into (/ (+ hi lo) 2)) ))
    (* a (gaussunit #'(lambda(x)(funcall fun (with-temps (+ b (* a (into x)))))) n))))

;;;gauss0inf(%gg,n):= 
;;; 2* gq1(lambda([t],block([d:(1-t)],
;;;                        %gg((1+t)/d)/d^2)),n,ab_and_wts[n,fpprec]),
(defun gauss0inf(fun n) ;; integrate from 0 to infinity
  (* 2 (gaussunit 
	#'(lambda(z) 
	     (let ((d (into (- 1 z))))
	       (/ (funcall fun 
			(with-temps
			    (/ (+ 1 z) d)))(* d d))))
	n)))

;;;gaussminfinf(%gg,n):= 
;;; 2* gq1(lambda([t],block([d: (1-t),
;;;                         r: (1+t)/(1-t)],
;;;			 (%gg(r)+%gg(-r))/d^2)),n,ab_and_wts[n,fpprec])

(defun gaussminfinf(fun n) ;; integrate from -infinity to infinity
  (* 2 (gaussunit 
	#'(lambda(z) 
	    (let* ((d (into (- 1 z)))
		   (r (with-temps (/ (+ 1 z) d ))))
	      (/ (+ (funcall fun r)
		    (funcall fun (- r))) (* d d))))
	n)))



;; we could try playing with this:
;; accurate summation in a multiple-precision setting...
#+ignore
(defun acsum(a) 
  ;;accurate summation of a[0]+ ...+a[last], all mpfr numbers
  ;; in an array a. Assume we are free to sort the array.
  (sort a #'> :key #'abs)
  (let ((p(set-prec (+ (get-prec) 48))))
    (prog1 (reduce #'ga::two-arg-+ a)
      (set-prec p))))

(defun acsum(a) 
  ;;accurate summation of a[0]+ ...+a[last], all mpfr numbers
  ;; in an array a. Assume we are free to sort the array.
  ;;(sort a #'> :key #'abs)
  (let ((p(set-prec (cl::+ (get-prec) 48)))); hack: just boost precision a bunch
    (prog1 (reduce #'ga::two-arg-+ a)
      (set-prec p))))
  


;;(setf r (vector (into 1)(into (expt 10 1000)) (into -1)(into (- (expt 10 1000)))))




;;; slower, more memory, acsum with sort has bug, different but maybe not much better when it counts.
(defun int_gs_l5bfa(fun n)		;like l5 but with accurate summation, stores terms
  ;; memoize the abscissae and weights
  (let ((prec (or (gethash n *legendprec*) 0))
	;;did we compute these weights previously
	(weights nil)
	(abscissae nil))
    
    (cond ((cl::>= prec (get-prec))	
	   ;; Is enough precision already computed for this number of terms?
	   (setf abscissae (gethash n *legendabs*));use previous version
	   (setf weights (gethash n *legendwts*)))
	  (t				
	   ;; not found or insufficient precision, so compute and remember them now.
	   (multiple-value-setq (abscissae weights)(ab_and_wts n (get-prec)))
	   (setf (gethash n *legendabs*) abscissae)
	   (setf (gethash n *legendwts*) weights)
	   (setf (gethash n *legendprec*) (get-prec))))
    ;; compute the sum.
    (let ((halfn (ash n -1))
	  (summands nil))
      (loop for i from 0 to (1- halfn) do 
	    (push (* (aref weights i)
		     (funcall fun (aref abscissae i))) summands)
	    (push (* (aref weights i)
		     (funcall fun (with-temps (-(aref abscissae i))))) summands))
      (if (oddp n) 
	  (push (* (aref weights halfn)(funcall fun (into 0)))	summands))
       (acsum summands)
      )))


#| some testing in Mathematica regarding Legendre zeros
;; One zero of LegendreP[10,x] is about 
;;0.973906528517171720077964012084452053428269946692382119231212066696595203234636159625723564956268556258233042518774211215022168601434477779920540958726
;; correct to the last rounded digit.
This program give about
;;0.97390652851717172007796401208425031143498830232617797549445377752271945185287852121950994663924446112734455480619806549061
;;................................^wrong


|#