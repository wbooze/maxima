;;; Gaussian Quadrature, arbitrary precision.
;;; See http://www.cs.berkeley.edu/~fateman/generic/quad-ga.lisp or quad-fast.lisp 
;;; for basic ideas and comments.
;;; hacked to run in Maxima or Macsyma, but consequently slower than using mpfr

(eval-when (load compile eval)
  (if (string= "MAXIMA" (package-name *package*))
      (in-package :maxima) ;; for sourceforge maxima
    (in-package :climax))) ;; for commercial macsyma

;; I suspect that binding fpprec is not the right thing to do in commercial Macsyma..

;;;  Richard Fateman  Feb 18, 2007

;; legendre_pd returns legendre_p(k,x) and its derivative.
;;Note that d/dx P[n](x) =  (1/(x^2-1))*n*(x*P[n](x)-P[n-1](x)) except for x=+-1
;; This program is used with Newton iteration to refine roots of P[n](x).
;; useful only from lisp because it uses value construct to return.

(defun legendre_pd (k x) ;; use for double float 
  (assert (and (typep k 'fixnum)(<= 0 k)
	       (typep x 'double-float)))
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
       (values t1
		      ;; (1/(x^2-1))*k*(x*P[k](x)-P[k-1](x))
		      ;; except if abs(x)=1 then use
		      (if (= 1 (abs x))
			  ;; 1/2*k*(k+1)*x^(k+1)
			  (/ (* k (1+ k) (if (oddp k) 1 x)) 2)
			(/ (* k (- (* x t1)t0))  (1-  (* x x)))
			
))))))

;;bigfloat version. Returns a macsyma list

(defun $legendre_pdbf (k x)
  (assert (and (typep k 'fixnum)(<= 0 k)))
  (setf x ($bfloat x))		;make sure it's a bigfloat
  (case k 
    (0 (list '(mlist) 1 0))
    (1 (list '(mlist) x 1))
    (otherwise
     (let ((t0 ($bfloat 1))
	   (t1 ($bfloat x))
	   (ans ($bfloat 0))
	   (i ($bfloat 1)))
       (declare (fixnum k))
       (loop for ii from 2 to k  do
	     (setf i (add i 1))
	     (setf ans 
		   (div (sub (mul (sub (mul 2 i) 1) x t1) (mul (sub i 1) t0)) i))
	     (setf t0 t1)
	     (setf t1 ans) )
       (list '(mlist)  t1
		;; (1/(x^2-1))*k*(x*P[k](x)-P[k-1](x))
		;; except if abs(x)=1 then use
		(if (equal (cdr bigfloatone) (fpabs (cdr x))) ;abs of the bf insides
		    ;; 1/2*k*(k+1)*x^(k+1)
			  (div (mul k (add 1 k) (if (oddp k) 1 x)) 2)
		  (div (mul k (sub (mul x t1)t0))  (sub  (mul x x) 1))))))))

;;The Gaussian quad weights are w[j]:=  -2/ ( (n+1)* P'[n](x[j])*P[n+1](x[j]))
;; this program computes w[k](x)

(defun $legendre_pdwt (k x &aux (kf ($bfloat k)))
  ;; compute -2 / (  k * P'[k-1](x)*P[k](x))
  (assert (and (typep k 'fixnum)(< 1 k))); k>=2
  (setf x ($bfloat x))
  (div -2 
     (mul kf
	(let 
	    ((t0 ($bfloat 1))
	     (t1 x)
	     (ans 0)
	     (i ($bfloat 0)))
	  (declare (fixnum k))
	  (loop for ii from 2 to (1- k)  do
		(setf i ($bfloat ii))
		(setf ans   
		      (div (sub (mul (sub (mul 2 i) 1) x t1) (mul (sub i 1) t0)) i)
		      t0 t1
		      t1 ans))
	  (mul				;deriv of p[k-1]
	   (div (mul (sub kf 1) (sub (mul x t1)t0)) (sub (mul x x) 1))
	   ;; legendre_p[k](x)
	    (div (sub (mul (sub (mul 2 kf) 1) x t1) (mul (sub kf 1) t0)) kf) )))))

(defun $ab_and_wts (n &optional(precision fpprec)) ;compute abscissae and weights upto bigfloat global precision
  (declare (special fpprec $float2bf ))
  (let ((a 0)
	(v 0.0d0)
	(np1 (+ n 1))
	(nph (/ 1.0d0(+ n 0.5d0)))
	(halfn (ash n -1))
	(myprec 38)
	(globalprec fpprec)
	($float2bf t) ; no warnings
	(abscissae  (make-array (ceiling n 2)))
	(weights   (make-array (ceiling n 2))))
    (declare (double-float v nph))
    (loop for i from 0 to (1- halfn) do
	  (setf v (cos(* pi (- (1+ i) 0.25) nph)));estimate of zero
	  (loop for i from 1 to 4  do	; 3 iterations is almost enough.
		(multiple-value-bind (val deriv)(legendre_pd n v)
		  (declare (double-float val deriv))
		  ;;new_guess= guess-f(x)/f'(x)
		  (setf v (- v (/ val deriv)))))
	  (setf  a ($bfloat v))
	  ;; compute more accurate abscissa in bigfloat
	  ;;  we build up gradually to goal precision.
	  (setf myprec 38)
	  (loop while (< myprec precision) do
		(setf fpprec  (min globalprec (setf myprec 
						       (* 2 myprec)  )))
		(let* ((valderiv($legendre_pdbf n a))
		       (val (cadr valderiv))
		       (deriv (caddr valderiv)))
		  ;;new_guess= guess-f(x)/f'(x)
		  (setf a  (sub a (div val deriv))) ))
	  ;; enough precision.
	  (setf (aref abscissae i) a)
	  (setf (aref weights i) ($legendre_pdwt np1 a)))
    (cond ((oddp n)
	   (setf (aref abscissae  halfn) 0)
	   (setf (aref weights  halfn)	; fill in middle element by subtraction 2-2*(sum of others)
		 (let ((sum  0))
		   (loop for i from 0 to (1- halfn)  do (setf sum (add sum (aref weights i ))))
		   (setf sum (sub 2 (add sum sum)))))))
    ;;(values abscissae weights) ;probably what we would prefer, for computing
    ;; to make it accessible to macsyma, return 2 lists.
    (list '(mlist) 
	  (cons '(mlist) (coerce abscissae 'list))
	  (cons '(mlist) (coerce weights 'list)))    
    ))

#|

A Maxima program

First define some function, e.g.  g(x):=exp(x).
Using gaussian integration to integrate g from -1 to 1 in the
current bigfloat precision, using n points, do gaussunit(g,n).

To see if your answer is correct, use n then 2n and 4n points, and/or
more and more digits. Compare answers. This will provide heuristic confirmation.


( gq1(%%g,n,aw):=
block([sum:0],
 map(lambda([%%a,%%w],sum:sum+%%w*(%%g(%%a)+%%g(-%%a))),aw[1],aw[2]),
 sum+ (if oddp(n) then -%%g(0)*aw[2][length(aw[1])] else 0 )),

 ab_and_wts[n,prec]:=   
/* memoize the abscissae and weights for Legendre zeros and Gauss quad */
  block([fpprec:prec],
         ab_and_wts(n)),

/* integrate function g(x) from x=-1 to x=1 using n points and fpprec */	 
gaussunit(%ggg,n):= gq1(%ggg,n,ab_and_wts[n,fpprec]),

/* integrate function g(x) from x=lo to x=hi using n points and fpprec */	 

gaussab(%hh,lo,hi,n):= 
  block([a:(hi-lo)/2, b:(hi+lo)/2], local(%zz),
     define (%zz(x),%hh(a*x+b)),
    a* gaussunit(%zz,n)),

/* integrate function g(x) from x=0 to x=inf using n points and fpprec */	 
gauss0inf(%gg,n):= 
 block([],local(%zz),
	       define(%zz(x),      
			 block([d:(1-x)], %gg((1+x)/d)/d^2)),
 2* gaussunit(%zz,n)),
/* integrate function g(x) from x=minf to x=inf using n points and fpprec */	 
gaussminfinf(%gg,n):= 
 block([],local(%zz),
      define(%zz(x),      
        block([d:(1-x),r:(1+x)/(1-x)], (%gg(r)+%gg(-r))/d^2)),
     2* gaussunit(%zz,n))

)$

			 |#

