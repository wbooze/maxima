;;; -*- Mode:Common-Lisp; Package:octi; Base:10 -*-
;;; A quadruple-precison floating-point arithmetic package.
;;; minimum components for oct implementation.
;;; see octi.lisp for documentation, license, etc.

;;; Other parts in the works include user-overloaded 
;;; data types, elementary functions, complex, etc.

;;; 10/27/07 RJF
(defpackage :octi
  (:use  :cl)
  (:export into lisp2oct oct2lisp
	   add-oct-t add-oct-d-t
	   sub-oct-t sub-oct-d-t
	   mul-oct-t mul-oct-d-t
	   div-oct-t div-oct-d-t
	   sqr-oct-t sqr-oct-d-t	   
	   neg-oct-t neg-oct-d-t	   
	   abs-oct-t abs-oct-d-t	   
	   pow-oct-i pow-oct-i-t))


;; compute power of oct to an integer, store in ans
(defun pow-oct-i-t(a n ans)			; n is positive integer
  ;; this is generalized in expt-oct.
  (cond((cl:zerop n) (setf (aref ans 0) 1d0) ans)
       ((cl:evenp n)(sqr-oct-t (pow-oct-i-t a (cl:ash n -1) ans) ans))
       (t (mul-oct-t a (pow-oct-i a (1- n)) ans))))

(defun expt-oct-t(a b ans) ;; a^b, where a, b are both octs.
  ;;a^b = exp(b*log(a))
  ;; first see if b is really an integer.
  (let* ((r (oct-0 b))
	 (s (round r)))
       
    (if (and (= r s)(= 0d0 (oct-1 b) (oct-2 b) (oct-3 b)))
	(pow-oct-i-t a s ans)
      (exp-oct-t (mul-oct b (log-oct a)) ans))))

(defun mul-oct-d-t (a b ans)
  "Multiply quad-double A with B, result put in ans"
  (declare (type %quad-double a ans)
	   (double-float b)
	   (optimize (speed 3)(space 0)(safety 1)))
  (let ((p0 0d0)(q0 0d0)(p1 0d0)(q1 0d0) (p2 0d0)(q2 0d0))
    (declare (double-float p0 q0 p1 q1 p2 q2))
    (two-prodmac p0 q0 (oct-0 a) b)
    (when (float-infinity-p p0)
      (setf (oct-0 0)p0)
      (return-from mul-oct-d-t ans))
    (two-prodmac p1 q1 (oct-1 a) b)
    (two-prodmac p2 q2 (oct-2 a) b)
    (let* ((p3 (cl:* (oct-3 a) b))
	   (s0 p0)
	   (s1 p0)
	   (s2 p0))
      (declare (double-float s0 s1 s2 p3))
      (two-summac s1 s2 q0 p1)
      (three-summac s2 q1 p2 s2 q1 p2)
      (three-sum2mac q1 q2 q1 q2 p3)
      (let ((s3 q1)
	    (s4 (cl:+ q2 p2)))
	(renorm-5mac s0 s1 s2 s3  s0 s1 s2 s3 s4)
	(cond ((zerop s0)
	       (setf (aref ans 0)(float-sign p0 0d0))
	       (setf (aref ans 1) 0d0)
	       (setf (aref ans 2) 0d0)
	       (setf (aref ans 3) 0d0))
	      (t
	       (setf (aref ans 0) s0)
	       (setf (aref ans 1) s1)
	       (setf (aref ans 2) s2)
	       (setf (aref ans 3) s3)))
	ans))))

(defun neg-oct-t (a target)
  (let((a0 0d0)(a1 0d0)(a2 0d0)(a3 0d0))
    (declare (type %quad-double a target)
	     (double-float a0 a1 a2 a3)(optimize (speed 3)))
    (oct-partsmac a0 a1 a2 a3 a)
    (setf (aref target 0) (cl:- a0))
    (setf (aref target 1) (cl:- a1))
    (setf (aref target 2) (cl:- a2))
    (setf (aref target 3) (cl:- a3))
    target))

(defun neg-oct-d-t (a target)
  (declare (type %quad-double a target)
	   (optimize (speed 3)))
    (setf (oct-0 target) (cl:- a))
    (setf (oct-1 target) 0d0)
    (setf (oct-2 target) 0d0)
    (setf (oct-3 target) 0d0)
    target))

(defun abs-oct-t(a target)
  (if (two-arg-< a +oct-zero+) (neg-oct-t a target) 
    (copy-octi-into-octi a target)))

(defun abs-oct-d-t (a target)
  (declare (type %quad-double a target)
	   (optimize (speed 3)))
    (setf (oct-0 target) (cl:abs a))
    (setf (oct-1 target) 0d0)
    (setf (oct-2 target) 0d0)
    (setf (oct-3 target) 0d0)
    target)

(defun sub-oct-t (a b target)
  (add-oct-t a (neg-oct b) target))

(defun sub-oct-d-t (a b target)
  (add-oct-d-t a (- b) target))

(defun add-oct-d-t (a b ans)
  "Add a quad-double A and a double-float B, store in ans"
  (declare (type %quad-double a)
	   (double-float b)
	   (optimize (speed 3)(safety 1)))
  (let ((c0 0.0d0)(c1 0.0d0)(c2 0.0d0)(c3 0.0d0) (e 0.0d0)
	(r0 0.0d0)(r1 0.0d0)(r2 0.0d0)(r3 0.0d0)	)
    (declare(double-float c0 c1 c2 c3 e r0 r1 r2 r3))
    (two-summac c0 e (aref a 0) b)
    (when (float-infinity-p c0)
      (setf (aref ans 0) c0)
      (setf (aref ans 1) 0d0)
      (setf (aref ans 2) 0d0)
      (setf (aref ans 3) 0d0)
      (return-from add-oct-d-t  ans))
    (two-summac c1 e (aref a 1) e)
    (two-summac c2 e (aref a 2) e)
    (two-summac c3 e (aref a 3) e)
    (renorm-5mac  r0 r1 r2 r3 c0 c1 c2 c3 e)
    (cond ((and (zerop (aref a 0)) (zerop b))
	         (setf (aref ans 0) c0)
		 (setf (aref ans 1) 0d0)
		 (setf (aref ans 2) 0d0)
		 (setf (aref ans 3) 0d0))
	  (t
	         (setf (aref ans 0) r0)
		 (setf (aref ans 1) r1)
		 (setf (aref ans 2) r2)
		 (setf (aref ans 3) r3)))
    ans))

(defun 1+-oct-t (a ans)(add-oct-d-t a 1d0 ans))
(defun 1--oct-t (a ans)(add-oct-d-t a -1d0 ans))


;;
;;Sine and Cosine. Used after reduction so |a|<pi/2048.
;; copied from Ray Toy's code.

(defun sincos-taylor(a)
  (let ((s (into 0))
	(c (into 0)))
    (sincos-taylor-t a s c)
    (values s c)))

(defun sincos-taylor-t (a s-ans c-ans)
  (declare (type %quad-double a))
  (let ((thresh (cl:* +oct-eps+ (abs (oct-0 a)))))
    (when (zerop-oct a)
      (copy-octi-into-octi +oct-zero+ s-ans)
      (copy-octi-into-octi +oct-one+ c-ans)
      (return-from sincos-taylor-t s-ans))
    (let* ((x (neg-oct (sqr-oct a)))
	   (s a)
	   (p a)
	   (m 1d0))
      (loop
	 (setf p (mul-oct p x))
	 (incf m 2)
	 (setf p (div-oct-d p (cl:* m (cl:1- m))))
	 (setf s (add-oct s p))
	 ;;(format t "p = ~A~%" (oct-0 p))
	 (when (<= (abs (oct-0 p)) thresh)
	   (return)))
      ;; cos(c) = sqrt(1-sin(c)^2).  This works ok, since c is small.
      (copy-octi-into-octi s s-ans)
      (sqrt-oct-t (add-oct-d (neg-oct (sqr-oct s)) 1d0) c-ans)
      s-ans)))

;;; end of octi arithmetic package

;;; see also oct/qd-methods for Ray's version of =, /=, >, etc etc
;;; Tiring to do it over again. Can we just use it?  I think I like my
;;; version with  ga::monotone.

(defun octi::two-arg-< (a b)
  (declare (type (simple-array double-float (4)) a b)(optimize (speed 3)))
  (or (cl:< (oct-0 a)(oct-0 b))
      (and(cl:= (oct-0 a)(oct-0 b))
	  (cl:< (oct-1 a)(oct-1 b)))
      (and(cl:= (oct-1 a)(oct-1 b))
	  (cl:< (oct-2 a)(oct-2 b)))
      (and(cl:= (oct-2 a)(oct-2 b))
	  (cl:< (oct-3 a)(oct-3 b)))))

(defun two-arg-> (a b)
  (declare (type (simple-array double-float (4)) a b)(optimize (speed 3)))
  (or (cl:> (oct-0 a)(oct-0 b))
      (and(cl:= (oct-0 a)(oct-0 b))
	  (cl:> (oct-1 a)(oct-1 b)))
      (and(cl:= (oct-1 a)(oct-1 b))
	  (cl:> (oct-2 a)(oct-2 b)))
      (and(cl:= (oct-2 a)(oct-2 b))
	  (cl:> (oct-3 a)(oct-3 b)))))

(defun two-arg-= (a b)
  (declare (type (simple-array double-float (4)) a b)(optimize (speed 3)))
  (and(cl::= (oct-0 a)(oct-0 b))
      (cl::= (oct-1 a)(oct-1 b))
      (cl::= (oct-2 a)(oct-2 b))
      (cl::= (oct-3 a)(oct-3 b))))

(defun two-arg-/= (a b)(not (two-arg-= a b)))
(defun two-arg-<= (a b)(not(two-arg-> a b)))
(defun two-arg->= (a b)(not(two-arg-< a b)))
;; This does not work with NaNs; if we checked,  
;; all compares with NaNs are false (nil).

(defmacro plusp-oct (x)`(> (oct-0 ,x) 0d0)) ;sign will always be on 0th term
(defmacro minusp-oct (x)`(< (oct-0 ,x) 0d0))
(defun zerop-oct(x)(= (oct-0 x) 0d0))

#| oct can record some numbers to very high accuracy if they can
 be encoded as the sum of two floats of very different exponent.
 Seeing this is not easy; you must either print to many places
 or look at the internal encoding.

 To 65 places, these two numbers look the same:
 (setf r1  (into (expt 2 256))) ; 2^256
 (setf r2  (add-oct (into (expt 2 256))(into (expt 2 -256)))); 2^256+2^(-256)
 (oct-decode r1)   
 (oct-decode r2)

but if you look at 160 decimal digits, 
 (oct-decode  r1 160) 
 (oct-decode  r2 160) 

you see they differ.

 Probably a better way to look at the numbers is in binary.
	     
(oct-decode  r2 200 10)

Another set of anomalies occurs at the lower limits (small numbers).
On the system I am using at the moment, the (gradually underflowing)
least-positive-double-float number is lpdf= 5.0d0-324.  Encoding that
as an OCT does not provide any room for more precision, and indeed
multiplying lpdf by 1.01d0 does not change it at all.

Results are not quite so extreme if we look at the (not underflowing)
least-positive-normalized-double-float, lpndf=2.2250738585072014d-308.

Here we can multiply lpndf by 1.01d0 and get a different number, but
even so, numbers of this magnitude are not represented to quad
precision: much of the precision is washed out in underflow of the
lower-order terms.

|#

(defun nint-oct (a)
  "Round the quad-float to the nearest integer, which is returned as a
  quad-float"
 (declare  (optimize (speed 3)(safety 1)))
  (let ((x0 (fround (oct-0 a)))
	(x1 0d0)
	(x2 0d0)
	(x3 0d0))
    (declare (double-float x0 x1 x2 x3))
    (cond ((= x0 (oct-0 a))
	   ;; First double is already an integer
	   (setf x1 (fround (oct-1 a)))
	   (cond ((= x1 (oct-1 a))
		  ;; Second is an integer
		  (setf x2 (fround (oct-2 a)))
		  (cond ((= x2 (oct-2 a))
			 ;; Third is an integer
			 (setf x3 (fround (oct-3 a))))
			(t
			 (when (and (zerop (abs (cl:- x2 (oct-2 a))))
				    (minusp (oct-3 a)))
			   (decf x2)))))
		 (t

		  (when (and (zerop (abs (cl:- x1 (oct-1 a))))
			     (minusp (oct-2 a)))
		    (decf x1)))))
	  (t
	   (when (and (zerop (abs (cl:- x0 (oct-0 a))))
		      (minusp (oct-1 a)))
	     (decf x0))))

    (let ((s0 0d0)(s1 0d0)(s2 0d0)(s3 0d0))
	  (renorm-4mac s0 s1 s2 s3  x0 x1 x2 x3)
	  (%make-oct-d s0 s1 s2 s3))))


(defun oct-=(x y)
  (declare (optimize (speed 3)))
  (and(= (the double-float (oct-0 x))(the double-float (oct-0 y)))
      (= (the double-float (oct-1 x))(the double-float (oct-1 y)))
      (= (the double-float (oct-2 x))(the double-float (oct-2 y)))
      (= (the double-float (oct-3 x))(the double-float (oct-3 y)))))


(defun incf-oct(x &optional (y +oct-one+) )
  (add-oct-t x (octi::into y) x ))

(defun decf-oct(x &optional (y +oct-one+) )
  (sub-oct-t x (octi::into y) x ))


(defun ffloor-oct  (num &optional (div 1))
  ;; return quotient q and remainder r such that q*div+r=num
  ;; quotient is returned as an oct. remainder is an oct.
  (multiple-value-bind (quo rem)
      (floor (oct2lisp num)(oct2lisp div))
    (values (into quo)(into rem))))

(defun fceiling-oct  (num &optional (div 1))
  ;; return quotient q and remainder r such that q*div+r=num
  ;; quotient is returned as an oct. remainder is an oct.
  (multiple-value-bind (quo rem)
      (ceiling (oct2lisp num)(oct2lisp div))
    (values (into quo)(into rem))))

(defun ftruncate-oct  (num &optional (div 1))
  ;; return quotient q and remainder r such that q*div+r=num
  ;; quotient is returned as an oct. remainder is an oct.
  (multiple-value-bind (quo rem)
      (truncate (oct2lisp num)(oct2lisp div))
    (values (into quo)(into rem))))

(defun fround-oct  (num &optional (div 1))
  ;; return quotient q and remainder r such that q*div+r=num
  ;; quotient is returned as an oct. remainder is an oct.
  (multiple-value-bind (quo rem)
      (round (oct2lisp num)(oct2lisp div))
    (values (into quo)(into rem))))

;; how about a function that returns 2 answers, like sincos?

;; how about complex octs?

#| Here is the list of CL stuff that might make sense, defined in this package purely for octi real data: array of 4 doubles.

* / + - = /= < > <=  >=   ;; done as two-arg-*   etc

;; partly done in REAL
;; see RToy's code for most of these.

expt done for integer exponent as pow
... maybe faster ffloor etc.

;; not yet done
minusp plusp 

ftruncate fround

mod rem 
signum random numberp cis
complex complexp conjugate phase realpart imagpart realp 
numerator denominator rational rationalize 
decode-float 

See file oct-const for constants

|#



;; a version of exp function based on oct  from Ray Toy
;;  exp-qd/reduce
(defun exp-oct-t (a target)
  ;; Strategy:  Reduce the size of x by noting that
  ;;
  ;; exp(k*r+m) = exp(m) * exp(r)^k
  ;;
  ;; Thus, by choosing m to be a multiple of log(2) closest to x, we
  ;; can make |kr| < log(2)/2 = 0.3466.  Now we can set k = 256, so
  ;; that |r| <= 0.00136.
  ;;
  ;; Then
  ;;
  ;; exp(x) = exp(k*r+s*log(2)) = 2^s*(exp(r))^256
  ;;
  ;; We can use Taylor series to evaluate exp(r).

  (let* ((k 256)
	 (z (truncate (oct-0 (nint-oct (div-oct a +oct-log2+)))))
	 (r (div-oct-d (sub-oct a (mul-oct-d +oct-log2+ (float z 1d0)))
		      (float k 1d0)))
	 ;; For Taylor series.  p = r^2/2, the first term
	 (p (div-oct-d (sqr-oct r) 2d0))
	 ;; s = 1+r+p, the sum of the first 3 terms
	 (s (add-oct-d (add-oct r p) 1d0))
	 ;; Denominator of term
	 (m 2d0))
    ;; Taylor series until the term is small enough.
    ;;
    ;; Note that exp(x) = sinh(x) + sqrt(1+sinh(x)^2).  The Taylor
    ;; series for sinh has half as many terms as for exp, so it should
    ;; be less work to compute sinh.  Then a few additional operations
    ;; and a square root gives us exp.
    (loop
      (incf m)
      (mul-oct-t p r p)			;setf p
      (div-oct-d-t p m p)		;setf p
      (add-oct-t s p s)			;setf s
      (unless (> (abs (oct-0 p)) +oct-eps+)
	 (return)))
   (scale-float-oct-t (pow-oct-i-t s k r) z target) ))

;;exp(1)
;;2.718281828459045235360287471352662497757247093699959574966967627724077b0


;; EVALUATING a POLYNOMIAL

;; Octlist is an ordinary lisp list of the coefficients in a
;; polynomial. The last item in the list is the constant
;; coefficient. All coeffs are octi objects.  

;; (setf testpoly (list (into 5) (into 3) (into 1))) ; 5*x^2+3*x+1 
;; (setf r0 (polyeval-oct testpoly (into 1)); returns 9
;; 

(defun polyeval-oct (octlist x)
  (let ((sum (into 0))
	(tmp #.(%make-oct 0d0 0d0 0d0 0d0)))
	(dolist (i octlist sum)
	  (add-oct-t (mul-oct-t x sum tmp) i sum))))

;; similar but dlist is a list of DOUBLE FLOATS.
;; evaluate the polynomial to a quad-double

(defun polyeval-oct-d (dlist x)
  (let ((sum (into 0)))
	(polyeval-oct-d-t dlist x sum)))

(defun polyeval-oct-d-t (dlist x sum) ;; sum is the target
  (let ((sum (copy-octi-into-octi +oct-zero+ sum))
	(tmp #.(%make-oct 0d0 0d0 0d0 0d0)))
	(dolist (i dlist sum)
	  (add-oct-d-t  (mul-oct-t x sum tmp) i sum))))

;;; an example to show that oct numbers can have very high pseudo precision..
;;;  (setf testpoly2 (list (into 5) (into 3) (into 0))) ; 5*x^2+3*x  
;;;  (setf r1  (polyeval-oct testpoly (into (expt 2 510))))
;;; #(5.617791046444737d+307 1.0055855947456948d+154 1.0d0 0.0d0)
;;;  (setf r2  (polyeval-oct testpoly2 (into (expt 2 510))))
;;; #(5.617791046444737d+307 1.0055855947456948d+154 0.0d0 0.0d0)
;;;  (sub-oct r1 r2) --> 1

;;;  we can evaluate exp(x) for  |x|< 0.01, say, by
;;; a pade approximation, computed using Maxima.

;;[(x^10+110*x^9+5940*x^8+205920*x^7+5045040*x^6+90810720*x^5+1210809600*x^4+11762150400*x^3+79394515200*x^2+335221286400*x+670442572800)/(x^10-110*x^9+5940*x^8-205920*x^7+5045040*x^6-90810720*x^5+1210809600*x^4-11762150400*x^3+79394515200*x^2-335221286400*x+670442572800)]

;; Notice that the numerator and denominator have the same
;; coefficients with different signs on odd terms, so they can be
;; evaluated as f(x^2)+x*g(x^2) and f(x^2)-x*g(x^2) respectively using
;; about half the number of multiplies.

(defconstant f-exp-pade  '(1.0d0 5940.0d0 5045040.0d0  1.2108096d+9
			   7.93945152d+10 6.704425728d+11))

(defconstant g-exp-pade  '(110.0d0  205920.0d0  9.081072d+7 
			   1.17621504d+10  3.352212864d+11 ))

#+ignore  ;; an alternative from rjf. Not quite so accurate./
(defun exp-oct-small(x) ;; re-use temps  exp(x) for |x|<0.01, to quad-double  
  (let* ((x2 (sqr-oct x))
	 (fx2 (polyeval-oct-d f-exp-pade x2))
	 (xgx2 (mul-oct x (polyeval-oct-d g-exp-pade x2))))
    ;; 12 oct*oct, 10 oct+double  so far. 

    ;;(format t "~%f=~s g=~s"  (add-oct-t fx2 xgx2 x2)(sub-oct-t fx2 xgx2 xgx2))
    ;; next line takes 2 oct adds and a divide
		(div-oct-t (add-oct-t fx2 xgx2 x2)(sub-oct-t fx2 xgx2 xgx2) fx2)
		))

#+ignore
(defun exp-oct-t(a target)
  (if (< (oct-0 a) 0d0)(invert-oct-t (exp-oct-t (neg-oct-t a target) target) target)
    
    (multiple-value-bind (m n)
	(divrempos-oct a +oct-log2+)

      ;; now a= m*log(2)+n.  compute 2^m* exp(n/256)^256.
 ;;     (format t "~%doing exp ~s, m=~s, n=~s" a m n)
      (mul-oct-t (pow-oct-i (into 2) (truncate (oct-0 m)));2^m
		 (pow-oct-i  (exp-oct-small 
				   (div-oct-d n  256.0d0))
			    256)
		 target))));;  (e^(n/256))^256

#|
;; more coefficients --> not more accurate??

;; pade approx
;; x^12+156*x^11+12012*x^10+600600*x^9+21621600*x^8+588107520*x^7+12350257920*x^6+201132771840*x^5+2514159648000*x^4+23465490048000*x^3+154872234316800*x^2+647647525324800*x+1295295050649600



(defconstant f-exp-pade2		; coefs of x^12, x^10, ..., x^0
    '(1d0 12012d0 21621600d0 12350257920d0 2514159648000d0 154872234316800d0 1295295050649600d0))
(defconstant g-exp-pade2
    '(156d0 600600d0 588107520d0 201132771840d0 23465490048000d0  647647525324800d0))

;; no better than exp-oct-small, I think.
(defun eos(x) ;; re-use temps  exp(x) for |x|<0.01, to quad-double
  (let* ((x2 (sqr-oct x))
	 (fx2 (polyeval-oct-d f-exp-pade2 x2))
	 (xgx2 (mul-oct x (polyeval-oct-d g-exp-pade2 x2))))
    ;; 12 oct*oct, 10 oct+double  so far. 
    ;; next line takes 2 oct adds and a divide
    (div-oct-t (add-oct-t fx2 xgx2 x2)(sub-oct-t fx2 xgx2 xgx2) x2)))

(defun exp-oct2(a)
  (multiple-value-bind (m n)
      (ftruncate-oct a +oct-log2+)
    ;; now a= m*log(2)+n.  compute 2^m* exp(n/256)^256.
    (mul-oct (pow-oct-i (into 2) (truncate (oct-0 m)));2^m
	     (pow-oct-i (into (eos
			       (div-oct-d n  256.0d0)))
			256))))  

|#

;; my attempts to understand sin and cos.

#| To compute sin(x), we want to reduce the range of x, which could be any
representable number.

For example we find some y, 0<= y < 2*pi such that x=y+2*pi*N for some
integer N.  This process of range reduction may require that we know
pi to very many digits, at least if we are to be accurate.
After finding y, the method used in the qd library is to
decomposing y = a*pi/2 +b*pi/1024 + s, with a, b, non-negative integers,
and s some number less than pi/1024, or about 0.0030679616
Actually, a can be 0,1,2,3.  b can be 0 to 1023

let ss[b]:= sin(b*pi/1024), an entry in a table. Similarly, cc[b].
sin(y) is then
.. for a=0,  cc[b]*sin(s)+ss[b]*cos(s)
.. for a=1, -ss[b]*sin(s)+cc[b]*cos(s)
.. for a=2, -cc[b]*sin(s)-ss[b]*cos(s)
.. for a=3,  ss[b]*sin(s)-cc[b]*cos(s)
.. for a=4, same as a=0 etc

so all we need to do is pre-compute tables of sines and cosines, and
compute the approximation (by taylor series or other means) for sin(s)
and cos(s) where s is guaranteed to be less than about 0.003068. The
largest error would be at s=0.003068, and if we run the Taylor series
to 17 terms, the first neglected term x^19/19! would be at most
1.463e-65 . This is a little close, since sin(pi/1024) is about
0.0030, so an error of 1.4e-65 would show up. So we can arrange some
room for slosh, by keeping x^19 and leaving off the x^21 term, at
3.277e-73. At least I think this will work.

We have 2 arrays, sina cosa, corresponding to ss and cc in the formulas above.

;; Regarding Range Reductions
;; Kind of arbitrary cutoff to do exact range reduction.
;; we replace y by y-2*n*pi where pi is known to huge precision.
;; This is important if y=, say, 6283185307179586476925286766559006, which
;; is very close 10^33 times 2pi. If we compute y-10^33*pi, we have about
;; 31 decimal digits in the result, and 33 unknown because we used only 64
;; decimal digits (actually, 212 bits) of pi.  This way we use an extravagant
;; number of digits of pi to make sure that doesn't happen.
;; We don't do it often, so we hope efficiency is not an issue.
;; It's nice we can do exact integer stuff relatively simply in lisp.


|#
    
(defvar two-pi-many-bits ;; at least 1236 bits, as a rational number.
     22884743329868046105125534265462446198541589729467624782075250820510692843229684905276031268214145918457914724288873462827859618300605284995751611318802551321814951688142116429613752561980/3642220022337780247665573591773167826362669080317417579448364704693110029206203569295741890258185811272321756922288545062428726354857482731853161900688354006071607571978699476172264403277)
      
(defun careful-range-reduction(y)
    (multiple-value-bind (n m)		; y=n*2pi+m
	(truncate (oct2lisp y) two-pi-many-bits)
      (declare (ignore n))
    (into m)))

;;(defun sincos(a b c) ;; stub for sin/cos of small arg
;;		(copy-octi-into-octi (into (sin (oct-0 a))) b)
;;		(copy-octi-into-octi (into (cos (oct-0 a))) c))


;;(defun test-s (x)(list (oct-0(sin-oct (into x))) (sin x)))
;;(defun test-c (x)(list (oct-0(cos-oct (into x))) (cos x)))




#| 38999999999999384 * pi is very nearly the integer
k=122522113490000000, so range reduction tends to make the computation
of cos (122522113490000000) rather touchy.  Using Mathematica to
compute N[Cos[k]] as a double-float, and N[Cos[k],16] gives two
numbers
0.47237690434970653   (use InputForm[] to see the digits)
0.4722378698148295

The cosines agree to only 3 decimal digits.
This first value is not unique to Mathematica, but happens in 2 other systems
using ordinary floating point arithmetic. We tried (Maxima in GCL, Allegro CL, both
on Intel Pentium). 

The right answer? As computed by oct (rjf) and oct (rtoy) and then Mathematica (t0 70 places) and
then in Maxima (to 70 places), and then in Yozo's QD...
  0.472237869814829536825748604378294945886073849481652855352802025677Q0  oct 
#q0.47223786981482953682574860437829494588607384948165285535280202569q0   oct from rtoy; good
 "0.472237869814829536825748604378294945886073849481652855352802025777Q0" oct rjf; 10/26/07
  0.4722378698148295368257486043782949458860738494816528553528020257046972 mathematica 6.0
   4.722378698148295368257486043782949458860738494816528553528020257046972b-1 maxima
  0.472237869814829536825748604378294945886073849482247612284170520164Q0   qd yozo

Note that QD includes about 19 digits of noise. The last few digits printed of 
OCT and QD are something of a stretch. 63.8 decimal digits are representable in 212 bits;
The last few digits are not correct as decimals: OCT is really filling 
in the closest decimal digits to get closest to the last few bits.
 
|#

;; more code from Ray Toy
(defun expm1-oct (a)  
  "exp(a) - 1, done accurately"
  (declare (type %quad-double a))
  (when (float-infinity-p (oct-0 a))
    (return-from expm1-oct
      (if (minusp (float-sign (oct-0 a)))
	  +oct-zero+
	  a)))
  (expm1-oct/duplication a))


(defun expm1-oct/duplication (a)
  (declare (type %quad-double a))
  ;; Brent gives expm1(2*x) = expm1(x)*(2+expm1(x))
  ;;
  ;; Hence
  ;;
  ;; expm1(x) = expm1(x/2)*(2+expm1(x/2))
  ;;
  ;; Keep applying this formula until x is small enough.  Then use
  ;; Taylor series to compute expm1(x).
  (cond ((< (abs (oct-0 a)) .0001d0)
	 ;; What is the right threshold?
	 ;;
	 ;; Taylor series for exp(x)-1
	 ;; = x+x^2/2!+x^3/3!+x^4/4!+...
	 ;; = x*(1+x/2!+x^2/3!+x^3/4!+...)
	 (let ((sum +oct-one+)
	       (term +oct-one+))
	   (dotimes (k 28)
	     (setf term (div-oct-d (mul-oct term a) (float (cl:+ k 2) 1d0)))
	     (setf sum (add-oct sum term)))
	   (mul-oct a sum)))
	(t
	 (let ((d (expm1-oct/duplication (scale-float-oct a -1))))
	   (mul-oct d (add-oct-d d 2d0))))))

(defun sinh-oct (a)
  "Sinh(a)"
  (declare (type %quad-double a))
  ;; Hart et al. suggests sinh(x) = 1/2*(D(x) + D(x)/(D(x)+1))
  ;; where D(x) = exp(x) - 1.  This helps for x near 0.
  (cond ((zerop-oct a)
	 a)
	((float-infinity-p (oct-0 a))
	 a)
	(t
	 (let ((d (expm1-oct a)))
	   (when (float-infinity-p (oct-0 d))
	     (return-from sinh-oct d))
	   (scale-float-oct (add-oct d
				   (div-oct d (add-oct-d d 1d0)))
			   -1)))))


(defun cosh-oct (a)
  "Cosh(a)"
  (declare (type %quad-double a))
  ;; cosh(x) = 1/2*(exp(x)+exp(-x))
  (let ((e (exp-oct a)))
    (when (float-infinity-p (oct-0 e))
      (return-from cosh-oct e))
    (scale-float-oct (add-oct e (div-oct +oct-one+ e))
		    -1)))


(defun tanh-oct (a)
  "Tanh(a)"
  (declare (type %quad-double a))
  ;; Hart et al. suggests tanh(x) = D(2*x)/(2+D(2*x))
  (cond ((zerop-oct a)
	 a)
	((> (abs (oct-0 a)) (/ (+ (log most-positive-double-float)
				 (log 2d0))
			      4d0))
	 ;; For this range of A, we know the answer is +/- 1.
	 ;;
	 ;; However, we could do better if we wanted.  Assume x > 0
	 ;; and very large.
	 ;;
	 ;; tanh(x) = sinh(x)/cosh(x)
	 ;;         = (1-exp(-2*x))/(1+exp(-2*x))
	 ;;         = 1 - 2*exp(-2*x)/(1+exp(-2*x))
	 ;;
	 ;; So tanh(x) is 1 if the other term is small enough, say,
	 ;; eps.  So for x large enough we can compute tanh(x) very
	 ;; accurately, thanks to how quad-double addition works.
	 ;; (The first component is, basically 1d0, and the second is
	 ;; some very small double-float.)
	 #+ignore
	 (let* ((e (exp (* -2 a)))
		(res (- 1 (/ (* 2 e) (1+ e)))))
	   (if (minusp (float-sign (oct-0 a)))
	       (neg-oct res)
	       res))
	 (into (float-sign (oct-0 a))))
	(t
	 (let* ((a2 (mul-oct-d a 2d0))
		(d (expm1-oct a2)))
	   (div-oct d (add-oct-d d 2d0))))))

(defun asinh-oct (a)
  "Asinh(a)"
  (declare (type %quad-double a))
  ;; asinh(x) = log(x + sqrt(1+x^2))
  ;;
  ;; But this doesn't work well when x is small.
  ;;
  ;; log(x + sqrt(1+x^2)) = log(sqrt(1+x^2)*(1+x/sqrt(1+x^2)))
  ;;   = log(sqrt(1+x^2)) + log(1+x/sqrt(1+x^2))
  ;;   = 1/2*log(1+x^2) + log(1+x/sqrt(1+x^2))
  ;;
  ;; However that doesn't work well when x is large because x^2
  ;; overflows.
  ;;
  ;; log(x + sqrt(1+x^2)) = log(x + x*sqrt(1+1/x^2))
  ;;   = log(x) + log(1+sqrt(1+1/x^2))
  ;;   = log(x) + log1p(sqrt(1+1/x^2))
  #+ignore
  (log-oct (add-oct a
		  (sqrt-oct (add-oct-d (sqr-oct a)
				     1d0))))
  (cond ((< (abs (oct-0 a)) (sqrt most-positive-double-float))
	 (let ((a^2 (sqr-oct a)))
	   (add-oct (scale-float-oct (log1p-oct a^2) -1)
		   (log1p-oct (div-oct a
				     (sqrt-oct (add-oct-d a^2 1d0)))))))
	((float-infinity-p (oct-0 a))
	 a)
	(t
	 (if (minusp (oct-0 a))
	     (neg-oct (asinh-oct (neg-oct a)))
	   (let ((1/a (div-oct +oct-one+
			       a)))
	       (+ (log-oct a)
		  (log1p-oct (sqrt-oct (add-oct-d (sqr-oct 1/a) 1d0)))))))))


(defun acosh-oct (a)
  "Acosh(a)"
  (declare (type %quad-double a))
  ;; acosh(x) = log(x + sqrt(x^2-1))

  ;; log(x+sqrt(x^2-1)) = log(x+sqrt((x-1)*(x+1)))
  ;;  = log(x+sqrt(x-1)*sqrt(x+1))

  ;; Let x = 1 + y
  ;; log(1 + y + sqrt(y)*sqrt(y + 2))
  ;;   = log1p(y + sqrt(y)*sqrt(y + 2))
  ;;
  ;; However, that doesn't work well if x is large.
  ;;
  ;; log(x+sqrt(x^2-1)) = log(x+x*sqrt(1-1/x^2))
  ;;   = log(x) + log(1+sqrt(1-1/x^2))
  ;;   = log(x) + log1p(sqrt(1-1/x)*sqrt(1+1/x))
  ;;
  (cond ((< (abs (oct-0 a)) (sqrt most-positive-double-float))
	 (let ((y (sub-oct-d a 1d0)))
	   (log1p-oct (add-oct y (sqrt-oct (mul-oct y (add-oct-d y 2d0)))))))
	((float-infinity-p (oct-0 a))
	 a)
	(t
	 (let ((1/a (div-oct +oct-one+ a)))
	   (+ (log-oct a)
	      (log1p-oct (mul-oct (sqrt-oct (sub-oct +oct-one+ 1/a ))
				(sqrt-oct (add-oct-d  1/a 1d0)))))))))

(defun atanh-oct (a)
  "Atanh(a)"
  (declare (type %quad-double a))
  ;; atanh(x) = 1/2*log((1+x)/(1-x))
  ;;          = 1/2*log(1+(2*x)/(1-x))
  ;; This latter expression works better for small x
  #+nil
  (scale-float-oct (log-oct (div-oct (add-oct-d 1d0 a 1d0)
				  (sub-oct +oct-one+ a)))
		  -1)
  ;; atanh(+/-1) = +/- infinity.  Signal a division by zero or return
  ;; infinity if the division-by-zero trap is disabled.
  (if (oct-= (abs-oct a) +oct-one+)
      (div-oct (into (float-sign (oct-0 a)))
	      +oct-zero+)
      (scale-float-oct (log1p-oct (div-oct (scale-float-oct a 1)
					(sub-oct +oct-one+ a)))
		       -1)))


(defun log1p-oct/duplication (x)
  (declare (type %quad-double x)
	   (optimize (speed 3)))
  ;; Brent gives the following duplication formula for log1p(x) =
  ;; log(1+x):
  ;;
  ;; log1p(x) = 2*log1p(x/(1+sqrt(1+x)))
  ;;
  ;; So we apply the duplication formula until x is small enough, and
  ;; then use the series
  ;;
  ;; log(1+x) = 2*sum((x/(2+x))^(2*k+1)/(2*k+1),k,0,inf)
  ;;
  ;; Currently "small enough" means x < 0.005.  What is the right
  ;; cutoff?
  (cond ((> (abs (oct-0 x)) .005d0)
	 ;; log1p(x) = 2*log1p(x/(1+sqrt(1+x)))
	 (mul-oct-d (log1p-oct/duplication
		    (div-oct x
			    (add-oct-d
				      (sqrt-oct (add-oct-d  x 1d0)) 1d0)))
		   2d0))
	(t
	 ;; Use the series
	 (let* ((term (div-oct x (add-oct-d x 2d0)))
		(mult (sqr-oct term))
		(sum term))
	   (loop for k of-type double-float from 3d0 by 2d0
	      while (> (abs (oct-0 term)) +oct-eps+)
	      do
		(setf term (mul-oct term mult))
		(setf sum (add-oct sum (div-oct-d term k))))
	   (mul-oct-d sum 2d0)))))


(defun log1p-oct (x)
  "log1p(x) = log(1+x), done more accurately than just evaluating
  log(1+x)"
  (declare (type %quad-double x))
  #+cmu
  (when (float-infinity-p (oct-0 x))
    x)
  (log1p-oct/duplication x))

;; either (log array base array)  or (log array array). base is a cl real.

;; how to deal with log(arg base target)

;; vs               log(arg base)
;; vs               log(arg target)


#+ignore
(defun log-oct-t (x arg1 &optional (arg2  0 arg2p))
  ;; if arg2p, there are 3 args, log(x,base,target)
  ;; otherwise there are 2 args, log(x, target)
  (cond (arg2p   (log-oct-t-aux arg1 arg2)
		 arg2)
	(t (setf (oct-0 arg1)(cl:log (oct-0 x)))
	   arg1)))

(defun log-oct-t(a ans) ;; log base e.
  "Log(a)"
  (declare (type %quad-double a))
  (cond ((oct-=  a +oct-one+)
	 (copy-octi-into-octi +oct-zero+ ans))
	((zerop-oct a) (error "log-oct of zero"))
	((minusp (float-sign (oct-0 a)))
	 (error "log of negative"))
	(t
	 ;; Default is Halley's method
	 (copy-octi-into-octi (log-oct/halley a) ans))))

(defun log-oct/halley (a)
  (declare (type %quad-double a))
  ;; Halley iteration:
  ;;
  ;; x' = x - 2*(exp(x)-a)/(exp(x)+a)
  ;;
  ;; But the above has problems if a is near
  ;; most-positive-double-float.  Rearrange the computation:
  ;;
  ;; x' = x - 2*(exp(x)/a-1)/(exp(x)/a+1)
  ;;
  ;; I think this works better, but it's also probably a little bit
  ;; more expensive because each iteration has two divisions.
  (let ((x (into (log (oct-0 a)))))
    (flet ((iter (est)
	     (let ((exp (div-oct (exp-oct est)
				a)))
	       (sub-oct est
		       (scale-float-oct
			(div-oct (sub-oct-d exp 1d0)
				(add-oct-d exp 1d0))
			1)))))
      ;; Two iterations should be enough
      (setf x (iter x))
      (setf x (iter x))
     ; (setf x (iter x));; a third..
      x)))

;; encoding floats as integers, to make sure we get exactly the same
;; constants for quad-doubles
(defun float2form(f)(multiple-value-bind (signif expon sign)
			(integer-decode-float f)
		      (if (= signif 0) 0d0
		      `(scale-float (float ,(* sign signif) 1d0) ,expon))))

(defun dumpoct (a)
  `(%make-oct-d ,(float2form (oct-0 a))
		,(float2form (oct-1 a))
		,(float2form (oct-2 a))
		,(float2form (oct-3 a))))

(defun dumptab (q)
  `(make-array ,(length q) :initial-contents
	       ,(cons 'list
		     (map 'list #'dumpoct q))))

(defun drem-oct (a b)
  (declare (type %quad-double a b))
  (let ((n (nint-oct (div-oct a b))))
    (sub-oct a (mul-oct n b))))

(defun divrem-oct (a b)
  (declare (type %quad-double a b))
  (let ((n (nint-oct (div-oct a b))))
    (values n (sub-oct a (mul-oct n b)))))


(defun divrempos-oct (a b) ;; make remainder positive if a is pos.
  (declare (type %quad-double a b))
  (let* ((n (nint-oct (div-oct a b)))
	 (r (sub-oct a (mul-oct n b))))
    (when (and (cl:> (oct-0 a) 0d0) (cl:<  (oct-0 r) 0d0))
    ;  (format t "~% n=~s r=~s" n r)
      (sub-oct-d-t n 1d0 n)
      (add-oct-t r b r))
    (values n r ))))
