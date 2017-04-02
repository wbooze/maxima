;;  A basis for SIGNIFICANCE ARITHMETIC
;;  constructed by overloading generic arithmetic. 
;;  Richard Fateman, July, 2010
;; most text copied from other existing code in http://www.cs.berkeley.edu/~fateman/lisp/generic
;; I'm not sure what we want to store here.

;; for a number X+-eps, we can store (X, -log_10(eps))

;; so that (1.23, 5) means 1.2345~~~

;; or faster computing, we can do (X, -log_2(eps)).


;; where X can be a double-float, or a bigfloat, and we control the printing
;; (and the arithmetic) by looking at the 2nd component

;; /{1.0, 2}/    means   1.0 +- 2^(-2) or 1.0+- 0.25.


(defpackage :SA		;significance arithmetic uses generic arithmetic
  (:use :ga :cl)
  (:shadowing-import-from 
   :ga
   "+" "-" "/" "*" "expt"		;binary arith
   "=" "/=" ">" "<" "<=" ">="		;binary comparisons
   "sin" "cos" "tan"			;... more trig
   "atan" "asin" "acos"			;... more inverse trig
   "sinh" "cosh" "atanh"		;... more hyperbolic
   "expt" "log" "exp" "sqrt"		;... more exponential, powers
   "1-" "1+" "abs"
   "tocl" "re-intern"
   ) )

(eval-when (load)(require "ga") (provide "SA"))
(in-package :SA)

(defstruct sa  mid (sig double-float))

(defmethod print-object ((a sa) stream)
  (format stream "/{~a,~a}/"  (pbg(sa-mid a))(pbg(sa-sig a))))

(defun pbg(x) ;; print bad guy. This uses the mistake that infinities can be compared equal
  (if
      (badguy x)
      (case x
	((#.excl::*infinity-double*   #.excl::*infinity-single*)
	 "oo")
	((#.excl::*negative-infinity-double* #.excl::*negative-infinity-single*) 
	 "-oo")
	((#.excl::*nan-double*  #.excl::*nan-single*) 
	 "NaN")
	(otherwise x)) ;; not really a bad guy
    x))


;; must figure out sa version of sin, cos, tan, etc.
;; must figure out =, >, <,
;; = on sa  doesn't make sense unless sig is either infinite, or you use =
;; to mean (say) possibly-equal, as in Mathematica
;; from Graham, On Lisp, macro hackery
(defun mkstr (&rest args)
  (with-output-to-string (s)(dolist (a args) (princ a s))))

(defun symb (&rest args) (values (intern (apply #'mkstr args))))

;; take 2 real sa objects and grab their insides. Then
;; do something with them. sample usage...
;;(with-sa2 sa1 sa2 (mid1 sig1)(mid2 sig2) (sa (f1 mid1 mid2) (f2 sig1 mid2 sig2 mid1)))

(defmacro with-sa2 (struct1 struct2 names1 names2 &body body)
  (let ((gs1 (gensym))
	(gs2 (gensym)))
    `(let ((,gs1 ,struct1)
	   (,gs2 ,struct2))
       (let ,(append 
	      (mapcar #'(lambda (f field)
			  `(,f (,(symb "sa-" field) ,gs1)))
		      names1
		      '(mid sig))
	      (mapcar #'(lambda (f field)
			  `(,f (,(symb "sa-" field) ,gs2)))
		      names2
		      '(mid sig)))
	 ,@body))))

(defmacro with-sa (struct1 names1  &body body)
  (let ((gs1 (gensym)))
    `(let ((,gs1 ,struct1))
       (let  ,(mapcar #'(lambda (f field)
			  `(,f (,(symb "sa-" field) ,gs1)))
		      names1
		      '(mid sig))
	 ,@body))))

(defmethod ga::two-arg-+ ((r sa)(s sa))
  ;; adding sa to sa   (m1 +2^e1)+(m2+2^e2) =  (m1+m2) + 2 ^  (log(2^e1+2^e2)) ?
  ;; we don't really want to do this, do we??
  ;; note  the use of "+" in the line below. What does it mean?
  ;; it depends on the types of mid1 and mid2. They could be double-floats,
  ;; arbitrary precision MPFR numbers, exact integers, ratios, even intervals,
  ;; or complexes.  "+" is overloaded in the "ga" or generic arithmetic package,
  ;; and all this additional package (SA) does, is overload "+" some more.
  (with-sa2 r s (mid1 sig1)(mid2 sig2) (sa (+ mid1 mid2)(log (+(expt 2 e1)(expt 2 e2)) 2))))

(defmethod ga::two-arg-+ (r (s sa)) ;adding scalar num+ sa
  (with-sa s (mid1 sig1) (sa (+ mid1 r)(+ sig1 r))))

(defmethod ga::two-arg-+ ((s sa) r) ;; adding sa to scalar num
  (with-sa s (mid1 sig1) (sa (+ mid1 r)(+ sig1 r))))

(defmethod ga::two-arg-* ((r sa)(s sa))
  ;; multiplying sa by sa   (m1 +2^e1)*(m2+2^e2) =  m1*m2 + 2^e1*m2 +2^e2*m2  dropping 2^(e1+e2)
  ;; this too seems too hard.
  (with-sa2 r s (mid1 sig1)(mid2 sig2)
	      (sa (* mid1 mid2)(+ (log (+ (abs(* mid1 (log sig2 2)))(abs (* mid2 (log sig1 2)))))))))

( defmethod ga::two-arg-* (r (s sa))
  ;; multiplying num by sa
  (with-sa s (mid1 sig1)
	      (sa (* mid1 r) sig1)))

(defmethod ga::two-arg-* ((s sa) r) (ga::two-arg-* r s))

;; maybe copy rest of stuff from intervals.



