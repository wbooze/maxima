;;; -*-  Mode: Lisp; Package: :ga; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;; Generic Arithmetic package
;; based on code posted on newsgroups by
;; Ingvar Mattsson <ing...@cathouse.bofh.se> 09 Oct 2003 12:16:09 +0100
;; code extended by Richard Fateman, November, 2005 -- February, 2006

(eval-when '(compile load) (load "packs"))

(provide :ga "ga.lisp")
(in-package :ga)

(defun tocl(n)
	;; get corresponding name in cl-user package
  (find-symbol (symbol-name n) :common-lisp))

#|
;; a tool to manufacture two-arg-X arithmetic for X=+,*, etc
(defmacro defarithmetic (op id0 id1)	;id0 = no args, id1 =1 arg
  (let ((two-arg
	 (intern (concatenate 'string "two-arg-" (symbol-name op))
		 :ga ))
        (cl-op (tocl op)))
    `(progn
       (defun ,op (&rest args)
         (cond ((null args) ,id0 )
               ((null (cdr args)),id1)
               (t (reduce (function ,two-arg)
                          (cdr args)
                          :initial-value (car args)))))
       (defgeneric ,two-arg (arg1 arg2))
       (defmethod ,two-arg ((arg1 number) (arg2 number))
	 (,cl-op arg1 arg2))
       (compile ',two-arg)
       (compile ',op)
       ',op)))

(defarithmetic + 0 (car args))
(defarithmetic - 0 (two-arg-* -1 (car args)))
(defarithmetic * 1 (car args))
(defarithmetic / (error "/ given no args") (car args))
(defarithmetic expt (error "expt given no args") (car args))
(defarithmetic min  (error "min given no args") (car args))
(defarithmetic max  (error "max given no args") (car args))
(defmethod two-arg-min(x y)(if (< x y) x y))  ;; unless something more specific is around
(defmethod two-arg-max(x y)(if (> x y) x y))
|#
;; defcomparison is a tool to manufacture two-arg-X numeric comparisons.
;; CL requires comparison args to be monotonic. That is in lisp, (> 3 2 1) is true.
;; maybe we could replace monotone by a macro??

(defun monotone (op a rest)(cond ((null rest) t)
				 #+allegro
				 ((excl::nan-p a) nil) ;; a NaN !
				 (t (and #+allegro
					 (not(excl::nan-p (car rest)))
				     (funcall op a (car rest))
					 (monotone op (car rest)(cdr rest))))))

(defmacro defcomparison (op)
  (let ((two-arg (intern (concatenate 'string "two-arg-" 
				      (symbol-name op))    :ga ))
        (cl-op (tocl op)))
    `(progn
        (defun ,op (&rest args)
	  (cond ((null args) (error "~s wanted at least 1 arg"  ',op))
               ((null (cdr args)) t) ;; one arg e.g. (> x) is true
               (t (monotone (function ,two-arg)
			    (car args)
			     (cdr args)))))
      (defgeneric ,two-arg (arg1 arg2))
      (defmethod ,two-arg ((arg1 number) (arg2 number)) (,cl-op arg1 arg2))
      (compile ',two-arg)
      (compile ',op)
      ',op)))

(defcomparison >)
(defcomparison =)
(defcomparison /=)
(defcomparison <)
(defcomparison <=)
(defcomparison >=)

(define-symbol-macro ga::* cl::*);we want to keep top-level symbols * + /
(define-symbol-macro ga::+ cl::+); for the Read-Eval-Print loop.
(define-symbol-macro ga::/ cl::/)

;; these two macros use ga::+, not cl:+, but should compute complex "places" more carefully
(defmacro incf (x &optional (delta 1)) `(setf ,x (+ ,x ,delta)))
(defmacro decf (x &optional (delta 1)) `(setf ,x (- ,x ,delta)))

(defun re-intern(s p)  ;utility to "copy" expressions into package p
  (cond	((consp s)(cons (re-intern (car s) p)
			(re-intern (cdr s) p)))
	((symbolp s)(intern (symbol-name s) p))
	(t s))) ;nil, number, string, other
			  
(defmacro r (op) ;;   ;make (ga::sin x) call (cl::sin x) for ordinary lisp numbers
     `(defmethod ,op ((arg number)) (,(intern op :cl) arg)))

(r sin) (r cos)  (r tan)
;;(r atan)  ;might be 2 arg
(r asin) (r acos)
(r sinh)(r cosh) (r tanh)
(r asinh)(r acosh) (r atanh)
;;(r log) might be 2 arg
(r exp)  (r sqrt)
(r 1+) (r 1-) (r abs)
(r numerator)(r denominator)
(r evenp)(r oddp)

#| notes on built-in CL number functions.|#

;; in CLOS we can't define a method with variable number of args
;; so we will use different names as shown below
(defmethod atan2((x number)(y number))(cl::atan x y))
(defmethod log10((x number))(cl::log x 10))
(defmethod logbase((x number)(b number))(cl::log x b))

(defmethod two-arg-atan((a real)(b real))  (cl:atan a b))
(defmethod two-arg-log((a real)(b real))  (cl:log a b))
(defmethod neg((a real))(cl::- a))
(defmethod erf((a real)) (error "no erf for CL native numbers yet"))
(defmethod ash((a integer)(b fixnum))(cl:ash a b))

(defun atan(r &optional s)  ;;replaces cl:atan function.
  (if s (ga::two-arg-atan r s) 
    (ga::one-arg-atan r)))
(defun log(r &optional s)  ;;replaces cl:log function.
  (if s (ga::two-arg-log r s) 
    (ga::one-arg-log r)))

(defmethod one-arg-atan((r number))   (cl::atan r))
(defmethod one-arg-log((r number))   (cl::log r))

;; We should consider adding routines in ga for ordinary
;; lisp numbers for these, since they are available in some of the other
;; packages:

;; sec, asec, sech, asech
;; csc and friends
;; cot and friends

;; applied math: bessel, si, ci, li, erf, erfc etc.

;; number theory: gcd, gamma, lngamma, zeta, factorial, 
;; binomial, Lambert-W,  prime-test, factoring, etc.

;; mostly data-hacking utilities
;; multiplication or division by powers of 2 (shifting)

;; CL random has one or two args (optional "state").

;; round, truncate, floor, ceiling, nearest-up, nearest-down,
;; nearest-toward-zero, nearest-away-zero

;; note that CL floor and ceiling can take 1 or two arguments,
;; so the interface could look like the atan or log
;; setup.

;; other handy items.
;; polynomial evaluation;
;; newton iteration for polynomial root;

;; extension to complex numbers.

;; perhaps we should establish a uniform convention: given a new
;; representation, e.g. for quad double (qd):
;; (qd:into lispnum) returns qdnumber
;; (qd:outof qdnum) returns lisp rational number.
;; Exact numeric equality if possible.
;; for qd, lispnum could be any REAL: fixnum, bignum, single, double, rational

;; for multiple precision floats with rounding based on gmp arithemtic,

;; (mpfr:into lispnum) returns gmpfr number (of default precision)
;; (mpfr:outof mpfrnum) returns lisp rational number.

;; for mpfr, lispnum could be 
;; any REAL: fixnum, bignum, single, double, rational maybe QD also??
;; or we could do QD -> lisp ->mpfr



;; experiment
;;
;;(eval-when (compile load)
(defmacro twobytwo  
    ;; expand  (twobytwo f (a b c) 0 1) into 
    ;; uh, (f a b) c).  order matters f = -..
    ;; expand  (twobytwo f (a) 0 1) into 0.  
    ;; args has a special meaning in the macro expansions. See below for examples
    ;;(twobytwo + (1 2)  0)  ;; returns 3
    ;;(twobytwo two-arg-+ (1 2)  0)  ;; returns 3
    
    (f args &optional (initial-value0 `(error "~s needs 1 or 2 args got 0:~s" ',f ',args))
		      (initial-value1 `(error "~s needs 2 args got 1:~s" ',f ',args)))
  
  (cond ((null args) initial-value0)
	((null (cdr args)) initial-value1)
	((null (cddr args))(cons f args))
	;;(t `(,f  ,(car args)(twobytwo ,f ,(cdr args))))
	
	(t `(,f (twobytwo ,f  ,(butlast args)) ,(car (last args))))
	))

(defmacro defarithmetic (op)
  (let* ((two-arg
	 (intern (concatenate 'string "two-arg-" (symbol-name op))
		 :ga ))
	 (cl-op (tocl op))
	 )
    `(progn
       (defgeneric ,two-arg (arg1 arg2))
       (defmethod ,two-arg ((arg1 number) (arg2 number))
	 (,cl-op arg1 arg2))
       ',op)))

(defarithmetic + ) 
(defarithmetic - )
(defarithmetic * )
(defarithmetic / )
(defarithmetic expt )
(defarithmetic min  )
(defarithmetic max  )

(defmacro + (&rest args)    `(twobytwo two-arg-+ ,args  0 ,(car args)))
(defmacro - (&rest args)    `(twobytwo two-arg-- ,args  0 (two-arg-* -1 ,(car args))))
(defmacro * (&rest args)    `(twobytwo two-arg-* ,args  1 ,(car args)))
(defmacro / (&rest args)    `(twobytwo two-arg-/ ,args  (error " / [divide] given 0 args") ,(car args)))
(defmacro expt (&rest args) `(twobytwo two-arg-expt ,args  (error " expt [power] given 0 args") ,(car args)))
(defmacro min (&rest args)  `(twobytwo two-arg-min ,args  (error " min given 0 args") ,(car args)))
(defmacro max (&rest args)  `(twobytwo two-arg-max ,args  (error " max given 0 args") ,(car args)))
(defmethod two-arg-min(x y)(if (< x y) x y))  ;; unless something more specific is around
(defmethod two-arg-max(x y)(if (> x y) x y))
(setf (get 'ga::two-arg-expt 'argnum) 2)
(setf (get 'ga::two-arg-+ 'argnum) 2)
(setf (get 'ga::two-arg-* 'argnum) 2)
(setf (get 'ga::two-arg-/ 'argnum) 2)
(setf (get 'ga::two-arg-- 'argnum) 2)
(setf (get 'ga::two-arg-min 'argnum) 2)
(setf (get 'ga::two-arg-max 'argnum) 2)

;)
(defmethod scale-float((r number) (s fixnum))   (cl::scale-float r s))

;; do we need this??
#+allegro
(defun badguy(x) ;; this returns non-nil for single or double nans and infinities
  (excl::exceptional-floating-point-number-p x))