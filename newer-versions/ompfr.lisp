;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: mpfr; Base: 10 -*-
;; Author: Richard Fateman, Jan, 2006
;;

#| 

Rounding mode - can be set to
round to nearest,
round down, 
round toward zero,
round up
 or to the corresponding numeric
 value 0, 1, 2, or 3 
 Most Math::MPFR functions take as first argument the
    destination variable, as second and following arguments 
    the input variables, as last argument a rounding mode,
    and have a return value of type `int'. If this value
    is zero, it means that the value stored in the 
    destination variable is the exact result of the 
    corresponding mathematical function. If the
    returned value is positive (resp. negative), it means
    the value stored in the destination variable is greater
    (resp. lower) than the exact result.  For example with 
    the `GMP_RNDU' rounding mode, the returned value is 
    usually positive, except when the result is exact, in
    which case it is zero.  In the case of an infinite
    result, it is considered as inexact when it was
    obtained by overflow, and exact otherwise.  A
    NaN result (Not-a-Number) always corresponds to an
    inexact return value.

    |#

(defpackage :mpfr				;uses generic arithmetic
  (:use :ga :cl)
  (:shadowing-import-from  
   :ga    
   "+" "-" "/" "*" "expt"		;... n-ary arith
   "=" "/=" ">" "<" "<=" ">="		;... n-ary comparisons
   "1-" "1+" "abs" "incf" "decf"
   "min" "max"
   asin log tan atanh acos sin sinh cosh cos sqrt exp atan
   "tocl" "re-intern" 
   numerator denominator
   ) )

(eval-when '(load) (require "ga")(provide "mpfr"))

(in-package :mpfr)

(defvar *rndmode* 0) ;; mpfr::*rndmode* 


;;similar to stuff in mpf.lisp, but insert rounding modes all over.
;; the mpfr_struct has 4 items: precision sign exponent and pointer to "limbs".
;; looks like 4 ints.  actually, except for sign, unsigned longs would do.
;; also, entry point names, instead of being __gmpf_.... seem to be mpfr_...

(eval-when (compile load eval)
  (ff:def-foreign-type mpfr (cl::* :int))
  (ff:def-foreign-type mpf (cl::* :int))

  (ff:def-foreign-call
   (mpfr_init "mpfr_init")
   ((x (cl::* :int))) 
   :returning :int 
   :arg-checking nil  :call-direct t)
  (ff:def-foreign-call
   (mpfr_init2 "mpfr_init2")
   ((x (cl::* :int))
    (y  :int));; set precision to EXACTLY y bits.
   :returning :int 
   :arg-checking nil  :call-direct t)

  (ff:def-foreign-call
   (mpfr_set_default_prec "mpfr_set_default_prec")
   ((prec  :int));; set precision to at least y>0 bits for all future variables or const.
   :returning :void 
   :arg-checking nil  :call-direct t)
    
  (ff:def-foreign-call
   (mpfr_set_d "mpfr_set_d")
   ((target  mpf)
    (somedoub  :double)
    (rnd :int))
   :returning :int 
   :arg-checking nil  :call-direct t)
    
  (ff:def-foreign-call 
   (mpfr_set_str "mpfr_set_str")
      ;;changes value of mpf x. string is mmm@nnn  
      ;; the fraction is .mmm, and the exponent is nnn.  base is 
      ;; 2 to 36 or -36 to -2, where negative base means exponent
      ;; is in decimal.  returns 0 if string is valid else -1.
   ((x mpfr) 
    (s (cl::* :char))
    (base :int)
    (rnd :int)
    )		
   :strings-convert  t  :returning :int
   :arg-checking nil)
  
  (ff:def-foreign-call 
   (mpfr_get_str "mpfr_get_str")
   ((s :int)				;if nil, a result string will be allocated  sr
    (expptr (cl::* :int))		;? pointer to exponent
    (base :long) 
    (ndigits :int);;if 0, as many digits as significant
    (op mpf)
    (rnd :int)
    ) 
   :returning  ((cl::* :char)string )
   :arg-checking t :call-direct nil) ;??
     
  (ff:def-foreign-call 
   (mpfr_get_d "mpfr_get_d")
   ((op mpfr)
    (rnd :int))
   :returning  :double
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call;; remove gmp number from memory
   (mpfr_clear  "mpfr_clear") ((x mpf)) 
   :returning :int
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call;; remove string from gmp memory
   (mpfr_free_str  "mpfr_free_str") ((x (cl::* :char)) )
   :returning :int
   :arg-checking nil :call-direct nil) ;;? or :strings-convert nil??
  
  #+ignore
  (ff:def-foreign-call 
   (mpfr_sin  "mpfr_sin")
   ((target mpf)(op1 mpf) (rnd :int))
   :returning  :void
   :arg-checking nil :call-direct t)  ; re-done later
  
  (ff:def-foreign-call 
   (mpfr_add  "mpfr_add")
   ((target mpfr)(op1 mpfr) (op2 mpfr)(rnd :int))
   :returning  :void
   :arg-checking nil :call-direct t)
     (ff:def-foreign-call 
   (mpfr_mul  "mpfr_mul")
   ((target mpfr)(op1 mpfr) (op2 mpfr)(rnd :int))
   :returning  :void
   :arg-checking nil :call-direct t)
  (ff:def-foreign-call 
   (mpfr_sub  "mpfr_sub")
   ((target mpfr)(op1 mpfr) (op2 mpfr)(rnd :int))
   :returning  :void
   :arg-checking nil :call-direct t)
      
  (ff:def-foreign-call 
   (mpfr_div  "mpfr_div")
   ((target mpfr)(op1 mpfr) (op2 mpfr)(rnd :int))
   :returning  :void
   :arg-checking nil :call-direct t)

  ;; Function entries etc should all be in the gmp.h file
  ;; many more need to be added here..
  )

#|
/* Definition of the main structure */
typedef struct {
  mpfr_prec_t  _mpfr_prec;
  mpfr_sign_t  _mpfr_sign;
  mp_exp_t     _mpfr_exp;
  mp_limb_t   *_mpfr_d;
  } __mpfr_struct;

  umm.  it looks like the same as mpf except maybe the sign.  Here's gmpf:

typedef struct
{
  int _mp_prec;			/* Max precision, in number of `mp_limb_t's.
				   Set by mpf_init and modified by
				   mpf_set_prec.  The area pointed to by the
				   _mp_d field contains `prec' + 1 limbs.  */
  int _mp_size;			/* abs(_mp_size) is the number of limbs the
				   last field points to.  If _mp_size is
				   negative this is a negative number.  */
  mp_exp_t _mp_exp;		/* Exponent, in the base of `mp_limb_t'.  */
  mp_limb_t *_mp_d;		/* Pointer to the limbs.  */
} __mpf_struct;

|#

(defstruct mpfr f )
(defparameter gmpfrformat "~a0.~a*10^(~a)" )  ;; -0.123*10^(45)

(defun alloc-gmpfr()  ;; provides an empty gmpfr object for us to use, init to NaN
  (let ((inside (make-array 4 :element-type 
			     '(signed-byte 32) 
			     :initial-element 0)))
    (mpfr_init inside) 
    (excl:schedule-finalization inside 'mpfr_clear)
    (make-gmpfr :f inside)))


(defmethod print-object ((a gmpfr) stream)
  (let ((sign ""))
  (multiple-value-bind (frac expon)
      (create_string_from_mpfr a)
    (cond((eql (aref frac 0) #\-) ;; check for negative fraction
	  (setf frac (subseq frac 1))(setf sign "-")))
    (format stream gmpfrformat sign frac expon))))

(defvar stexpon  (make-array 1 :element-type '(signed-byte 32) :initial-element 0))

(defvar *mindigs* 0) ;; "all" digits

(defmethod create_string_from_mpfr((m gmpfr)) 
  ;; return 2 values, the fraction and the exponent, each as a string.
  (let ((r (mpfr_get_str 0 stexpon 10 *mindigs* ;; how many digits shown, depends.
			 (gmpfr-f m) 0))) ;rnd to nearest
    (excl:schedule-finalization r 'mpfr_free_str) ; put this in later.
    (values   r  (elt stexpon 0))))

(defun cs(x)(create_string_from_mpfr x))
;; stopgap method , prints as double always.
#+ignore
(defmethod print-object ((a gmpfr) stream)(format stream "~s"
						 (create_double_from_mpfr a)))

(defmethod create_double_from_mpfr((m gmpfr)) ;;  creates a double precision version of mpf
  (mpfr_get_d (gmpfr-f m) 0))

(defmethod lisp2gmpfr2((x string) (e string))
  (create_mpfr_from_string x e))				    

(defmethod create_mpfr_from_string((s string) (e string))  
  ;; s is a string like "123"
  ;; e is a string like "4"
  ;; produces   .123 X 10^4
  (let* ((r (alloc-gmpfr))
	 (inside (gmpfr-f r)))
    (mpfr_set_str inside 
		  (concatenate 'string s "@" e)
		  10;; base 10 number conversion
		  0)			;round
    r))
(defun l2g(x e)(lisp2gmpfr2 x e))

(defmethod lisp2gmpfr2 ((x integer) (e integer))
  (create_mpfr_from_string (format nil "~s" x) (format nil"~s" e)))


;; try (lisp2gmpfr2 314159 -5)

;; probably a better way to do this...
(defun create_mpfr_zero() (create_mpfr_from_string "0" "0"))

(defmacro defarithmetic (op pgm)
  (let ((two-arg
	 (intern (concatenate 'string "two-arg-" (symbol-name op))
		 :ga )) )
    `(progn
       ;; new defmethods for gmpfr. 
       (defmethod ,two-arg ((arg1 gmpfr) (arg2 gmpfr))
	 (let* ((r (create_mpfr_zero)) (in (gmpfr-f r)))
	   (,pgm in (gmpfr-f arg1)(gmpfr-f arg2) *rndmode*) r))
       
       (defmethod ,two-arg ((arg1 integer) (arg2 gmpfr))
	 (let* ((r (lisp2gmpfr arg1))(in (gmpfr-f r)))
	   (,pgm in in (gmpfr-f arg2) *rndmode*) r))
       
       (defmethod ,two-arg ((arg1 gmpfr) (arg2 integer))
	 (let* ((r (lisp2gmpfr arg2))(in (gmpfr-f r)))   
	   (,pgm in (gmpfr-f arg1) in  *rndmode*) r))
       
       (compile ',two-arg)
       (compile ',op)
       ',op)))

(defarithmetic + mpfr_add)
(defarithmetic - mpfr_sub)
(defarithmetic * mpfr_mul)
(defarithmetic / mpfr_div)

(defmacro r (op) ;;
    (let ((fun-name  (intern op :ga ))
	  (d-name (format nil "mpfr_~a" op))
	  (d-symb (intern (format nil "mpfr_~a" op))))
      `(progn
	 (ff:def-foreign-call
	     (,d-symb ,d-name)
	     ((target mpfr)(op1 mpfr)(rnd :int))
   :returning :int
   :arg-checking nil :call-direct t)
     (defmethod ,fun-name ((arg gmpfr))
	 (let* ((h (alloc-gmpfr)) (in (gmpfr-f h)))
	   (,d-symb in (gmpfr-f arg) *rndmode*) h ))
     (compile ',fun-name))))

;; I don't know how 
(r abs) 
(r sin )
(r cos )
(r tan )
(r exp )
;;(r log ) might be 2-arg
(r log10)
(r asin )
(r acos )
;;(r atan )might be 2-arg
(r sinh )
(r cosh )
(r tanh )
(r asinh)
(r acosh)
(r atanh)
(r sqrt )
(r neg) 
;;(r floor)
(r ceil)
;; extra
(r cot)
(r coth)
(r csc)
(r csch)
(r erfc)
(r exp2)
(r exp10)
(r gamma)
(r lngamma)
(r log2)
(r nexttozero)
(r nextabove)
(r nextbelow)
(r nexttoinf)
;;(r random) conflict. put in package
(r sec)
(r sech)
(r sqr)
(r sub1)
(r zeta)
(r zero_p)



(defmethod lisp2gmpfr ((x integer))
  (lisp2gmpfr2 x 0))

(defmethod lisp2gmpfr ((x rational))
  (/ (lisp2gmpfr (numerator x)) (denominator x)))

(defmethod lisp2gmpfr ((x double-float))
  (let* ((r (alloc-gmpfr))
	 (in (gmpfr-f r)))
    (mpfr_set_d in x *rndmode*)
    r))


;; could put in conversions from gmpf gmpz single double
;; what to do about rounding?  Maybe make rounding a global
;; variable *mpfr-rnd* and use that?

#| Let's say we want to do a few key vector operations without 
boxing and unboxing mpfr numbers or too many temps. For example, given a
sequence or list of them.  Could also use polynomial eval. Maybe
these are already written in C. |#

(defun sum-mpfr-list(L *gmpfr-rnd*)  ;; sum of items in a list
  (let* ((sum (create_mpfr_zero))
	 (u (gmpfr-f sum))) ; inside the box
    (loop for i in L do
	  (mpfr_add u u (gmpfr-f i) *gmpfr-rnd*))
    sum))

(defun prod-mpfr-list(L M *gmpfr-rnd*)  ;; sum of prods of items in a list
  (let* ((sum (create_mpfr_zero)) ; or copy a zero..
	 (u (gmpfr-f sum))
	 (temp (gmpfr-f (create_mpfr_zero)))) ;or copy a zero
    (mapcar #'(lambda (i j)
		(mpfr_mul temp (gmpfr-f i) (gmpfr-f j) *gmpfr-rnd*)
		(mpfr_add u u temp *gmpfr-rnd*) )
	    L M)
    sum))
;; we would use addmul but there isn't an mpfr version (yet?)

(defun set-prec(n) ;  mpfr::set-prec  changes default precision.
(assert (typep n 'fixnum))
   (mpfr_set_default_prec n))


;;; some tests
(defun time-sin(n)
  (let ((f1 (lisp2gmpfr 1)))
    (dotimes (i n)(declare (fixnum n))
		    (sin (the gmpfr f1)))))

(defun time-sin2(n)
  (let* ((f1 (lisp2gmpfr 1))
	 (ans (make-gmpfr))
	 (in (gmpfr-f f1))
	 (ina(gmpfr-f ans)))
      (declare (optimize speed)
	       (type (simple-array (signed-byte 32) (4)) in inat))
      ;; target is 1st
      (dotimes (i n ans)(declare (fixnum n))
	(mpfr_sin ina in 0))))
