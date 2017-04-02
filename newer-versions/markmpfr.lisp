;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: mpfr; Base: 10 -*-
;; Author: Richard Fateman, Jan, 2006
;; last edit, March 7, 2006
;;

;; TODO. Look at QD.  need to set up  polyeval, newton.

#| 

Rounding mode - can be set to
round to nearest,
round down, 
round toward zero,
round up
 by using the corresponding numeric
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
   sin cos tan 
   asin acos 
   atan log   ;; these are trickier, 1 or 2 args.
   ;; must add all these like
   sinh cosh tanh 
   asinh acosh atanh sqrt exp
   numerator denominator
   neg erf erfc
   scale-float
   ;; begin mfh
   floor
   ceiling
   ;; end mfh
   ))

(eval-when '(load) (require "ga")(provide "mpfr"))

(in-package :mpfr)



;; similar to stuff in mpf.lisp, but insert rounding modes all over.
;; the mpfr_struct has 4 items: precision, sign, exponent, and pointer
;; to "limbs".  looks like 4 ints.  actually, except for sign,
;; unsigned longs would do.  also, entry point names, instead of being
;; __gmpf_.... seem to be mpfr_...

(eval-when (compile load eval)
  (defvar *rndmode* 0) ;; mpfr::*rndmode* 
  (ff:def-foreign-type mpfr (:array :int))
  (ff:def-foreign-type mpf (:array :int))

  (ff:def-foreign-call
   (mpfr_init "mpfr_init")
   ((x mpfr (simple-array (unsigned-byte 32)(4))))
   :returning :int 
   :arg-checking nil  :call-direct t)
  
  (ff:def-foreign-call
   (mpfr_init2 "mpfr_init2")
   ((x mpfr (simple-array (unsigned-byte 32)(4)))
    (y  :int));; set precision to EXACTLY y bits.
   :returning :int 
   :arg-checking nil  :call-direct t)
  
    (ff:def-foreign-call
   (mpfr_init_set_si "mpfr_init_set_si")
	((x mpfr (simple-array (unsigned-byte 32)(4)))
	 (y :long) ;; set to this signed long int
	 (rnd :int))
   :returning :int 
   :arg-checking nil  :call-direct t)
    
  (ff:def-foreign-call
   (mpfr_set_default_prec "mpfr_set_default_prec")
   ((prec  :int));; set precision to exactly y>0 bits for all future variables or const.
   :returning :void 
   :arg-checking nil  :call-direct t)
  
  (ff:def-foreign-call
   (mpfr_get_default_prec "mpfr_get_default_prec")
   (); no args
   :returning :int
   :arg-checking nil :strings-convert nil)
    
  (ff:def-foreign-call
   (mpfr_set_d "mpfr_set_d")
   ((x mpfr (simple-array (unsigned-byte 32)(4)))
    (somedoub  :double)
    (rnd :int))
   :returning :int 
   :arg-checking nil  :call-direct t)
  
    (ff:def-foreign-call
   (mpfr_set_si "mpfr_set_si")
   ((x mpfr (simple-array (unsigned-byte 32)(4)))
    (someint  :int)
    (rnd :int))
   :returning :int 
   :arg-checking nil  :call-direct t)
    
  (ff:def-foreign-call 
   (mpfr_set_str "mpfr_set_str")
      ;;changes value of mpf x. string is mmm@nnn  
      ;; the fraction is .mmm, and the exponent is nnn.  base is 
      ;; 2 to 36 or -36 to -2, where negative base means exponent
      ;; is in decimal.  returns 0 if string is valid else -1.
   ((x mpfr (simple-array (unsigned-byte 32)(4)))
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
    ;; mfh: is this right? should it be an mpf or an mpfr?
    (op mpf)
    (rnd :int)
    ) 
   :returning  ((cl::* :char)string )
   :arg-checking t :call-direct nil)	;??
     
  (ff:def-foreign-call 
   (mpfr_get_d "mpfr_get_d")
   ((x mpfr (simple-array (unsigned-byte 32)(4)))
    (rnd :int))
   :returning  :double
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call;; remove gmp number from memory
   (mpfr_clear  "mpfr_clear")
   ((x mpfr (simple-array (unsigned-byte 32)(4))))
   :returning :int
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call;; remove string from gmp memory
   (mpfr_free_str  "mpfr_free_str")          
   ((x mpfr (simple-array (unsigned-byte 32)(4))))
   :returning :int
   :arg-checking nil :call-direct nil);;? or :strings-convert nil??
  
  #+ignore
  (ff:def-foreign-call 
   (mpfr_sin  "mpfr_sin")
   ((target mpf)(op1 mpf) (rnd :int))
   :returning  :void
   :arg-checking nil :call-direct t)	; re-done later
  
  
   (ff:def-foreign-call
   (mpfr_mul_2si "mpfr_mul_2si") ;; return x*2^power
	  ((targ mpfr (simple-array (unsigned-byte 32)(4)))
	   (x mpfr (simple-array (unsigned-byte 32)(4)))
	   (power  :long)
	   (rnd :int))
   :returning :void 
   :arg-checking nil  :call-direct t)
      
      
  (ff:def-foreign-call 
   (mpfr_add_si  "mpfr_add_si")
   ((target mpfr (simple-array (unsigned-byte 32)(4)))
    (op1 mpfr (simple-array (unsigned-byte 32)(4)))
    (op2 :long)
    (rnd :int))
   :returning  :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpfr_set  "mpfr_set")
   ((target mpfr (simple-array (unsigned-byte 32)(4)))
    (op1 mpfr (simple-array (unsigned-byte 32)(4)))
    (rnd :int))
   :returning  :void
   :arg-checking nil :call-direct t)
  
   (ff:def-foreign-call 
   (mpfr_sin_cos  "mpfr_sin_cos")
       ((target1 mpfr (simple-array (unsigned-byte 32)(4)))
	(target2 mpfr (simple-array (unsigned-byte 32)(4)))
    (op1 mpfr (simple-array (unsigned-byte 32)(4)))
    (rnd :int))
   :returning  :void
   :arg-checking nil :call-direct t)
   
  (ff:def-foreign-call
   (mpfr_get_si "mpfr_get_si")  ;; get a signed long, if possible.
	 ((x  mpfr (simple-array (unsigned-byte 32)(4)))
	  (rnd :int))
   :returning :int 
   :arg-checking nil  :call-direct t)
  
  
  (ff:def-foreign-call
   (mpfr_get_z "mpfr_get_z")  ;; get a signed long, if possible.
	((targ  mpfr (simple-array (unsigned-byte 32)(3))) ;a gmpz.
	 (x  mpfr (simple-array (unsigned-byte 32)(4)))
	  (rnd :int))
   :returning :void 
   :arg-checking nil  :call-direct t)

  (ff:def-foreign-call
      (mpfr_nan_p "mpfr_nan_p") 
      ((x mpfr (simple-array (unsigned-byte 32)(4))))
    :returning :int
    :arg-checking nil :call-direct t)

  (ff:def-foreign-call
      (mpfr_inf_p "mpfr_inf_p") 
      ((x mpfr (simple-array (unsigned-byte 32)(4))))
    :returning :int
    :arg-checking nil :call-direct t)

  ;; zero_p is defined via (r zero_p) below.

  (ff:def-foreign-call
      (mpfr_floor "mpfr_floor")
      ((dest mpfr (simple-array (unsigned-byte 32) (4)))
       (src mpfr (simple-array (unsigned-byte 32) (4))))
    :returning :int
    :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call
      (mpfr_ceil "mpfr_ceil")
      ((dest mpfr (simple-array (unsigned-byte 32) (4)))
       (src mpfr (simple-array (unsigned-byte 32) (4))))
    :returning :int
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
(eval-when (compile load eval)
(defstruct gmpfr  (f  (make-array 4 :element-type '(signed-byte 32) :initial-element 0)
		      :type (simple-array  (signed-byte 32)(4))))

(defparameter mpfrformat "~a0.~a*10^(~a)" )  ;; -0.123*10^(45)

(defmethod make-load-form ((a gmpfr)&optional environment)
  (declare (ignore environment))
  (let ((q (ga::outof a)))
    `(into ,q)))

(defun alloc-mpfr()  ;; provides an empty mpfr object for us to use, init to NaN
  (let* ((ans (make-gmpfr))
	 (inside (gmpfr-f ans)))
    ;; set the inside to NaN
    (mpfr_init inside) 
    ;; indicate that when the gmpfr shell is unused, call mpfr_clear on the inside.
    (excl:schedule-finalization inside 'mpfr_clear)
    ans
    ))

(defmethod print-object ((a gmpfr) stream)
  (let ((sign ""))
  (multiple-value-bind (frac expon)
      (create_string_from_mpfr a)
    (cond((eql (aref frac 0) #\-) ;; check for negative fraction
	  (setf frac (subseq frac 1))(setf sign "-")))
    (format stream mpfrformat sign 
	    (string-right-trim "0" frac) ;remove [excess?] trailing zeros
	    expon))))

(defvar stexpon  (make-array 1 :element-type '(signed-byte 32) :initial-element 0))

(defvar *mindigs* 0) ;; "all" digits

(defmethod create_string_from_mpfr((m gmpfr)) 
  ;; return 2 values, the fraction and the exponent, each as a string.

  (let ((r (mpfr_get_str 0 stexpon 10 *mindigs* ;; how many digits shown, depends.
			 (gmpfr-f m) 0))) ;rnd to nearest
    (excl:schedule-finalization r 'mpfr_free_str) 
    (values   r  (elt stexpon 0))))

(defmethod ga::outof((m gmpfr)) 
  ;; using base 32 conversion  takes an mpfr number and changes to lisp int or ratio!	
  ;; maybe not as fast as, say, tearing the number apart,
  ;; limb from limb, but how often are we going to do this?
  (let ((r (mpfr_get_str 0 stexpon 32 0  (gmpfr-f m) 0))) ;rnd to nearest
    (excl:schedule-finalization r 'mpfr_free_str) 
    (setf r (string-right-trim "0" r))
    (if (string= r "") 0  ;; can't feed parse-integer an empty string.
      (let* ((f (parse-integer r :radix 32))
	     (e (cl::- (elt stexpon 0) (length r))))
	(if (cl::< f 0)(cl::incf e))
	(cl::* f (cl::expt 32 e))))))

(defun cs(x)(create_string_from_mpfr x))

#+ignore ;;just for debugging; convert to regular double.
(defmethod print-object ((a gmpfr) stream)(format stream "~s"
						 (create_double_from_mpfr a)))

(defmethod create_double_from_mpfr((m gmpfr)) ;;  creates a double precision version of mpf
  (mpfr_get_d (gmpfr-f m) 0))

;; To convert from an ordinary lisp "real" number, use the "into" function.

;; Note that this is mpfr::into, distinct from programs in other packages.
;; The optional 2nd arg says we already have a place
;; for this number, so in that case we don't need to allocate a new one.

;;; USE THIS FUNCTION,  into

(defun into(r &optional (where (alloc-mpfr)))
  (lisp2mpfr r where))
;;;**********************

(defmethod lisp2mpfr2((x string) (e string) (where gmpfr)) ;fraction and exponent
  (create_mpfr_from_string x e where ))				    

(defmethod create_mpfr_from_string((s string) (e string)  (where gmpfr))
  ;; s is a string like "123"
  ;; e is a string like "4"
  ;; produces   .123 X 10^4
  (let* ((inside (gmpfr-f where)))
    (mpfr_set_str inside 
		  (concatenate 'string s "@" e)
		  10;; base 10 number conversion
		  0)			;round
    where))

(defun l2g(x e &optional(where (alloc-mpfr)))  ;; e.g. (l2g "3" "-10")  or (l2g -3 -10)
  (lisp2mpfr2 x e where))


(defmethod lisp2mpfr2 ((x integer) (e integer) (where gmpfr))
  (create_mpfr_from_string (format nil "~s" x) (format nil"~s" e) where))



;; probably a better way to do this...
(defun create_mpfr_zero()
  (let ((ans (alloc-mpfr)))
    (mpfr_set_si (gmpfr-f ans) 0 *rndmode*)
    ans))

  
(defmacro defarithmetic (op pgm)
  (let ((two-arg
	 (intern (concatenate 'string "two-arg-" (symbol-name op))
		 :ga ))
	(c-entry (format nil "~a" pgm)))
    `(progn

       (ff:def-foreign-call 
	(,pgm ,c-entry)
	((target mpfr (simple-array (unsigned-byte 32)(4)))
	 (op1 mpfr (simple-array (unsigned-byte 32)(4)))
	 (op2 mpfr (simple-array (unsigned-byte 32)(4)))
	 (rnd :int))
	:returning  :void
	:arg-checking nil :call-direct t)
       
       ;; new defmethods for gmpfr. 
       (defmethod ,two-arg ((arg1 gmpfr) (arg2 gmpfr))
	 (let* ((r (create_mpfr_zero)) (in (gmpfr-f r)))
	   (,pgm in (gmpfr-f arg1)(gmpfr-f arg2) *rndmode*)
	   r))
       
       (defmethod ,two-arg ((arg1 number) (arg2 gmpfr))
	 (let* ((r (into arg1))(in (gmpfr-f r)))
	   (,pgm in in (gmpfr-f arg2) *rndmode*) r))
       
       (defmethod ,two-arg ((arg1 gmpfr) (arg2 number))
	 (let* ((r (into arg2))(in (gmpfr-f r)))   
	   (,pgm in (gmpfr-f arg1) in  *rndmode*) r))
     ;;  (compile ',two-arg)
       ;;  (compile ',op)
       (setf (get ',op 'argnum) 2)	;used by with-temps, dsetv
       (setf (get ',op 'mpfr-program) ',pgm) ;used by with-temps, dsetv
       (setf (get ',two-arg 'mpfr-program) ',pgm) ;used after macroexpand-all
       (setf (get ',two-arg 'argnum) 2)

       ',op)))

(defarithmetic + mpfr_add)
(defarithmetic - mpfr_sub)
(defarithmetic * mpfr_mul)
(defarithmetic / mpfr_div)

(defmethod ga::two-arg-expt ((base gmpfr)(n integer))
  (let ((ans (alloc-mpfr)))
    (mpfr_npwr (gmpfr-f base) n (gmpfr-f ans))
    ans)) ;; this special case doesn't fit into defarithmetic macro.

(setf (get 'ga::two-arg-expt 'mpfr-program) 'mpfr_pow) ;; for mpfr numbers.
(setf (get 'ga::two-arg-expt 'argnum) 2)
    
(defmethod ga::two-arg-expt ((base gmpfr) (n gmpfr))
  (exp (* n (log base)))) ;;; change this

(defmethod ga::two-arg-expt ((base gmpfr) (n real))
  (exp (* (into n) (log base))))

(defmethod ga::two-arg-expt ((base real) (n gmpfr))
  (exp (* n (log (into base)))))

;; begin mfh
(defmethod ga::two-arg-floor ((num gmpfr) (div gmpfr))
  (let ((result (alloc-mpfr))
	(remainder (alloc-mpfr)))
    ;; We have to instruct MPFR to round down.
    ;; This should work for negative numbers too:  (floor -1.1) is -2.
    (progn
      (mpfr_div (gmpfr-f result) (gmpfr-f num) (gmpfr-r div) 3)
      ;; MPFR manual (see www.mpfr.org) says you can use the same variable
      ;; for both input and output.  Store the floor of num/div in result.
      (mpfr_floor (gmpfr-f result) (gmpfr-f result))
      ;; remainder = num - result*div, even if div != 1.
      (warn "ga::two-arg-floor: what rounding mode should we use for mpfr_mul and mpfr_sub?")
      (mpfr_mul (gmpfr-f remainder) (gmpfr-f result) (gmpfr-f div) *rndmode*)
      (mpfr_sub (gmpfr-f remainder) (gmpfr-f num) (gmpfr-f remainder) *rndmode*)
      (values result remainder)
      )))

;;; mfh: I'm not sure if this is right, because mpfr_floor alone isn't
;;; enough to calculate floor of an MPFR number, as CL understands the
;;; floor function (taking two inputs and returning two values).
(setf (get 'ga::two-arg-floor 'mpfr-program) 'mpfr_floor)
(setf (get 'ga::two-arg-floor 'argnum)  2)

(defmethod ga::one-arg-floor ((num gmpfr))
  (let ((result (alloc-mpfr))
	(remainder (alloc-mpfr)))
    (progn
      (mpfr_floor (gmpfr-f result) (gmpfr-f num))
      ;; remainder = num - result.
      ;; FIXME: What rounding mode should we use?
      (warn "ga::one-arg-floor: what rounding mode should we use for mpfr_sub?")
      (mpfr_sub (gmpfr-f remainder) (gmpfr-f num) (gmpfr-f result) *rndmode*)
      (values result remainder))))

(defmethod ga::two-arg-ceiling ((num gmpfr) (div gmpfr))
  (let ((result (alloc-mpfr))
	(remainder (alloc-mpfr)))
    ;; We have to instruct MPFR to round up.
    ;; This should work for negative numbers too:  (ceiling -1.1) is -1.
    (progn
      (mpfr_div (gmpfr-f result) (gmpfr-f num) (gmpfr-r div) 2)
      ;; MPFR manual (see www.mpfr.org) says you can use the same variable
      ;; for both input and output.  Store the ceiling of num/div in result.
      (mpfr_ceil (gmpfr-f result) (gmpfr-f result))
      ;; remainder = num - result*div, even if div != 1.
      ;; FIXME: What rounding mmode should we use?
      (warn "ga::two-arg-ceiling: what rounding mode should we use for mpfr_mul and mpfr_sub?")
      (mpfr_mul (gmpfr-f remainder) (gmpfr-f result) (gmpfr-f div) *rndmode*)
      (mpfr_sub (gmpfr-f remainder) (gmpfr-f num) (gmpfr-f remainder) *rndmode*)
      (values result remainder)
      )))

;;; mfh: I'm not sure if this is right, because mpfr_ceil alone isn't
;;; enough to calculate ceiling of an MPFR number, as CL understands the
;;; ceiling function (taking two inputs and returning two values).
(setf (get 'ga::two-arg-ceiling 'mpfr-program) 'mpfr_ceil)
(setf (get 'ga::two-arg-ceiling 'argnum)  2)

(defmethod ga::one-arg-ceiling ((num gmpfr))
  (let ((result (alloc-mpfr))
	(remainder (alloc-mpfr)))
    (progn
      (mpfr_ceil (gmpfr-f result) (gmpfr-f num))
      ;; remainder = num - result.
      (warn "ga::one-arg-ceiling: what rounding mode should we use for mpfr_sub?")
      (mpfr_sub (gmpfr-f remainder) (gmpfr-f num) (gmpfr-f result) *rndmode*)
      (values result remainder))))
;; end mfh

(defmacro r (op) ;;
    (let ((fun-name  (intern op :ga ))
	  (d-name (format nil "mpfr_~a" op))
	  (d-symb (intern (format nil "mpfr_~a" op))))
      `(progn
	 (ff:def-foreign-call
	  (,d-symb ,d-name)
	     ((target mpfr (simple-array (unsigned-byte 32)(4)))
	      (op1 mpfr (simple-array (unsigned-byte 32)(4)))
	      (rnd :int))
	   :returning :int
	   :arg-checking nil :call-direct t)
     (defmethod ,fun-name ((arg gmpfr))
	 (let* ((h (alloc-mpfr)) (in (gmpfr-f h)))
	   (,d-symb in (gmpfr-f arg) *rndmode*) h ))
     ;; (compile ',fun-name)
            (setf (get ',fun-name 'argnum) 1)
       (setf (get ',fun-name 'mpfr-program) ',d-symb)

     )))


(r abs ) 
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
;(r floor) 
;(r ceil)
;; extra
(r cot)
(r coth)
(r csc)
(r csch)
(r erf)
(r erfc)
(r exp2)
(r exp10)
(r gamma)
(r lngamma)
(r log2)
(r log1p)
(r nexttozero)
(r nextabove)
(r nextbelow)
(r nexttoinf)
;;(r random) conflict. put in package
(r sec)
(r sech)
(r sqr)
(r sub1)
(r add1)
(r zeta)
(r zero_p)
(r trunc )


(defmethod lisp2mpfr ((x integer) (ans gmpfr))
  (lisp2mpfr2 x 0 ans))

(defmethod lisp2mpfr ((x fixnum) (ans gmpfr))
  (mpfr_set_si (gmpfr-f ans) x *rndmode*)
  ans)

(defmethod lisp2mpfr ((x rational) (ans gmpfr))
  (let ((n (into (cl:numerator x) ans))
	(d (into (cl:denominator x) (alloc-mpfr))))
    (mpfr_div (gmpfr-f ans)(gmpfr-f n)(gmpfr-f d) *rndmode*)
    ans))

(defmethod lisp2mpfr ((x double-float) (r gmpfr))
  (let* ((in (gmpfr-f r)))
    (mpfr_set_d in x *rndmode*)
    r))
(defmethod lisp2mpfr ((x single-float) (r gmpfr))
  (lisp2mpfr (coerce x 'double-float) r))

(defmethod lisp2mpfr ((x gmpfr) (r gmpfr))
  (mpfr_set (gmpfr-f r)(gmpfr-f x) 0) r)

(defun copy (x)
  (let ((r (alloc-mpfr)))			; x had better be gmpfr
    (mpfr_set (gmpfr-f r)(gmpfr-f x) 0)
    r))

 

;; could put in conversions from mpf gmpz single double
;; what to do about rounding?  Maybe make rounding a global
;; variable *mpfr-rnd* and use that?

#| Let's say we want to do a few key vector operations without 
boxing and unboxing gmpfr numbers or too many temps. For example, given a
sequence or list of them.  Could also use polynomial eval. Maybe
these are already written in C. |#

(defun sum-mpfr-list(L *mpfr-rnd*)  ;; sum of items in a list
  (let* ((sum (create_mpfr_zero))
	 (u (gmpfr-f sum))) ; inside the box
    (loop for i in L do
	  (mpfr_add u u (gmpfr-f i) *mpfr-rnd*))
    sum))

(defun prod-mpfr-list(L M *mpfr-rnd*)  ;; sum of prods of items in a list
  (let* ((sum (create_mpfr_zero)) ; or copy a zero..
	 (u (gmpfr-f sum))
	 (temp (gmpfr-f (create_mpfr_zero)))) ;or copy a zero
    (mapcar #'(lambda (i j)
		(mpfr_mul temp (gmpfr-f i) (gmpfr-f j) *mpfr-rnd*)
		(mpfr_add u u temp *mpfr-rnd*) )
	    L M)
    sum))
;; we would use addmul but there isn't an mpfr version (yet?)

(defun set-prec(n) ;  mpfr::set-prec  changes default precision.Returns old
 (assert (typep n 'fixnum))
 (prog1 (mpfr_get_default_prec)
  (mpfr_set_default_prec n)))

;; to find out the current precision, call  (mpfr_get_default_prec)
(defun get-prec() 
(mpfr_get_default_prec))

(defmacro defcomparison (op pgm)
  (let ((two-arg (intern (concatenate 'string "two-arg-" 
				      (symbol-name op))    :ga ))      )
    `(progn
       (ff:def-foreign-call
	  (,pgm ,(format nil "~a" pgm))
	     ((op1 mpfr (simple-array (unsigned-byte 32)(4)))
	      (op2 mpfr (simple-array (unsigned-byte 32)(4))))
	   :returning :int ;; non-zero if the relation is true
	   :arg-checking nil :call-direct t)
       
       (defmethod ,two-arg ((arg1 gmpfr) (arg2 gmpfr)) (cl:/= 0(,pgm (gmpfr-f arg1)(gmpfr-f arg2))))
       (defmethod ,two-arg ((arg1 real) (arg2 gmpfr)) (cl:/= 0(,pgm (gmpfr-f (into arg1))(gmpfr-f arg2))))
       (defmethod ,two-arg ((arg1 gmpfr) (arg2 real)) (cl:/= 0(,pgm(gmpfr-f arg1) (gmpfr-f (into arg2)))))
      ',op)))

(defcomparison < mpfr_less_p)
(defcomparison <= mpfr_lessequal_p)
(defcomparison >= mpfr_greaterequal_p)
(defcomparison > mpfr_greater_p)
(defcomparison = mpfr_equal_p)
(defcomparison unordered mpfr_unordered_p)

;; still to do...
;;mpfr_pow
;;mpfr_pow_si				;signed integer
;;mpfr_pow_z				;gmp bignum?

(defmethod addmpfr_f ((a gmpfr) (x fixnum))   ;;mpfr+fixnum, 
  (let* ((targ (alloc-mpfr))
	 (in (gmpfr-f targ)))
    (mpfr_add_si in (gmpfr-f a) x  *rndmode*) targ))

(defmethod ga::1+((a gmpfr))(addmpfr_f a 1))
          
(defmethod ga::1-((a gmpfr))(addmpfr_f a -1))

;;; some tests
(defun time-sin(n)
  (let ((f1 (into 1)))
    (dotimes (i n)(declare (fixnum n))
		    (sin (the gmpfr f1)))))

(defun time-sin2(n)
  (let* ((f1 (into 1))
	 (ans (into 0))
	 (in (gmpfr-f f1))
	 (ina(gmpfr-f ans)))
      (declare (optimize speed)
	       (type (simple-array (signed-byte 32) (4)) in inat))
      ;; target is 1st
      (dotimes (i n ans)(declare (fixnum n))
	(mpfr_sin ina in 0))))

;; maybe all of these: oneroot, deriv, polyeval, polyroot  should be in ga.lisp?
;; but then what arithmetic to use?
;; these do not use COMPLEX arithmetic. To find all zeros of polynomials,
;; we need the complex ones, too.

;; oneroot is going to be OK anywhere. Probably deriv too.  They don't
;; do a lot of arithmetic directly.

;; polyeval could figure out the type of the input, but that doesn't really
;; provide a definite indication of the precision for the computation.

;; So specializations might still exist.. e.g. evaluate this double-precision poly
;; to get an mpfr answer..
)
(defun oneroot (f df x threshold iters &aux fval)
  (dotimes  (i iters (error "rootfinder failed to converge. Residual at ~s is ~s after ~s iterations." 
			    x fval i))
    (setf fval (funcall f x))
    (if (< (abs fval) threshold) 
	(return (values x fval i))		;return x, residual and iteration count
      (decf x (/ fval (funcall df x))))))


(defun deriv(coefs) ;given coefs of a polynomial. return coefs of derivative.
  (let ((ans nil) (i 0))
    (dolist (c (cdr (reverse coefs)) ans) 
      (push (* (incf i) c) ans))  ans))

;; slow generic version
(defun polyeval (alist x)  ;; 
      (let ((sum (into 0)))
      (dolist (i alist sum)
	(setf sum  (+ i (* x sum))))))

;; perhaps default threshold should be a function of  precision 

(defun polyrootmpfr (coefs x &optional (threshold 1.0d-15) (iters 20))
  (setf coefs (mapcar #'into coefs))
  (let ((dp (deriv coefs)))
    (oneroot  #'(lambda(x)(polyeval coefs x))
	      #'(lambda(x)(polyeval dp x))
	      (into x)
	      (into threshold)
	      iters)))

;; try, for example, (polyrootmpfr '(1 0 -1) (into 0.5))


;;;todo... with-temps and dsetv.


(defmacro dsetv (targ ex)
  ;; try  (dsetv a (+ b c)) 
  ;; should be faster than (setf a (+ b c)). maybe 2X.
  ;; All the logic below is done during macro-expansion,
  ;; which means it is usually done at compile time. Run time
  ;; is therefore not penalized.  If you use dsetv from an interpreted
  ;; program it will be slow, however, because it will do the macro
  ;; expansion followed by the execution, each time it is used.
  (setf ex (macroexpand ex))
  (cond 
   ((atom ex) `(into ,ex ,targ))
   ((eq (car ex) 'into) `(into ,@(cdr ex)  ,targ))
   ((eq (car ex) 'setq) 
    (let ((gg (gensym))) ;; need to protect against capturing z in (setq z ..))
    `(let ((,gg  ,(with-temps (caddr ex))))
        (mpfr_set
	;; the target
	(gmpfr-f ,(cadr ex))
	;; the value
	(gmpfr-f ,gg)
	*rndmode*)
       ,gg)))
   (t 
    (let* ((op (car ex))
	   (args (cdr ex))
	   (the-op (get op 'mpfr-program))
	   (argnum (get op 'argnum)))
      (cond 	       
       ((not the-op);; not a previously listed op
	`
	   (let* ((lval ,targ)
		  (a1 (gmpfr-f  (,op ,@ args)))
		  (tt (gmpfr-f lval)))
	     (mpfr_set tt a1 *rndmode*) 
	     lval))
       ((not (eql argnum (length args))) 
	(error "dsetv was given operator ~s which expects ~s args, but was given ~s --  ~s" 
	       op argnum (length args) args))
       (t
	(case argnum
	  (1;; one argument.
	   `(let ((a1 (gmpfr-f ,(macroexpand `(with-temps ,(car args)))))
		    (tt (gmpfr-f ,targ)))
		(,the-op  tt a1 *rndmode*)
	      ,targ))
	  (2
	   `(let ((a1 (gmpfr-f ,(macroexpand `(with-temps ,(car args)))))
		   ;(a1 (gmpfr-f  ,(car args)))
		    (a2 (gmpfr-f ,(macroexpand `(with-temps ,(cadr args)))))
		  ;;  (a2 (gmpfr-f  ,(cadr args)))
		    (tt (gmpfr-f ,targ)))
		(,the-op tt a1 a2 *rndmode*)
		,targ
		))
	  (otherwise (error "argnum is wrong for op ~s " op))
	  )))))))


(defmacro with-temps(expr) ;; unchanged code from qd.
  (let ((*names* nil)
	(*howmany* 0))
    (labels ((genlist(n)(loop for i from 1 to n collect (into i))) ;make a list of fresh mpfr items
	     (ct1 (r) ;; count temporaries needed
	       (cond ((numberp r) (incf *howmany*))
		     ((not (consp r)) r)
		     (t (incf *howmany*)
			(mapc #'ct1 (cdr r)))))
		
	   (maketemps(r) ;change r=(+ a (* b c)) to  temp storage .
		     (cond ((numberp r) (into r))
			   ((atom r) r)
			   ((get (car r) 'argnum); known operator
			    `(dsetv ,(pop *names*)
				    ,(cons (car r)(mapcar #'maketemps (cdr r)))))
			   ;; just a symbol name? maybe aref? better be the right type, gmpfr
			   (t  r))))
      (setf expr (macroexpand expr))
       (ct1 expr)
     ;; (ct1 expr); count the temporaries
    (setf *names* (genlist *howmany*))
    (maketemps expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;FFT!!!;;;;;;;;;;;;;;;;;

;;(eval-when (compile load eval)
  ;; mostly copied from qd.lisp
  
(defmacro newzero() ;; maybe use mpfr_init_set_si?? now missing?
  `(create_mpfr_zero))

(defun v2dfa(a &optional (m (length a))	)
  ;;coerce a vector of mpfr numbers (or lisp numbers) of length m to a
  ;; mpfr array of length 2m, since here complex numbers are stored in 2
  ;; adjacent locations.
  (let* ((k (length a))
	 (ans (make-array (cl::* 2 m) :allocation :lispstatic-reclaimable ))
	 (h nil))
    (declare (fixnum k))
    (dotimes (i k)
      (declare (fixnum i))
      (setf (aref ans (cl::* 2 i)) 
	 (if (gmpfr-p (setf h (aref a i)))(copy h) (into h))) ;; here we convert.
      (setf (aref ans (cl::1+ (cl::* 2 i))) (newzero)))
    (loop for i fixnum from (cl::* 2 k) to (cl::1-(cl::* 2 m)) do 
	  (setf  (aref ans i) (newzero)))
    ans))

;; (v2dfa #(30 40 50) 4)
;;  --> #(0.3Q2 0.Q0 0.4Q2 0.Q0 0.5Q2 0.Q0 0.Q0 0.Q0)

(defparameter *zz* (into 0))
(defparameter *one* (into 1))

(defun dfa2v(a &optional (m (/ (length a)2)))
  ;; Coerce real parts back to integers, more or less.  a is an an
  ;; array of even length.  If you know that there are trailing zeros,
  ;; set the actual length with the optional second parameter m.
  ;; also divides each element by half the length of m; part of Inverse FFT
  (let* ((k (/ (length a) 2))
	 (ans (make-array m :allocation :lispstatic-reclaimable)))
    (declare (fixnum k))
    (dotimes (i m ans)
      (declare (fixnum i))
      (setf (aref ans i)(round (ga::outof (aref a (cl::* 2 i))) k)))))

;;(dfa2v  (v2dfa #(256 256 256) 4))
;; -->  #(64 64 64 0)  is correct.


(defun mpfr::polymultfft(r s)
  ;;compute the size Z of the answer. Z is a power of 2.
  ;; compute complex array r, increased to size S
  ;; compute complex array r, increased to size S
  ;; compute two FFTs
  ;; multiply pointwise
  ;; compute inverse FFT  * 1/n
  ;; convert back to array and return answer
  (let* ((lr (length r))
	 (ls (length s))
	 (lans (+ lr ls -1))
	 (z (ash 1 (ceiling (log lans 2)))) ; round up to power of 2
	 (rfft (four1 (v2dfa r z) z))
	 (sfft (four1 (v2dfa s z) z))
	 (ans (make-array (* 2 z) :allocation :lispstatic-reclaimable)))
    (dotimes (i (* 2 z))(setf (aref ans i) (newzero)))
    (prodarray rfft sfft z ans)
    (dfa2v(four1 ans z :isign -1) lans)))

(defun prodarray(r s len ans)
  ;; r and s are the same length arrays
  ;; compute, for i=0, 2, ..., len-2
  ;; ans[i]:=  r[i]*s[i]-r[i+1]*s[i+1] ;; real part
  ;; ans[i+1]:=r[i]*s[i+1]+s[i]*r[i+1] ;; imag part
  (declare (fixnum len))
  (let ()
    ;;((ans (make-array (* 2 len))))
  (dotimes (i len ans)
    (let* ((ind (* 2 i))
	   (ind1 (1+ ind))
	   (a (aref r ind))
	   (b (aref r ind1))
	   (c (aref s ind))
	   (d (aref s ind1)))
      (declare ;(type aqd a b c d)
       (fixnum i ind ind1))
      (setf (aref ans ind)(copy(with-temps (- (* a c)(* b d)))))
      (setf (aref ans ind1)(copy(with-temps (+ (* a d)(* b c)))))))))


#+ignore

(defun prodarray(r s len ans)
  ;; r and s are the same length arrays
  ;; compute, for i=0, 2, ..., len-2
  ;; ans[i]:=  r[i]*s[i]-r[i+1]*s[i+1] ;; real part
  ;; ans[i+1]:=r[i]*s[i+1]+s[i]*r[i+1] ;; imag part
  (declare (fixnum len))
  (let ()
    ;;((ans (make-array (* 2 len))))
  (dotimes (i len ans)
    (let* ((ind (* 2 i))
	   (ind1 (1+ ind))
	   (a (aref r ind))
	   (b (aref r ind1))
	   (c (aref s ind))
	   (d (aref s ind1)))
      (declare ;(type aqd a b c d)
       (fixnum i ind ind1))
      (setf (aref ans ind)(- (* a c)(* b d)))
      (setf (aref ans ind1) (+ (* a d)(* b c)))))))

(defparameter onehalf (/ (into 1) (into 2)))

;;; this is a fairly generic FFT that works, but not optimized much.
;;; we leave it here just in case you want to copy it for other 
;;; generic arithmetic packages

;; This works, but see next version for somewhat faster.
#+ignore
(defun four1 (data nn &key (isign 1))
  (declare (type fixnum nn isign))
 (prog ((wr (newzero)) 
	(wi (newzero)) 
	(wpr (newzero))
	(wpi (newzero))
	(wtemp (newzero)) 
        (theta (newzero)) 
	(tempr (newzero)) 
	(tempi (newzero))
	(twopi (* 4 (asin *one*))) ; should be a pre-stored 2*pi somewhere. use current prec.
	(j 0) (n 0) (m 0) (mmax 0) (istep 0))
   (declare
	;;(type aqd wr wi wpr wpi wtemp theta tempr tempi)
    (fixnum j n m mmax istep))
  (setf n (* 2 nn)) 
  (setf j 1) 
  (do ((i 1 (+ i 2)))
      ((> i n) t)
      (declare (fixnum i))
    (when (> j i) 
     (setf tempr (aref data (1- j)))
     (setf tempi (aref data j)) 
     (setf (aref data (1- j)) (aref data (1- i)))
     (setf (aref data j) (aref data i)) 
     (setf (aref data (1- i)) tempr)
     (setf (aref data i) tempi))
    (setf m (floor (/ n 2)))
 label1
    (when (and (>= m 2) (> j m))
     (setf j (- j m)) (setf m (floor (/ m 2)))
     (go label1))
    (setf j (+ j m))) 
  (setf mmax 2) 
 label2 
  (when (> n mmax)
    (setf istep (cl::* 2 mmax))
    (setf theta  (/ twopi (* isign mmax)))
    (setf wpr (sin (* 1/2 theta)))
    (setf wpr  (* -2 wpr wpr))
    (setf wpi (sin theta)) (setf wr (copy *one*)) (setf wi (newzero))
    (do ((m 1 (+ m 2)))
	((cl::> m mmax) t)
      (declare (fixnum  m))
      (do ((i m (+ i istep)))
	  ((> i n) t)
	(declare (type fixnum i))
	(setf j (+ i mmax))
	(setf tempr  (- (* wr (aref data (1- j)))
				    (* wi (aref data j))))
	(setf tempi  (+ (* wr (aref data j))
				    (* wi (aref data (1- j)))))
	(setf (aref data (1- j)) (- (aref data (1- i)) tempr))
	(setf (aref data j) (- (aref data i) tempi))
	(setf (aref data (1- i)) (+ (aref data (1- i)) tempr))
	(setf (aref data i) (+ (aref data i) tempi)))
      (setf wtemp wr)
      (setf wr (+  (* wr wpr) (* (* -1 wi) wpi) wr))
      (setf wi (+  (* wi wpr) (* wtemp wpi) wi)))
    (setf mmax istep)
    (go label2)) 
   (return data)))

;; hacking four1 for speed, keeping space consumption down

(defun four1 (data nn &key (isign 1))
  (declare (type fixnum nn isign))
  (prog ((wr (copy *zz*)) 
	 (wi (copy *zz*)) 
	 (wpr (copy *zz*))
	 (wpi (copy *zz*))
	 (wtemp (copy *zz*)) 
	 (theta (copy *zz*)) 
	 (halftheta (copy *zz*)) 
	 (cost (copy *zz*))
	 (tempr (copy *zz*)) 
	 (tempi (copy *zz*))
	 (one (into 1))
	 (zero (copy *zz*))
	 (twopi (* 4 (asin (into 1))))	; should be a pre-stored 2*pi somewhere. use current prec.
	 (temprx 0) (tempix 0)
	 (j 0) (n 0) (m 0) (mmax 0) (istep 0))
	(declare  (fixnum j n m mmax istep))
	(setf n (cl::* 2 nn)) 
	(setf j 1) 
	(do ((i 1 (cl::+ i 2)))
	    ((cl::> i n))
	  (declare (fixnum i))
	  (when (cl::> j i) 
	    (setf temprx (aref data (cl::1- j)))
	    (setf tempix (aref data j)) 
	    (setf (aref data (cl::1- j)) (aref data (cl::1- i)))
	    (setf (aref data j) (aref data i)) 
	    (setf (aref data (cl::1- i)) temprx)
	    (setf (aref data i) tempix))
	  (setf m (cl::floor n 2))

	  label1
	  (when (and (cl::>= m 2) (cl::> j m))
	    (setf j (cl::- j m)) (setf m (cl::floor m 2))
	    (go label1))
	  (setf j (cl::+ j m))) 
	(setf mmax 2) 
	label2 
	(when (cl::> n mmax)
	  (setf istep (cl::* 2 mmax))
	  (dsetv theta (into (cl::* isign mmax)))

	  (dsetv theta (/ twopi theta))
	  (dsetv halftheta (* 1/2 theta))
	  (mpfr_sin_cos (gmpfr-f wpr)(gmpfr-f cost) (gmpfr-f halftheta) 0)
	  (dsetv wpi (with-temps (* 2(* wpr cost)))) ;; 2*sin(t/2)*cos(t/2)
	  (dsetv wpr (with-temps (* -2 (* wpr wpr))))

	  (dsetv wr one)
	  (dsetv wi zero)
	  (do ((m 1 (cl::+ m 2)))
	      ((cl::> m mmax) t)
	    (declare (fixnum m))
	    (do ((i m (cl::+ i istep)))
		((cl::> i n) t)
	      (declare (fixnum i))
	      (setf j (cl::+ i mmax))
	      (dsetv tempr (with-temps (- (* wr (aref data (cl::1- j)))
					  (* wi (aref data j)))))
	      (dsetv tempi (with-temps (+ (* wr (aref data j))
					  (* wi (aref data (cl::1- j))))))

	      (dsetv (aref data (cl::1- j))
		     (- (aref data (cl::1- i)) tempr))
	      (dsetv (aref data j)
		     (- (aref data i) tempi))
	      (dsetv (aref data (cl::1- i))   
		     (+ (aref data (cl::1- i)) tempr))
	      (dsetv (aref data i) (+  (aref data i) tempi)))
	    (dsetv wtemp wr)
	    (dsetv wr  (with-temps (+  (+(* wr wpr) (* (* -1 wi) wpi)) wr)))
	    (dsetv wi  (with-temps (+  (+ (* wi wpr) (* wtemp wpi)) wi)))
	    )
	  (setf mmax istep)
	  (go label2)) 
    (return data)))
  

#|  what the answer should be ...
(defun t1()(polymultfft #(1  2 3 4 5 6) #(7 8 9)));; test
(defun t2()
  (let ((m #(1 1 1 1 1 1 1 1 1 1)))
    (polymultfft m m)))
(t1)
#(7 22 46 70 94 118 93 54)
(four1 (v2dfa #(1 2 3 4 5 6) 16) 16) ;; except higher precision
#(21.0d0 0.0d0 4.203712543852026d0 17.12548253340269d0
  -9.656854249492387d0 2.999999999999995d0 1.4918055861931159d0
  -4.857754915068683d0 2.999999999999997d0 4.0d0 ...)

(defun randpol(n m)
  (let ((ans (make-array n))
	(lim (expt 2 m)))
    (dotimes (i n ans)(setf (aref ans i)(random lim)))))

(setf r (randpol 512 332))
(setf qr (map 'vector #'into r))
(time (progn (polymultfft r r) nil))
(time (progn (polymultfft qr qr) nil))
(setf s (make-array 512 :initial-element 1))
(setf qs (map 'vector #'into s))
(setf qt (map 'vector #'into s))

mpfr fft looks like this ...

[3] mpfr(78): (time (progn (polymultfft r r) nil))
; cpu time (non-gc) 811 msec user, 31 msec system
; cpu time (gc)     48 msec user, 0 msec system
; cpu time (total)  859 msec user, 31 msec system
; real time  891 msec
; space allocation:
;  261,293 cons cells, 7,186,264 other bytes, 28,736 static bytes
nil

(time(progn (polymultfft s s ) nil))
; cpu time (non-gc) 578 msec user, 0 msec system
; cpu time (gc)     47 msec user, 0 msec system
; cpu time (total)  625 msec user, 0 msec system
; real time  625 msec
; space allocation:
;  251,041 cons cells, 5,732,296 other bytes, 28,736 static bytes
nil
 qd(82): (time(progn (polymultfft mpfr::r mpfr::r)nil))
; cpu time (non-gc) 390 msec user, 0 msec system
; cpu time (gc)     16 msec user, 0 msec system
; cpu time (total)  406 msec user, 0 msec system
; real time  406 msec
; space allocation:
;  65,822 cons cells, 4,190,368 other bytes, 905,224 static bytes

 qd(13): (time (polymultfft s s))
; cpu time (non-gc) 265 msec user, 0 msec system
; cpu time (gc)     16 msec user, 0 msec system
; cpu time (total)  281 msec user, 0 msec system
; real time  282 msec
; space allocation:
;  19,548 cons cells, 2,206,960 other bytes, 428,916 static bytes
#(1 2 3 4 5 6 7 8 9 10 ...)

 qd(85):  (time(progn (polymultfft qr qr)nil))
; cpu time (non-gc) 265 msec user, 0 msec system
; cpu time (gc)     16 msec user, 0 msec system
; cpu time (total)  281 msec user, 0 msec system
; real time  282 msec
; space allocation:
;  20,556 cons cells, 2,828,336 other bytes, 416,596 static bytes
nil


|#

;;; macroexpanded and then edited for one-arg-log instead of log
(progn (foreign-functions:def-foreign-call (mpfr_log "mpfr_log")
         ((target mpfr (simple-array (unsigned-byte 32) (4)))
          (op1 mpfr (simple-array (unsigned-byte 32) (4))) (rnd :int))
         :returning :int :arg-checking nil :call-direct t)
       (defmethod ga::one-arg-log ((arg gmpfr))
         (let* ((h (alloc-mpfr)) (in (gmpfr-f h)))
           (mpfr_log in (gmpfr-f arg) *rndmode*)
           h))
       (setf (get 'one-arg-log 'argnum) 1)
       (setf (get 'one-arg-log 'mpfr-program) 'mpfr_log))




(progn (foreign-functions:def-foreign-call (mpfr_atan "mpfr_atan")
         ((target mpfr (simple-array (unsigned-byte 32) (4)))
          (op1 mpfr (simple-array (unsigned-byte 32) (4))) (rnd :int))
         :returning :int :arg-checking nil :call-direct t)
       (defmethod ga::one-arg-atan ((arg gmpfr))
         (let* ((h (alloc-mpfr)) (in (gmpfr-f h)))
           (mpfr_atan in (gmpfr-f arg) *rndmode*)
           h))
       (setf (get 'one-arg-atan 'argnum) 1)
       (setf (get 'on-arg-atan 'mpfr-program) 'mpfr_atan))

;; returns a lisp fixnum.  Is this of any use?

(defmethod truncate2fix((z gmpfr))(mpfr_get_si (gmpfr-f z) *rndmode*))

;; here we return a gmpz integer instead.
(defmethod truncate2z((f gmpfr))
  (let ((ans (gmp::into 0))); a gmpz integer
  (mpfr_get_z (gmp::gmpz-z ans) (gmpfr-f f) *rndmode*)
  ans))

(defmethod ga::scale-float((f gmpfr) (h fixnum))
  (let ((fans (into 0)))
    (mpfr_mul_2si (gmpfr-f fans) (gmpfr-f f) h 0)
    fans))
