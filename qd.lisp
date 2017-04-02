;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: qd; Base: 10 -*-
;; Author: Richard Fateman, February, 2006
;; quad-double extension for common lisp based on QD {Hida, Li, Bailey}
;; This file must be loaded after packs.lisp.
;; it must be loaded along with ga.[lisp,fasl] and
;;  "qd.dll"

(defpackage :qd
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
   exp
   sinh cosh tanh 
   asinh acosh atanh 
   sqrt 
   tocl 
   numerator denominator
   expt
   )
    (:export into)
  )

(in-package :qd)
;(eval-when (compile) (load "ga.fasl"))
(eval-when (compile load)
  (declaim (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))
  )

(eval-when (compile load eval)
  (ff:def-foreign-type cqd (:array :double))
  (ff:def-foreign-type cdd (:array :double))
  (ff:def-foreign-type mpz (cl::* :int))
  
   
  ;; the next two programs don't have the right
  ;; effect in Allegro CL on windows x86, and so
  ;; we rewrote the c_qd_xxx programs to call fpu_fix_start
  ;; something like this might work in other lisps, though
  
  #-Allegro
  (ff:def-foreign-call (fpu_fix_start "fpu_fix_start") ;set rounding modes
		       ((cw (cl::* :int)))
		       :returning :void
		       :arg-checking nil :call-direct t :strings-convert nil)

  #-Allegro
  (ff:def-foreign-call (fpu_fix_end "fpu_fix_end") ;restore rounding modes
		       ((cw (cl::* :int)))
		       :returning :void
		       :arg-checking nil :call-direct t :strings-convert nil)

  (ff:def-foreign-call
   (qd_comp  "c_qd_comp");; compare returns -1 0 1  for < = >
      ((op1  (:array :double) (simple-array double-float(4)))
       (op2  (:array :double) (simple-array double-float(4)))
       (target (cl::* :int)))
   :returning :void 		         	     
   :arg-checking nil :call-direct t :strings-convert nil)
  
  (ff:def-foreign-call
   (qd_copy_into  "c_qd_copy");; 
	((op1  (:array :double) (simple-array double-float(4)))
	 (target  (:array :double) (simple-array double-float(4))))
   :returning :void 		         	     
   :arg-checking nil :call-direct t :strings-convert nil)

  (ff:def-foreign-call
   (qd_pi  "c_qd_pi");; produce a value for pi
   ((target  (:array :double) (simple-array double-float(4))))
   :returning :void 		         	     
   :arg-checking nil :call-direct t :strings-convert nil)

  (ff:def-foreign-call
   (qd_rand  "c_qd_rand");; produce a random value
   ((target  (:array :double) (simple-array double-float(4))))
   :returning :void 		         	     
   :arg-checking nil :call-direct t :strings-convert nil)
  
   (ff:def-foreign-call
   (qd_atan2  "c_qd_atan2")
       ((op1  (:array :double) (simple-array double-float(4)))
	(op2  (:array :double) (simple-array double-float(4)))
	(target  (:array :double) (simple-array double-float(4))))
   :returning :void 		         	     
   :arg-checking nil :call-direct t :strings-convert nil)
   
   (ff:def-foreign-call
   (qd_sincos  "c_qd_sincos")
       ((op1  (:array :double) (simple-array double-float(4)))
	(targets  (:array :double) (simple-array double-float(4)))
	(targetc  (:array :double) (simple-array double-float(4))))
   :returning :void 		         	     
   :arg-checking nil :call-direct t :strings-convert nil)
   
   (ff:def-foreign-call
   (qd_sincosh  "c_qd_sincosh")
       ((op1  (:array :double) (simple-array double-float(4)))
	(targets  (:array :double) (simple-array double-float(4)))
	(targetc  (:array :double) (simple-array double-float(4))))
   :returning :void 		         	     
   :arg-checking nil :call-direct t :strings-convert nil)
      
   (ff:def-foreign-call
   (qd_npwr  "c_qd_npwr")  ;; qd to an integer power
       ((base  (:array :double) (simple-array double-float(4)))
	(expon  :int)
	(targetc  (:array :double) (simple-array double-float(4))))
   :returning :void 		         	     
   :arg-checking nil :call-direct t :strings-convert nil)
      
   
      ;; The routines for add, mul, sub, div are defined via defarithmetic macro.
      ;; The routines for sin cos tan etc. are defined via r macro.
      ;; These macros to expand into ff interfaces as well as methods.
      ;; because of special issues of 2 or 1 arg atan and log, we define
      ;; them here.
          
   (ff:def-foreign-call
    (qd_log "c_qd_log")
    ((op1  (:array :double) (simple-array double-float(4)))
     (target  (:array :double) (simple-array double-float(4))))
    :returning :void 		         	     
    :arg-checking nil :call-direct t :strings-convert nil)

   (ff:def-foreign-call
    (qd_atan "c_qd_atan")
    ((op1  (:array :double) (simple-array double-float(4)))
     (target  (:array :double) (simple-array double-float(4))))
    :returning :void 		         	     
    :arg-checking nil :call-direct t :strings-convert nil)

   (ff:def-foreign-call
    (qd_add_double  "c_qd_add_qd_d")	; qd + double ; sometimes useful
    ((op1  (:array :double) (simple-array double-float(4)))
     (op2  :double)
     (target  (:array :double) (simple-array double-float(4))))
    :returning :void 		         	     
    :arg-checking nil :call-direct t :strings-convert nil)
   

;; We could extend the list of operations with other entry points as
;; needed. There are 120 external entry points for qd and dd
;; routines. We don't support access to the dd routines because
;; we suspect that -- as long as you are incurring the overhead
;; for the linkage -- the difference between dd and the higher
;; precision qd is small.

;; An programmer with a strong incentive to gain the most efficiency
;; might wish to access the raw qd routines rather than use the
;; overloaded +,*, etc.  like qd_mul.  The amount to be gained by this
;; is not very great compared to the normal lisp syntax plus two tricks:
;; see below for dsetv and with-temps.

;;Since we want to tag these guys as a QD we do this:
  
(defstruct aqd (q  (make-array 4 :element-type 'double-float 
			       :initial-element 0.0d0 
			       ;; next line is Allegro-specific
			       :allocation :lispstatic-reclaimable)
		   :type (simple-array double-float (4))))
;; Here q is the "guts" of the representation, an array of doubles.
;; The structure "aqd" provides a wrapper or tag so that
;; we can hang methods on it, like print-object, +, * etc.

(defmethod print-object ((a aqd) stream)(format stream "~a" (qd2string a)))

;; This next defmethod says that if the compiler needs to
;; dump any qd objects into a file for later re-loading, the form
;; to use is to just reconstruct the ordinary aqd object. 
;; (this is a standard ANSI CL idiom).

(defmethod make-load-form ((a aqd)&optional environment)
  (make-load-form-saving-slots a :environment environment))

;; The next program converts a qd number to a lisp rational. There is
;; probably a "more efficient" way to do this, but I doubt there
;; is a much shorter program! note denom is a power of 2

(defmethod ga::outof ((x aqd))(qd2lisp x))

(defmethod qd2lisp((x aqd))
  (if (excl::nan-p (aref (aqd-q x) 0)) (aref (aqd-q x) 0)
					; if qd contains a NaN, use it.
      (apply #'cl::+  (map 'list #'rational (aqd-q x)))))

(defmethod qd2lisp(y) y) ; not a qd. just return it.

;; To convert from an ordinary lisp "real" number, use the "into" function.

;; Note that this is qd::into, distinct from "into" programs in other
;; packages.  The optional 2nd arg says we already have a place for
;; this number, so in that case we don't need to allocate a new one.

;;; USE THIS FUNCTION,  into

(defun into(r &optional (where (make-aqd)))
  (lisp2qd r where))

;; The following methods REQUIRE a place to put the answer, and so
;; are less convenient to use directly.
(defmethod lisp2qd((x aqd) (ans aqd)) ;make a fresh copy
  (let ((inx (aqd-q x))
	(ina (aqd-q ans)))
  (dotimes (i 4 ans)(declare (fixnum i))(setf (aref ina i)(aref inx i)))))

(defmethod lisp2qd((x rational) (ans aqd)) ;to encode a lisp rational, divide num/den
  (setf (aqd-q ans) (aqd-q (/ (into (numerator x))(into (denominator x)))))
  ans)

(defmethod lisp2qd((x fixnum) ans)  ;small integers are easy.
  (lisp2qd (coerce x 'double-float) ans))

(defmethod lisp2qd ((x float) (ans aqd)) ;single-float or double-floats fit here
  (let* ((in (aqd-q ans)))
    (loop for i from 1 to 3 do (setf (aref in i) 0.0d0))
    (setf (aref in 0) (coerce x 'double-float))
      ans))


;; this works for bignums
(defmethod lisp2qd ((x integer) res)
  ;; bignum integers that are shorter than a double-float fraction are easy.
  (if (< (cl::abs x) #.(cl::expt 2 53)) (lisp2qd  (coerce x 'double-float) res)

    (let* ((p (coerce x 'double-float))
	   (ans (lisp2qd p res)))
      (dotimes (i 3 ans)
	(setf p (coerce (setf x(- x (round p))) 'double-float))
	(setf ans (+ ans p)))))) ;;CLOS uses QD addition


#+ignore ;; older method, works too. maybe faster.
(defmethod lisp2qd ((x integer) res)	;integer but not fixnum
  ;; bignum integers that are shorter than a double-float fraction are easy.
  (if (< (cl::abs x) #.(cl::expt 2 53)) (lisp2qd  (coerce x 'double-float) res)
    ;; the rest of this code is for when x is a bignum with more bits.
    ;; We convert it, 52-bit section by 52-bit section
    (let* ((ans (aqd-q res))
	   (s (signum x))
	   (shifter (aqd-q (into 1))) ;initially, shift by 1
	   (newdig 0)
	   (shiftam (aqd-q (into(cl::expt 2 52))) ))
      
      (if (< s 0)(setf x (- x)))
      (loop while (> x 0) do
	   (setf newdig (logand x #.(cl::1- (cl::expt 2 52))))
	    (setf newdig (aqd-q (into newdig))) ; grab some bits
	    (qd_mul shifter newdig newdig) ;newdig now a qd
	    (setf x (ash x -52)) ;remove them from x
	    (qd_add ans newdig ans)
	    (qd_mul shifter shiftam shifter))
      (if (< s 0)(qd_mul ans (into -1) ans))
      res)))

;; should be faster ways of shifting and changing sign in the above.
  
;; Converting from qd to lisp to  a decimal (or other base) string we
;; look at the first n decimal digits of a fraction, also show exponent.
;; We only use base 10 in qd2string, which is used in the default print method.

;; lplt will return z such that base^z<r; it is
;; not completely general. Works for
;; rational r, where r can be converted without overflow to double-float.
;; Therefore it should be OK for qd numbers.

(defun largest-power-less-than(r &optional (base 10))
  (if (cl::= r 0) -1
  (let ((guess (cl::floor(cl::log (cl::* 1.0d0 (cl::abs r)) base))))
    (if (>= (expt base guess) r) (1- guess) guess))))
;; a table of (expt base guess) could be precomputed or memoized.

;; convert rational r to a decimal [or other base] number with n digits
;; showing in the fraction. If r=0, then just 0.
;; return sign=1,-1, or 0;  fraction is an integer, exponent is an integer.

(defun decimalize(r n &optional (base 10)); r rational number >=0
  (let* ((expon  (largest-power-less-than r base))
	 (frac (truncate;; was, round. causes tricky edge cases.
		(cl::*(cl::abs r) (cl::expt base (cl::- n expon 1))))))
    (values   (signum r)
	      frac
	      (cl::1+ expon))))

;; make a formatted output . Something like this does the job

(defparameter *qd-digits-to-show* 66) ; MAX number of digits to show. Trailing zeros are omitted.

(defmethod qd2string((x aqd))
  ;; check if x is a NaN
  (if (excl::nan-p (aref (aqd-q x) 0)) "NaN(qd)"
    (multiple-value-bind (s r e h)  ;h is extra variable
	(decimalize (qd2lisp x) *qd-digits-to-show* 10)
      (format nil "~a0.~aQ~s" (if (cl::< s 0)"-" "") 
	      (string-right-trim 
	       "0"  
	       (subseq (setf h(format nil "~a" r)) 0  
		       (cl::min (length h) *qd-digits-to-show*)) )
	      e))))

#+ignore  ;; an alternative regarding NaN

(defmethod qd2string((x aqd))
  ;; check if x is a NaN
  (if (excl::nan-p (aref (aqd-q x) 0)) excl::*nan-double*
    (multiple-value-bind (s r e h)  ;h is extra variable
	(decimalize (qd2lisp x) *qd-digits-to-show* 10)
      (format nil "~a0.~aQ~s" (if (cl::< s 0)"-" "") 
	      (string-right-trim 
	       "0"  
	       (subseq (setf h(format nil "~a" r)) 0  
		       (cl::min (length h) *qd-digits-to-show*)) )
	      e))))

(defmacro defarithmetic (op pgm)
    (let ((two-arg
	   (intern (concatenate 'string "two-arg-" (symbol-name op))
		   :ga ))
	  (c-entry
	   (concatenate 'string "c_"(symbol-name pgm))))
 ;;           (format t "~% defining ~s" two-arg)
      `(progn
	 (ff:def-foreign-call
	  (,pgm  ,c-entry);; associate, for example, qd_mul with c_qd_mul
	  ((op1  (:array :double) (simple-array double-float(4)))
	   (op2  (:array :double) (simple-array double-float(4)))
	   (target  (:array :double) (simple-array double-float(4))))
	  :returning :void 		         	     
	  :arg-checking nil :call-direct t :strings-convert nil)
	 ;; new defmethods for qd. .. note order of args different from gmp
	 (defmethod ,two-arg ((arg1 aqd) (arg2 aqd))
	   (let* ((r (make-aqd)) 
		  (in (aqd-q r))
		  (a1 (aqd-q arg1))
		  (a2 (aqd-q arg2)))
	     (declare (optimize speed)
		      (type(simple-array double-float (4)) in a1 a2))
	     (,pgm a1 a2 in) r))
       
	 (defmethod ,two-arg ((arg1 real) (arg2 aqd))
	   (let* ((r (into arg1))
		  (in (aqd-q r))
		  (a2 (aqd-q arg2)))
	     (declare (optimize speed)
		      (type(simple-array double-float (4)) in a1 a2))
	     (,pgm in a2 in) r))
       
	 (defmethod ,two-arg ((arg1 aqd) (arg2 real))
	   (let* ((r (into arg2))
		  (in (aqd-q r))
		  (a1 (aqd-q arg1) ))
	     (declare (optimize speed)
		      (type(simple-array double-float (4)) in a1 a2))
	   
	     (,pgm a1 in in) r))
	 (setf (get ',op 'argnum) 2) ;used by with-temps, dsetv
	 (setf (get ',op 'qd-program) ',pgm) ;used by with-temps, dsetv
	 (setf (get ',two-arg 'qd-program) ',pgm) ;used after macroexpand-all
	 (setf (get ',two-arg 'argnum) 2)
	 
	 )))

(defarithmetic + qd_add)
(defarithmetic - qd_sub)
(defarithmetic * qd_mul)
(defarithmetic / qd_div)

(defmethod ga::two-arg-expt ((base aqd)(n integer))
  (let ((ans (make-aqd)))
    (qd_npwr (aqd-q base) n (aqd-q ans))
    ans)) ;; this special case doesn't fit into defarithmetic macro.

;; eh, if you use expt, we won't compile it fancy.
;;(setf (get 'ga::two-arg-expt 'argnum) 2)
;;(setf (get 'ga::two-arg-expt 'qd-program) 'qdpower) 


;; maybe define (power qd n) for n integer???

(defmethod ga::two-arg-expt ((base aqd) (n aqd))
  (exp (* n (log base))))

(defmethod ga::two-arg-expt ((base aqd) (n real))
  (exp (* (into n) (log base))))

(defmethod ga::two-arg-expt ((base real) (n aqd))
    (exp (* n (log (into base)))))


;; do analogous stuff for other entry points of interest from qd.dll
;; the call to ff:def-foreign-call is specific to allegro common lisp.
;; Someone else may translate these into an alternative like UFFI.

(defmacro r (op);;
  (let ((fun-name  (intern op :ga ))
	(cqd-name (format nil "c_qd_~a" op))
	(qd-symb (intern (format nil "qd_~a" op))))
    `(progn
       (ff:def-foreign-call
	(,qd-symb ,cqd-name)
	((op1  (:array :double) (simple-array double-float (4)))
	 (target  (:array :double) (simple-array double-float (4))));reverse order from gmp
	:returning :void 		         	     
	:arg-checking nil :call-direct t :strings-convert nil)
       (defmethod ,fun-name ((arg aqd))
	 (let* ((h (make-aqd)) (in (aqd-q h)))
	   (declare (optimize speed)
		    (type (simple-array double-float (4)) in))
	   (,qd-symb (aqd-q arg) in) h))
       ;;(compile ',fun-name)
       (setf (get ',fun-name 'argnum) 1)
       (setf (get ',fun-name 'qd-program) ',qd-symb)
       
       
       )))

(r abs) 
(r sin )
(r cos )
(r tan )
(r exp )

(r log10)
(r asin )
(r acos )
;(r atan )
(r sinh )
(r cosh )
(r tanh )
(r asinh)
(r acosh)
(r atanh)
(r sqrt )
)					;end of eval-when

(r neg)  ;need to put in ga.
;(r floor qd_floor)
;(r ceil qd_ceil)
;; more  still. 

;; Here's how to work with atan and log, which in Lisp can take one or two args.
;;(defmethod ga::two-arg-atan((a real)(b real))  (cl:atan a b))

(defmethod ga::two-arg-atan((a aqd)(b aqd))
  (let((ans (make-aqd)))
    (qd_atan2 (aqd-q a)(aqd-q b)(aqd-q ans ))
    ans))

(defmethod ga::two-arg-atan((a aqd)b)
  (let((ans (make-aqd)))
    (qd_atan2 (aqd-q a)(aqd-q (into b))(aqd-q ans ))
    ans))

(defmethod ga::two-arg-atan(a (b aqd))
  (let((ans (make-aqd)))
    (qd_atan2 (aqd-q (into a))(aqd-q b)(aqd-q ans ))
    ans))


(defmethod ga::two-arg-log((a aqd)(b (eql 10)))
  (let ((ans (make-aqd)))
    (qd_log10 (aqd-q a)(aqd-q ans))ans))

(defmethod ga::two-arg-log((a aqd)(b real))
  (/ (ga::one-arg-log a)(ga::one-arg-log (into b))))

(defmethod ga::two-arg-log((a real)(b aqd))
	   (/ (ga::one-arg-log (into a))(ga::one-arg-log b)))

(defmethod ga::one-arg-log((r aqd))  (let ((ans (make-aqd)))
				       (qd_log (aqd-q r) (aqd-q ans))
				       ans))

(defmethod ga::one-arg-atan((r aqd))  (let ((ans (make-aqd)))
				       (qd_atan (aqd-q r) (aqd-q ans))
				       ans))
;;; comparisons
;;; the c_qd_comp function does not return a value; it sets the return in a box.
;;; allocate the space for that return value, one time, and re-use it.


(eval-when (compile load eval)

  
(defvar compare-box (make-array 1 :element-type '(signed-byte 32) 
				  :initial-element 0 :allocation :lispstatic-reclaimable))

(defmacro defcomparison (op val)
    (let ((two-arg (intern (concatenate 'string "two-arg-" 
					(symbol-name op)) :ga))
	  )
    ;;  (format t "~% defining ~s" two-arg)
      `(progn
	 ;; only extra methods not in ga are defined here.
	 ;; qd_comp returns -1 0 1   for < = >
	 (defmethod ,two-arg ((arg1 aqd) (arg2 aqd))   
	   (declare (special compare-box))
	   (qd_comp(aqd-q arg1)(aqd-q arg2) compare-box)
	   ;; (format t "~%a1=~s,  a2=~s, compare a1 ~s a2 =~s" arg1 arg2 ',op (aref compare-box 0))
	   (member (aref ,compare-box  0) ,val :test #'=))
	 (defmethod ,two-arg ((arg1 real) (arg2 aqd))
	   (declare (special compare-box))
	   (let ((arg1 (into arg1)))
	     (qd_comp(aqd-q arg1)(aqd-q arg2) compare-box)
	     (member (aref compare-box  0) ,val :test #'=)))
	 (defmethod ,two-arg ((arg1 aqd) (arg2 real))
	   (declare (special compare-box))
	   (let ((arg2 (into arg2)))
	     (qd_comp(aqd-q arg1)(aqd-q arg2) compare-box)
	     (member (aref compare-box  0) ,val :test #'=)))
	 ',op)))

(defcomparison = '(0))
(defcomparison > '(1))
(defcomparison < '(-1))
(defcomparison /= '(-1 1))  ;; not equal is either less or greater
(defcomparison >= '(0 1))
(defcomparison <= '(0 -1))
)

;; In subsequent parts of the file we will be re-using storage,
;; or at least proposing to do so.  In such a programming context
;; we need to, at least occasionally, make a private copy of something.
;; Here's how.  (remember, this is qd::copy).

(defmethod copy((a aqd))(make-aqd :q (make-array 4 :element-type 
						 'double-float 
						 :initial-contents (aqd-q a)
						 :allocation :lispstatic-reclaimable)))

(defmacro copy_mac(a)
  `(make-aqd :q (make-array 4 :element-type 
			    'double-float 
			    :initial-contents (aqd-q ,a)
			    :allocation :lispstatic-reclaimable)))
;; maybe this is worth doing.
;; accumulating a qd by adding in smaller lisp double-floats.
(defmethod addqd_d ((a aqd) (d real))   ;;qd+fixnum, qd+flonum, only good to double-precision
  (let* ((targ (make-aqd))
	 (in (aqd-q targ)))
    (qd_add_double (aqd-q a) (coerce d 'double-float) in) targ))

(defmethod ga::1+((a aqd))    (addqd_d  a 1.0d0))
(defmethod ga::1-((a aqd))    (addqd_d  a -1.0d0))


;;; We want to make better use of the state-based programs like qd_mul.
;;; Assuming aqd... for a, b, and c:  (dsetv a (+ b c)) destroys the value in a.
;;; Compare this to (setf a (+ b c)) which creates a new value and points a to it.

;; dsetv,  data driven
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
       (qd_copy_into  (aqd-q ,gg) (aqd-q ,(cadr ex)))
       ,gg)))
   (t 
    (let* ((op (car ex))
	   (args (cdr ex))
	   (the-op (get op 'qd-program))
	   (argnum (get op 'argnum)))
      (cond 	       
       ((not the-op);; not a previously listed op
	`
	   (let* ((lval ,targ)
		  (a1 (aqd-q  (,op ,@ args)))
		  (tt (aqd-q lval)))
	     (declare (optimize speed)
		      (type (simple-array double-float (4)) a1 tt))
	     (qd_copy_into a1 tt)
	     lval))
       ((not (eql argnum (length args))) 
	(error "dsetv was given operator ~s which expects ~s args, but was given ~s --  ~s" 
	       op argnum (length args) args))
       (t
	(case argnum
	  (1;; one argument.
	   `(let ((a1 (aqd-q ,(macroexpand `(with-temps ,(car args)))))
		    ;;(a1 (aqd-q ,(car args)))
		    (tt (aqd-q ,targ)))
		(declare (optimize speed)(type (simple-array double-float (4)) a1 tt))
		;; could also check other args for being type qd
		;; could also allow for args to be si, ui, dd, etc.
		;; could also check number of args to be appropriate for operation
		(,the-op a1 tt)
	      ,targ))
	  (2
	   `(let ((a1 (aqd-q ,(macroexpand `(with-temps ,(car args)))))
		   ;(a1 (aqd-q  ,(car args)))
		    (a2 (aqd-q ,(macroexpand `(with-temps ,(cadr args)))))
		  ;;  (a2 (aqd-q  ,(cadr args)))
		    (tt (aqd-q ,targ)))
		(declare (optimize speed)(type (simple-array double-float (4)) a1 a2 tt))
		(,the-op a1 a2 tt)
		,targ
		))
	  (otherwise (error "argnum is wrong for op ~s " op))
	  )))))))



;;;;;;;;;;;;;;;;;;more efficiency hackery follows.;;;;;;;;;;;;;;;;

;; make a pile of qd frames, so allocating/deallocating is done at compile time.
;; The intention is to obviate the need to find a place to store the results,
;; knowing that the results will be used only once.

;; One thought --- we could allocat a stack of qds.
;;(defparameter qdstack  (make-array 100 :element-type 'aqd :initial-element (make-aqd) 
;;		     :weak t  ;; not all lisps have this option. affects garbage collection.
;;		     :fill-pointer 100 :adjustable t))

;;(dotimes (i 100)(setf (aref qdstack i) (into i))) ; just put something there so we can see it

;; When computing, take temporary locations off that stack;
;; when finished, restore the fill pointer. 

;; Or we could just allocate a few private  "registers" say, for a function, or an inner loop,
;; and re-use them, if we are in a loop. No need to tell anyone else about a few temp locations,
;; especially if they are GC'd when truly inaccessible.  That's what is below.

(defmacro with-temps(expr)
  (let ((*names* nil)
	(*howmany* 0))
    (labels ((genlist(n)(loop for i from 1 to n collect (into i))) ;make a list of fresh qd items
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
			   ;; just a symbol name? maybe aref? better be the right type, aqd.
			   (t  r))))
      (setf expr (macroexpand expr))
       (ct1 expr)
     ;; (ct1 expr); count the temporaries
    (setf *names* (genlist *howmany*))
    (maketemps expr))))


;;  try (pprint (macroexpand '(with-temps (+ x (* 3 z)))))
;; or  (defun hypot(x y)(copy (with-temps (sqrt (+ (* x x)(* y y))))))
;; need the call to copy to make a  copy of the result before calling hypot again. 

;; set up the environment for with-temps and dsetv here


(eval-when (compile load eval)
  (mapc #'(lambda(h) (setf (get h 'argnum) 2)) '(atan2 log2 setq))
;; for now assume they are given only one arg and if they are given 2 signal an error with dsetv.
  (mapc #'(lambda(h) (setf (get h 'argnum) 1)) '(atan log))
;;  (defun qd_setq (a b)(qd_copy_into b a) b)
 )

;; just in case you need both sin and cos,  or sinh and cosh.

(defmethod sincos1((a aqd)(s aqd)(c aqd))
  (let ((a-in (aqd-q a))(s-in (aqd-q s))(c-in (aqd-q c)))
    (qd_sincos a-in s-in c-in)))

(defmethod sincosh1((a aqd)(s aqd)(c aqd))
  (let ((a-in (aqd-q a))(s-in (aqd-q s))(c-in (aqd-q c)))
    (qd_sincosh a-in s-in c-in)))

(defmethod sincos((a aqd))
  (let* ((s (make-aqd))	 (c (make-aqd)))
	 (sincos1 a s c)
	 (values s c)))

(defmethod sincosh((a aqd))
  (let* ((s (make-aqd))	 (c (make-aqd)))
	 (sincosh1 a s c)
	 (values s c)))

(defun compute-pi()(let((ans (make-aqd)))
		    (qd_pi (aqd-q ans))
		    ans))

;; dqlist is an ordinary lisp list of the
;; coefficients in a polynomial. The last item in the list
;; is the constant coefficient. All coeffs are aqd objects.
;; (setf testpoly (list (into 5) (into 3) (into 1))) ;; 5*x^2+3*x+1
;; (setf x (into 1))

(defun polyevalqd (qdlist x)
      (let ((sum (make-aqd)))
      (dolist (i qdlist sum)
	(setf sum (with-temps (+ i (* x sum)))))))
;; takes about 250ms for 100 iterations of eval of qones at two

#+ignore ;; just for comparison with above.
(defun polyeval (qdlist x)  ;; about 10% slower, but uses 40X more CONS storage, 85X more other bytes
      (let ((sum (make-aqd)))
      (dolist (i qdlist sum)
	(setf sum  (+ i (* x sum))))))

;; pe is the same functionality as polyeval.
;; possibly a teensy bit faster. Benchmarks show no difference. 2/18/06 RJF
;; some test data:
#+ignore  ;;setting up tests
(progn
 (setf ones (loop for i from 1 to 999 collect (expt -1 (1+ i))))
 (setf qones (mapcar #'into ones))
 (setf dones (mapcar #'(lambda(s)(* s 1.0d0)) ones))
 (setf FA (qdlist2flatarray (reverse qones)))
 'done)
#+ignore
(progn (format t "~%testp1") (time (test-p1 10000)) 
       (format t "~%testp2") (time (test-p2 10000))
       (format t "~%testp3") (time (test-p3 10000)))

;; (time (pe qones (into 2)))
;; (time (dotimes (i 1000)(pe qones (into 2))))
;; (time (polyevali ones 2))
;; (setf targ (into 0))
;; (setf intarg (adq-q targ))
;; (qd_polyeval fones 998 (aqd-q (into 2))  intarg)
;; (setf tones '(1 0 0 0 0 0 0 0 1))
;; (setf qones (mapcar #'into tones))
;; (qd_polyeval fones 8 (aqd-q (into 2))  intarg)

;; 

;; A carefully declared polynomial eval using
;; Lisp double-floats. I don't see how to make this more optimized.
(defun polyevald (dlist x) 
  (let ((sum 0.0d0))
    (declare (double-float sum x) (optimize (speed 3)(debug 0)))
    (dolist (i dlist sum)
      (declare (double-float i))
      (setf sum (the double-float 
		  (cl::+ i
			 (the double-float 
			   (cl::* x sum)))))))) ;; this is about 150 X faster than the QD versions.

(defun polyevaldclos (dlist x) ;; try with double-floats, let the CLOS work. Look at two-arg expansion
  ;; this takes 220 ms, so it is not compiled so well.
  (let ((sum 0.0d0))
    (declare (double-float sum x) (optimize (speed 3)(debug 0)))
    (dolist (i dlist sum)
      (declare (double-float i))
      (setf sum (+ i (* x sum)))
      ;;(setf sum (ga::two-arg-+ i (ga::two-arg-* x sum))) ;;same as above line.
      )))

(defun polyevali (ilist x) ;; polynomial eval that works for ANY lisp numbers
  (let ((sum 0))
    (declare  (optimize (speed 3)(debug 0)))
    (dolist (i ilist sum)
      (setf sum (cl::+ i (cl::* x sum))))))    ;; this is about 9X faster than QD.

;;(defun test-p1 (n)(dotimes (i n)(polyevald dones 2.0d0)))
;;(defun test-p2 (n)(let ((two (into 2)))(dotimes (i n)(polyevalqd qones two))))
;;(defun test-p3 (n)(let ((two (into 2)))(dotimes (i n)(polyevalqfX FA 998  two))))
;;(defun test-p4 (n)(let ((two (into 2)))(dotimes (i n)(polyevali ones  2))))

;;; Here's some non-operative programs for windows x86 Allegro.
;;; maybe useful for other lisps wehre fpu_fix_start can be called once at the
;;; beginning of a seuence of QD operations, instead of before EACH operation.
;; For Allegro  we changed code in c_qd.cpp to call fpu_fix_start as needed.

;;(defvar *rndmodecw* (make-array 1 :element-type '(signed-byte 32) :initial-element 0))
;;(defmacro fpu_start ()'(fpu_fix_start *rndmodecw*) ) ;stores old control word in *rndmodecw*
;;(defmacro fpu_end ()'(fpu_fix_end *rndmodecw*))

;;; some more testing programs are in the file test-qd.lisp, which should be nearby.

(eval-when (compile load eval)
(ff:def-foreign-call
   (qd_polyevalflat "c_qd_polyevalflat")
    ( (op1 (:array :double)  (simple-array double-float(4))) ;;   length is really 4X (1+ op2)
      (op2  :int)  ;;degree of polynomial.
      (op3   (:array :double) (simple-array double-float(4))) ;x
      (target  (:array :double) (simple-array double-float(4))));; the answer
   :returning :void 		         	     
   :arg-checking nil :call-direct t :strings-convert nil))

(defun qdlist2flatarray(L)
  (let* ((n (length L))
	 (ans (make-array (* 4 n) :element-type 'double-float
	      		  :allocation :lispstatic-reclaimable
	      ))				;no need to initialize
	 (index 0))		
    (dolist (qditem L ans)
      (setf (subseq ans index (cl::incf index 4)) (aqd-q qditem)))))

(defun flatten(L)(qdlist2flatarray L))

(defun polyevalqfX (FA n x) ;;precomputed. FA is flat array, n is (1- length/4)  x is aqd
    (let* ((targ (into 0))
	   (intarg (aqd-q targ))
	   (inx (aqd-q x)))
      (declare (optimize speed)
	       (type (simple-array double-float (4)) FA intarg inx)
	       (fixnum n) (aqd x) )
      (qd_polyevalflat FA n inx  intarg)
      targ))

(defun polyevalqf(L x) ;more of a top-level program, should be fast!
    (polyevalqfX (qdlist2flatarray (reverse L)) 
		 (1- (length L))
		 (into x)))

#|
;; if you compute something but want to keep the result around until GC, it
;; is important to make a copy of the result. Otherwise it may be overwritten.
;; E.g. the third version below will overwrite the previous value returned.
;; This badness "optimization" happens only when hypot is compiled.

;;(defun hypot(x y)(copy (with-temps (sqrt (+ (* x x)(* y y))))));; ok
;;(defun hypot(x y) (sqrt (with-temps (+ (* x x)(* y y)))))  ;; also ok
;;(defun hypot(x y) (with-temps (sqrt (+ (* x x)(* y y))))) ;; bad, at least if compiled

|#

;; A BUNCH OF PROGRAMS PLAYING QD GAMES and ROOTFINDING.

;; Refining a polynomial root via Newton's method.
;; First, deriv computes the derivative of a polynomial, in a list.
;; recall that we can create a polynomial as a list:
;; (setf testpoly (list 5 3 1)) ;; 5*x^2+3*x+1
;; or as a QD polynomial
;; (setf testpoly (list (into 5) (into 3) (into 1))) ;; 5*x^2+3*x+1

(defun deriv(coefs) ;given coefs of a polynomial. return coefs of derivative.
  (let ((ans nil) (i 0))
    (dolist (c (cdr (reverse coefs)) ans) 
      (push (* (incf i) c) ans))  ans))

;;  a fairly general Newton iteration requiring three parameters, and some optional ones.
;;  f  is a function to evaluate f(x),
;;  df is a function to evaluate f'(x).
;;  x, a starting point,
;; optional: threshold for absolute value of f at which to stop,
;; optional: iters, a maximum number of iterations.

;;  oneroot uses whatever arithmetic is appropriate, based on what f and df use.

(defun oneroot (f df x threshold iters &aux fval)
  (dotimes  (i iters (error "rootfinder failed to converge. Residual is ~s after ~s iterations." 
			    fval i))
    (setf fval (funcall f x))
    (if (< (abs fval) threshold) 
	(return (values x fval i))		;return x, residual and iteration count
      (decf x (/ fval (funcall df x))))))


;; if we replace (decf x (/ fval (funcall df x)))
;; by            (dsetv x (with-temps (- x (/ fval (funcall df x))))))))
;; this would save 2 allocated temporaries per iteration.


;; Next, set up a call to oneroot given a list of coefficients representing
;; a polynomial.  polyrootd makes everything computing in (real) double-float.
;; defaults are provided, so all you need is the list and a starting point.
;; the inputs coefs and x may be given as exact integers, floats, rationals.
;; example  (polyrootd '(1 0 -2) 1); returns 1.4142135623730951d0, i.e. sqrt(2)

(defun polyrootd (coefs x &optional (threshold 1.0d-14) (iters 20))
  (setf coefs (mapcar #'(lambda(z)(coerce z 'double-float))coefs))
    (let ((dp (deriv coefs)))
      (oneroot  #'(lambda(x)(polyevald coefs x))
		#'(lambda(x)(polyevald dp x))
		(* 1.0d0 x)
		(* 1.0d0 threshold)
		iters)))

;; this is the same functionality, but using higher (QD) precision.
;; the inputs coefs and x may be given as exact integers, floats, rationals.
;; (qd::polyroot '(1 0 -2) 1); returns sqrt(2), but if you want more precision:
;; (qd::polyroot '(1 0 -2) 1 1.0d-60)  returns
;;0.141421356237309504880168872420969807856967187537694807317667973796Q1

;;this is called qd:polyroot. You may prefer other ways to do it, 
;;e.g. mpfr::polyroot or some interval method, or some method
;; computing derivatives.

(defun qd::polyroot (coefs x &optional (threshold 1.0d-15) (iters 20))
  (setf coefs (mapcar #'into coefs))
  (let ((dp (deriv coefs)))
    (oneroot  #'(lambda(x)(polyevalqd coefs x))
	      #'(lambda(x)(polyevalqd dp x))
	      (into x)
	      (into threshold)
	      iters)))

(defun qd_random( &optional (where (make-aqd)))
  (qd_rand (aqd-q where))
  where)

;; ONEROOT is considerably more versatile: it can find root of other
;; functions, not just polynomials.

;; (defun tt (x)(- (cos x) x))  ;; easy near 0.73
;; (defun ttd(x) (- (+ (sin x) 1))  )
;; (oneroot #'tt #'ttd (into 0) (into 1.0d-64) 20)
;; (oneroot #'tt #'ttd 0.0d0 1.0d-38 20)

;;;;;;;;;;;;;;;;;;;FFT!!!;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  
; Fourier Transform Spectral Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;Routines translated with permission by Kevin A. Broughan from ;;;;;;;;;;;
;;Numerical Recipies in Fortran Copyright (c) Numerical Recipies 1986, 1989;;;;
;;;;;;;;;;;;;;;Modified by Ken Olum for Common Lisp, April 1996;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; notes 4/27/05, RJF.  
;; more notes, 2/28/06 RJF. Converting h:/lisp/fft.lisp to QD arithmetic.
;; see comments at lisp/fft.lisp.
;; We use four1 only
;; the input data will be overwritten by the result.
;; the inverse fft is indicated by :isign -1, though it must be
;; multiplied by  1/nn to work.
; functions:
;	four1: fourier transform (FFT) in one dimension

(defun v2dfa(a &optional (m (length a))	)
  ;;coerce a vector of QD numbers (or lisp numbers) of length m to a
  ;; QD array of length 2m, since here complex numbers are stored in 2
  ;; adjacent QD locations.
  (let* ((k (length a))
	 (ans (make-array (cl::* 2 m) :allocation :lispstatic-reclaimable ))
	 (zz (qd::into 0))
	 (h nil))
    (declare (fixnum k))
    (dotimes (i k)	;just copy the actual numbers, or convert if needed
      (declare (fixnum i))
      (setf (aref ans (cl::* 2 i))
	(if (aqd-p (setf h (aref a i)))(copy h) (qd::into h))) ;; here we convert.
      (setf (aref ans (cl::1+ (cl::* 2 i))) (copy zz)))
    (loop for i fixnum from (* 2 k) to (1-(* 2 m)) do 
	  (setf  (aref ans i) (copy zz)))
    ans))

;; (v2dfa #(30 40 50) 4)
;;  --> #(0.3Q2 0.Q0 0.4Q2 0.Q0 0.5Q2 0.Q0 0.Q0 0.Q0)

(defparameter *zz* (qd::into 0))
(defparameter *one* (qd::into 1))

(defun dfa2v(a &optional (m (/ (length a)2)))
  ;; Coerce real parts back to integers, more or less.  a is an an
  ;; array of even length.  If you know that there are trailing zeros,
  ;; set the actual length with the optional second parameter m.
  (let* ((k (/ (length a) 2))
	 (ans (make-array m)))
    (declare (fixnum k))
    (dotimes (i m ans)
      (declare (fixnum i))
      (setf (aref ans i)(round (ga::outof (aref a (cl::* 2 i))) k)))))

;;(dfa2v  (v2dfa #(256 256 256) 4))
;; -->  #(64 64 64 0)  is correct.

;; this works!
(defun polymultfft(r s)
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
	 (ans (make-array (* 2 z))))
    (dotimes (i (* 2 z))(setf (aref ans i) (copy_mac *zz*)))
    (prodarray rfft sfft z ans)
    (dfa2v(four1 ans z :isign -1) lans)))

(defun polysquare(r) 
  ;;just square the polynomial r. saves one fft. so you can compare times.
  (let* ((lr (length r))
	 (lans (+ lr lr -1))
	 (z (ash 1 (ceiling (log lans 2)))) ; round up to power of 2
	 (rfft (four1 (v2dfa r z) z))
	 (ans (make-array (* 2 z))))
    (prodarray rfft rfft z ans)
    (dfa2v(four1 ans z :isign -1) lans)))

#+ignore
(defun polytime(r s)  ;; this shows that much of the time is in v2dfa etc
  (let* ((lr (length r))
	 (ls (length s))
	 (lans (+ lr ls -1))
	 (z (ash 1 (ceiling (log lans 2)))) ; round up to power of 2
	 (r1 (v2dfa r z))
	 (s1 (v2dfa s z))
	 (sfft 0)
	 (rfft  (four1 r1 z)))
;;    (start-profiler)
  (time (progn (setf sfft (four1 s1 z))
    (setf sfft (four1 s1 z))
    (setf sfft (four1 s1 z))
    (setf sfft (four1 s1 z))
    (setf sfft (four1 s1 z))))
;;    (show-flat-profile)
	 ;;(prod (prodarray rfft sfft z ))
	 ;;(ans (time(four1 prod z :isign -1)))
	 )
    ;;(dfa2v ans lans)
    )

#+ignore
(defun polytime2(r)  ;; this shows that much of the time is in v2dfa etc
  (let* ((lr (length r))
	 (lans (+ lr lr -1))
	 (z (ash 1 (ceiling (log lans 2)))) ; round up to power of 2
	 (r1 (v2dfa r z))
	 (start-profiler)
	 (rfft (time (four1 r1 z)))
	 (prod (prodarray rfft rfft z  rfft))
	 (ans (four1 prod z :isign -1)))
    (show-flat-profile)
  (dfa2v ans lans)))

;; Utility to copy an array because this fft will clobber input.
;; We don't use it here, but here's a way to write it.
;; (defun copyarray (a)(make-array (length a):initial-contents a))

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

(defparameter qdtwopi
    (ga::/ (qd::into  165424160919322423196824703508232170249081435635340508251270944637 )
     (qd::into  26328072917139296674479506920917608079723773850137277813577744384))
  )
(defparameter onehalf (/ (qd::into 1) (qd::into 2)))


;;0.62831853071795864769252867665590057683943387987502116419498891846Q1

;;; this is a fairly generic FFT that works, but not optimized much.
;;; we leave it here just in case you want to copy it for other 
;;; generic arithmetic packages
#+ignore
(defun four1 (data nn &key (isign 1))
  (declare (type fixnum nn isign))
 (prog ((wr (copy *zz*)) 
	(wi (copy *zz*)) 
	(wpr (copy *zz*))
	(wpi (copy *zz*))
	(wtemp (copy *zz*)) 
        (theta (copy *zz*)) 
	(tempr (copy *zz*)) 
	(tempi (copy *zz*))
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
    (setf theta  (/ qdtwopi (* isign mmax)))
    (setf wpr  (* -2 (expt (sin (* 1/2 theta)) 2)))
    (setf wpi (sin theta)) (setf wr (copy *one*)) (setf wi (copy *zz*))
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
  (prog ((wr (copy_mac *zz*)) 
	 (wi (copy_mac *zz*)) 
	 (wpr (copy_mac *zz*))
	 (wpi (copy_mac *zz*))
	 (wtemp (copy_mac *zz*)) 
	 (theta (copy_mac *zz*)) 
	 (halftheta (copy_mac *zz*)) 
	 (cost (copy_mac *zz*))
	 (tempr (copy_mac *zz*)) 
	 (tempi (copy_mac *zz*))
	 (one (copy_mac *one*))
	 (zero (copy_mac *zz*))
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
	  (dsetv theta (/ qdtwopi theta))
	  (dsetv halftheta (* 1/2 theta))
	  (qd_sincos (aqd-q halftheta)(aqd-q wpr)(aqd-q cost))
	  (dsetv wpi  (* 2(* wpr cost))) ;; 2*sin(t/2)*cos(t/2)
	  (dsetv wpr  (* -2 (* wpr wpr)))

	  (dsetv wr one)
	  (dsetv wi zero)
	  (do ((m 1 (cl::+ m 2)))
	      ((cl::> m mmax) t)
	    (declare (fixnum m))
	    (do ((i m (cl::+ i istep)))
		((cl::> i n) t)
	      (declare (fixnum i))
	      (setf j (cl::+ i mmax))
	      (dsetv tempr  (- (* wr (aref data (cl::1- j)))
			       (* wi (aref data j))))
	      (dsetv tempi (+ (* wr (aref data j))
			      (* wi (aref data (cl::1- j)))))

	      (dsetv (aref data (cl::1- j))
		     (- (aref data (cl::1- i)) tempr))
	      (dsetv (aref data j)
		     (- (aref data i) tempi))
	      (dsetv (aref data (cl::1- i))   
		     (+ (aref data (cl::1- i)) tempr))
	      (dsetv (aref data i) (+  (aref data i) tempi)))
	    (dsetv wtemp wr)
	    (dsetv wr  (+  (* wr wpr) (* (* -1 wi) wpi) wr))
	    ;;(dsetv wr  (with-temps (+  (* wr wpr) (*  -1 wi wpi) wr)))
	    
	    (dsetv wi  (+  (* wi wpr) (* wtemp wpi) wi))
	    ;;(dsetv wi  (with-temps (+  (* wi wpr) (* wtemp wpi) wi)))
	    )
	  (setf mmax istep)
	  (go label2)) 
    (return data)))
)  

#|  what the answer should be ...
(defun t1()(polymultfft #(1  2 3 4 5 6) #(7 8 9)));; test
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
(setf qs (map 'vector #'into     s))
(time (polymultfft qs qs) ))

(setf s (make-array 8 :initial-element 1))

|#



;;; Gaussian Quadrature, quad-double precision
;;; See quad-ga.lisp for basic ideas and comments
;;; refer to http://crd.lbl.gov/~xiaoye/quadrature.pdf
;;; Jan 10, 2007, revised March 27,2007.
;;; (c) Richard Fateman. 

;; Legendre_pd returns legendre_p(k,x) and its derivative.
;; Note that d/dx P[n](x) =  (1/(x^2-1))*n*(x*P[n](x)-P[n-1](x)) except for x=+-1
;; This program is used with Newton iteration to refine roots of P[n](x).

;;(in-package :qd)

;;(eval-when (compile load)
;;  (declaim (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))
;; (load "qd.dll") 
;;  )

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
	     (setf ans     (cl::/ (cl::- (cl::* (cl::1- (cl::* 2.0d0 i)) x t1) (cl::* (cl::1- i) t0)) i)
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
;; running the integration formula repeatedly is very cheap after
;; abscissae and weights are computed. That is, timing is much
;; faster after the first run at a give order.
(time (gaussunit #'exp 40))
(time (gaussunit #'exp 40))

;; note that we must be using the qd::exp version for this to be accurate.
|#
;; compare to 
;;(defun right ()(let ((k (exp (into 1)))) (- k (/ 1 k)))) ; (right) is answer to above integral

;;  For the middle weight of an odd
;; number of terms, realize that the sum of all the weights will be
;; 2. So if we compute S, the sum weights up to n/2, the remaining
;; weight will be 2*(1 - S).

;; here's where we store precomputed wts and abscissae
(defparameter *legendabs* (make-hash-table))
(defparameter *legendwts* (make-hash-table))

(defun gaussunit(fun n)		; memoize the abscissae and weights
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
			    (aref weights i)))))
      (if (oddp n) (dsetv sum (+ sum (* (aref weights halfn)(funcall fun (into 0))))))
      sum)))

;; pre-compute abscissae and weights
(defun ab_and_wts (n) ;precision is somewhat less than 215, qd. Stores only half the values (maybe +1)
  (let ((a 0)
	(v 0.0d0)
	(np1 (+ n 1))
	(nph (/ 1.0d0(+ n 0.5d0)))
	(halfn (ash n -1))
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
	  (dotimes (k 4)
	  (multiple-value-bind (val deriv)(legendre_pdbf n a)
	   ;;new_guess= guess-f(x)/f'(x)
	   ;; dsetv alters a in place
	    (dsetv a  (- a (/ val deriv)))))
	  (setf (aref abscissae i) a)
	  (setf (aref weights i) (legendre_pdwt np1 a))	)
    (cond ((oddp n)
	   (setf (aref abscissae  halfn) (into 0))
	   (setf (aref weights  halfn) 
      ;; fill in the middle element of an odd list by subtracting the others off
	     (let ((sum (into 0)))
		 (loop for i from 0 to (1- halfn)  do (dsetv sum (+ sum (aref weights i ))))
		 (dsetv sum (- 2 (+ sum sum))))
	   )))
    (values abscissae weights)))

(defun clear-leg-hash()			; call to deallocate storage from hash tables for legendre polys
  (clrhash *legendabs*)
  (clrhash *legendwts*))

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


(defun acsum(a) 
  ;;accurate summation of a[0]+ ...+a[last], all qd numbers
  ;; in an array a. Assume we are free to sort the array.
  ;; experimentation suggests this is not necessary..
  (sort a #'> :key #'abs)
  (reduce #'ga::two-arg-+ a)   )

;; SINH/TANH quadrature.  See 
;;;  http://crd.lbl.gov/~xiaoye/quadrature.pdf

(defun quadts(fun n)
  (let*((piby2(/(compute-pi) 2))
	(h (into(/ 4 n)))
	(sum (* piby2 (funcall fun 0)))
	(he (exp h))
	(relerr (into 1.5193d-64))  ;;(into (expt 2 (- 3 215)))
	(t2 1)t3 t4 ab cor (oldsum (into 0)))
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
	  (cond ((>= j n)
		 (if (= oldsum 0) (if (= sum 0) (return 0)))
		 (if (<(abs(/ (- sum oldsum) 
			      (if (= 0 oldsum)sum oldsum )))
		       relerr)
		     (return (* sum h))))))
    ;; fell out of loop
    (* sum h)))

(defun quadtsab (fun lo hi n) ;; integrate from lo to hi by sinh/tanh
  (let ((a (* 1/2 (- hi lo)))
	(b (* 1/2 (+ hi lo))))
    (* a (quadts #'(lambda(r)(funcall fun (+ b (* a r))))
		 n))))


;; convert from string into qd
(defmethod lisp2qd((s string) (ans aqd));; read integerQinteger like 10Q2   = 100 = 0.1Q3  or  -3.1q-1
  ;; examples of acceptable numbers [put " " around them]
  ;; .1q0 ; 1Q0; 1.2q3;  -1.23q-4;  - .23 q -4;  2.3q+4; +4.5q6; q0; --3Q1 same as 3q1
  ;; 3 ; 3.4; 123.45q+100; -.q0 ; .q0 ;
  ;; examples of not acceptable numbers:
  ;;    q ; .q;   123.45q+100  [=NaN(qd)]
  ;; first check for complex number syntax #q(r i)
  (if (and (>(length s) 1)(char= (aref s 0) #\()) (lisp2qdcomp s)
  (let ((p0 0)(sign 1))
    (setf s (delete #\  s));; remove leading or other spaces from string.
    (if (char= #\- (aref s 0))(setf p0 1 sign -1));; set sign if negative
    (multiple-value-bind (frac pos);; read fraction to left of .
	(parse-integer s :start p0 :radix 10 :junk-allowed t)
      (if (null frac)(setf frac 0));; empty fraction is zero
      (if (= pos (length s)) (* sign (qd::into frac))
	(case (aref s pos);; look at next char
	  ((#\Q #\q);; 10Q2
	   (multiple-value-bind (expon pos2)
	       (parse-integer s :start (1+ pos) ;skip the Q
			      :radix 10)
	     (* sign (qd::into frac) (expt 10 expon))))
	  (#\.;; 
	   (multiple-value-bind (frac2 pos2)
	       (parse-integer s :start (1+ pos);skip the "."
			      :radix 10 :junk-allowed t )
	     (if (null frac2)(setf frac2 0))
	     (setf frac
		   (+ (qd::into frac)
		      (* (if (< frac 0) -1 1) frac2 (expt 10 (1+(- pos pos2))))))
	     ;;  (format t "~% string pos ~s frac= ~s" pos2 frac)
	     (if (= pos2 (length s))(* sign (qd::into frac))
	       (case (aref s pos2)
		 ((#\Q #\q);; 10Q2
		  (multiple-value-bind (expon pos3)
		      (parse-integer s :start (1+ pos2) ;skip the Q
				     :radix 10 )
		    (* sign (qd::into frac)  (expt 10 expon))))
		 (otherwise 
		  (format t "next char is ~a -- Not a qd spec: ~s"  (aref s pos2) s)
		  (into (or frac 0)))))))))))))  

(defun lisp2qdcomp (s)
  (setf s (subseq s 1 (1- (length s)))) ;; remove "(" and ")"; sorry no error checks.
 (multiple-value-bind (re pos1)
    (read-from-string s)
  (setf re (lisp2qd re (qd::into 0)))
  (multiple-value-bind (im pos2)
        (setf s (subseq s  pos1))
      (read-from-string s)
      (setf im (lisp2qd im (qd::into 0)))
      (cons 'complex(list  re im)))))

(defun qd-reader(stream subchar arg)
  (declare (ignore subchar arg))
  (let ((s (read stream)))
   ;; (format t "~% stream got ~s" s)
  (qd::into (format nil "~s" s))))

(set-dispatch-macro-character #\# #\q #'qd-reader)
