;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: zqd; Base: 10 -*-
;; Author: Richard Fateman, Jan, 2006
;; last edit,
;; Sept 25, 2007
;; for details on documentation, see the file  or qd.lisp
;; modified from zmpfr

;; complex numbers here are represented by a structure zqd of 2 qd
;; numbers, a+b*i.  this is not the only way possible. For example, a
;; polar version might be fun.  it is also possible to consider, in
;; this generic arithmetic model, a complex number which is a pair of
;; <anykind1,anykind2> of two kinds of numbers.  Common Lisp insists
;; that it is a pair of two same-kind objects, so that (complex 0.5d0
;; 1/4) looks like #c(0.5d0 0.25d0). This uses the "rule" that
;; floating-pint contaminates rationals, even though rationals in CL are more general.
;; 

(defpackage :zqd				;uses generic arithmetic
  (:use :ga :cl :qd )
  (:shadowing-import-from  
   :ga    
   "+" "-" "/" "*" "expt"		;... n-ary arith
   "=" "/=" ">" "<" "<=" ">="		;... n-ary comparisons
   "1-" "1+" abs incf decf
   min max
   sin cos tan 
   asin acos 
   atan log   ;; these are trickier, 1 or 2 args.
   ;; must add all these like
   sinh cosh tanh 
   asinh acosh atanh sqrt exp
   numerator denominator
   neg erf erfc
   scale-float
   complex real ;imag

   ))


(eval-when '(load) (require "ga")(provide "zqd"))

(in-package :zqd)


(eval-when (compile load eval)
  
  ;; define make-zqd, zqd-re, zqd-im
 (defstruct  zqd  ;; f+i*g
    (re  0 :type qd::aqd)  ;; 0 is not really a qd.
    (im  0 :type qd::aqd)))


;;(defparameter mpfrformat "~a0.~a*10^(~a)" )  ;; -0.123*10^(45)
(defparameter zqdformat "(~a~a.~a^~a, ~a~a.~a^~a)" ) ;;  220+55=>(2.20^2,  5.5^1)

;;?
(defmethod make-load-form ((a zqd)&optional environment)
  (declare (ignore environment))
  (let ((r (ga::outof (zqd-re a)))
	(i  (ga::outof (zqd-im a))))
    `(complex (qd::into ,r)(qd::into ,i) )))

(defun alloc-zqd()  ;; provides an empty zqd
    (qdcomplex (qd::into 0)(qd::into 0)))

;;(defmethod qdcomplex ((r qd::aqd)(i qd::aqd))(zqd::make-zqd :re r :im i))
;;(defmethod qdcomplex ((r qd::aqd) i)(zqd::make-zqd :re r :im (qd::into i)))
;;(defmethod qdcomplex (r (i qd::aqd))(zqd::make-zqd :re (qd::into r) :im i))
(defun qdcomplex (r i)(zqd::make-zqd :re (qd::lisp2qd r (qd::into 0)) 
				     :im (qd::lisp2qd i (qd::into 0)) ))


(defmethod print-object ((a zqd) stream)
  (format stream "(complex ~a ~a)" (qd::print-object (zqd-re a) nil)  (qd::print-object (zqd-im a) nil)))


;; To convert from an ordinary lisp "real" or complex number, use the "into" function.
;;
;; Note that this is zmpfr::into, distinct from programs in other packages.
;; The optional 2nd arg says we already have a place
;; for this number, so in that case we don't need to allocate a new one.



(defmethod qd::lisp2qd((x complex) (ans zqd::zqd))
  (qd::lisp2qd (realpart x) (zqd-re ans))
  (qd::lisp2qd (imagpart x) (zqd-im ans))
  ans)

;;; USE THIS FUNCTION,  into, to convert lisp [complex] number to qd.

(defun qd::into(r &optional (where nil))  ;; overlay qd.lisp def.
  (if (complexp r)
      (qd::lisp2qd r (or where zqd::(alloc-zqd))) ;; where is a zqd, else error
    (qd::lisp2qd r (or where (qd::make-aqd)))))   ;; where is a aqd, else error

;;;**********************

(defun create_zqd_zero() 
 (make-zqd :re (qd::into 0) :im (qd::into 0)))
  
(defmacro defarithmetic (op pgm)
    (let ((two-arg
	   (intern (concatenate 'string "two-arg-" (symbol-name op))
		   :ga ))
	  ;;(c-entry (concatenate 'string "c_"(symbol-name pgm)))
	  )
 ;;           (format t "~% defining ~s" two-arg)
      `(progn

	 ;; new defmethods for zqd.
	 (defmethod ,two-arg ((arg1 zqd) (arg2 zqd))
	   (,pgm arg1 arg2))
	 (defmethod ,two-arg ((arg1 qd::aqd) (arg2 zqd))
	   (,pgm (qdcomplex arg1 0) arg2))
	 (defmethod ,two-arg ((arg1 real) (arg2 zqd))
	   (,pgm (qdcomplex (qd::into arg1) 0) arg2))
	 (defmethod ,two-arg ((arg1 complex) (arg2 zqd))
	   (,pgm  (qd::into arg1) arg2))
	 (defmethod ,two-arg ((arg1 zqd) (arg2 qd::aqd))
	   (,pgm arg1 (qdcomplex arg2 0)))
	 (defmethod ,two-arg ((arg1 zqd) (arg2 real))
	   (,pgm arg1 (qdcomplex (qd::into arg2) 0)))
	 (defmethod ,two-arg ((arg1 zqd) (arg2 complex))
	   (,pgm arg1 (qd::into arg2) ))
       
	 (setf (get ',op 'argnum) 2) ;used by with-temps, dsetv
	 (setf (get ',op 'qd-program) ',pgm) ;used by with-temps, dsetv
	 (setf (get ',two-arg 'qd-program) ',pgm) ;used after macroexpand-all
	 (setf (get ',two-arg 'argnum) 2)
	 
	 )))

(defarithmetic + zqd_add)
(defarithmetic - zqd_sub)
(defarithmetic * zqd_mul)
(defarithmetic / zqd_div)

(defun zqd_add (r s)
  (let ((ans (make-zqd)))
    (setf (zqd-re ans)(+ (zqd-re r)(zqd-re s)))
    (setf (zqd-im ans)(+ (zqd-im r)(zqd-im s)))
    ans))

(defun zqd_sub (r s)
  (let ((ans (make-zqd)))
    (setf (zqd-re ans)(- (zqd-re r)(zqd-re s)))
    (setf (zqd-im ans)(- (zqd-im r)(zqd-im s)))
    ans))

(defun zqd_mul (r s)
  (let ((ans (make-zqd)))
    (setf (zqd-re ans)(- (*(zqd-re r)(zqd-re s))(*(zqd-im r)(zqd-im s))))
    (setf (zqd-im ans)(+ (*(zqd-im r)(zqd-re s))(*(zqd-im s)(zqd-re r))))
    ans))

(defun zqd_div (r s) ;; this could be rearranged to minimize overflow possibilities
  (let* ((ans (make-zqd))
	 (a (zqd-re r))
	 (b (zqd-im r))
	 (c (zqd-re s))
	 (d (zqd-im s)) ;;  (a+bi)/(c+di)
	 (den (+ (* c c)(* d d)))
	 (numreal (+ (* a c)(* b d)))
	 (numimag (- (* b c)(* a d))))
    (setf (zqd-re ans)(/ numreal den))
    (setf (zqd-im ans)(/ numimag den))
    ans))
;; etc


(defmethod qd:abs((z zqd)) 
  (let ((a (zqd-re z))
	(b (zqd-im z)))
    (zqd-re (hypot-qd a b ))))

(defun qd-0(x)(aref (qd::aqd-q x) 0))

(defparameter half  (qd::into 1/2))
(defmethod qd::sqrt((z zqd)) 
  (let* ((a (zqd-re z))
	 (as (qd-0 a)) ;; approx to z, the first 53 bits.
	 (b (zqd-im z))
	 (bs (qd-0 b))
	 (sqr0 (sqrt (complex as bs)))	; appx squareroot of z
	 (sqr1 (into sqr0)) ;; convert approx to qd
	 )
    (cond ((zerop sqr0)(into 0))
	  (t
	   (setf sqr1 (* half (+ sqr1 (/ z sqr1))))
	   (setf sqr1 (* half (+ sqr1 (/ z sqr1)))) ;2 newton iterations, almost there.
	   (setf sqr1 (* half (+ sqr1 (/ z sqr1)))) ;; 3 iterations, for sure overkill.
	   sqr1))))

(defparameter one (into 1))

;; read this file AFTER qd.lisp.
;; change behavior from  sqrt(-...)  --> NaN   to complex result.
(defmethod qd::sqrt((z qd::aqd))
  (if (< z 0)(qd::sqrt (qdcomplex z 0))
    (qd::pos-sqrt z)))

(unless (boundp 'qd::pos-sqrt)
  (setf (symbol-function 'qd::pos-sqrt) (symbol-function 'qd::sqrt))
  (setf qd::pos-sqrt 'set))

;;exp(a+bi) = exp(a)*exp(bi)= exp(a)*(cos*b + i*sin(b)
(defmethod qd::exp((z zqd)) 
  (let* ((a (zqd-re z))
	 (b (zqd-im z))
	 (expa (exp a)))
    (qdcomplex (* expa (cos b))(* expa (sin b)))))

;; log(a+bi) = log(b^2+a^2)/2 +i*atan2(b,a)
(defmethod ga::one-arg-log((z zqd)) 
  (let* ((a (zqd-re z))
	 (b (zqd-im z)))
    (qdcomplex (* half (log (+ (* b b)(* a a)))) 
	       (ga::two-arg-atan b a))))

;; sin(a+bi)= sin(a)*cosh(b)+ i*cos(a)sinh(b)
(defmethod ga::sin((z zqd)) 
  (let* ((a (zqd-re z))
	 (b (zqd-im z)))
    (qdcomplex (* (sin a)(cosh b))
	       (* (cos a)(sinh b)))))


;; cos(a+bi)= cos(a)*cosh(b)- i*sin(a)sinh(b)
(defmethod ga::cos((z zqd)) 
  (let* ((a (zqd-re z))
	 (b (zqd-im z)))
    (qdcomplex (* (cos a)(cosh b))
	       (* -1 (sin a)(sinh b)))))

;; sinh(a+bi) = sinh a cos b + i cosh a sin b
(defmethod ga::sinh((z zqd)) 
  (let* ((a (zqd-re z))
	 (b (zqd-im z)))
    (qdcomplex (* (sinh a)(cos b))
	       (* (cosh a)(sin b)))))

;; cosh(a+bi) = cosh a cos b + i sinh a sin b
(defmethod ga::cosh((z zqd)) 
  (let* ((a (zqd-re z))
	 (b (zqd-im z)))
    (qdcomplex (* (cosh a)(cos b))
	       (* (sinh a)(sin b)))))

(defmethod qd-zerop((z qd::aqd))(= (qd-0 z) 0.0d0))

(defun hypot-qd(x y)
  ;; compute sqrt(x^2+y^2) without overflow if possible.
  (setf x (abs x) y (abs y))
  (cond ((> x y)  (hypot-qd-aux x y))
	((qd-zerop x) (into 0)) ; 0^2+0^2
	(t (hypot-qd-aux y x))))

(defun hypot-qd-aux(x y) ; x is bigger
  (* x(sqrt(+ (sqr (/ y x)) one))))


(defun sqr(a)(* a a))

(defmethod zqd2polar((z zqd))  ;; return r, theta  where z=a+bi = r*exp(i*theta)
    (let* ((a (zqd-re z))
	   (b (zqd-im z)))
      
      (values (hypot-qd a b)
	      (atan b a))))

(defmethod zqdphase((z zqd)) (atan (zqd-im z)(zqd-re z)))
  
#|
(r tan )
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

|#
 

#| Let's say we want to do a few key vector operations without 
boxing and unboxing gmpfr numbers or too many temps. For example, given a
sequence or list of them.  Could also use polynomial eval. Maybe
these are already written in C. 

(defun sum-mpfr-list(L *mpfr-rnd*)  ;; sum of items in a list
  (let* ((sum (create_mpfr_zero))
	 (u (gmpfr-f sum))) ; inside the box
    (loop for i in L do
	  (mpfr_add u u (gmpfr-f i) *mpfr-rnd*))
    sum))


(defun sum-prod-mpfr(L M *mpfr-rnd*)  ;; sum of prods of items in 2 lists or arrays
  (let* ((sum (create_mpfr_zero)) ; or copy a zero..
	 (u (gmpfr-f sum))
	 (temp (gmpfr-f (create_mpfr_zero)))) ;or copy a zero
    (map nil #'(lambda (i j)
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

;; done?
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

;;;; copying with-temps from qd.lisp. See if it works in mpfr-land.

;;;; see mpfr.lisp file for many other functions like fft, rootfinding, quadrature.
|#