;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: dd; Base: 10 -*-
;; Author: Richard Fateman, Jan, 2006
;; double-double

;(load "qd.dll")
(defpackage :dd
  (:use :ga :cl)
    (:shadowing-import-from  
   :ga    
   "+" "-" "/" "*" "expt"		;... n-ary arith
   "=" "/=" ">" "<" "<=" ">="		;... n-ary comparisons
   "1-" "1+" "abs" "incf" "decf"
   "min" "max"
   asin log tan atanh acos sin sinh cosh cos sqrt exp atan
   "tocl" "re-intern" 
   )
  )

(in-package :dd)

(eval-when (compile load)
  (declaim (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))
  )

(eval-when (compile load eval)
  (ff:def-foreign-type cdd (cl::* :double))
  (ff:def-foreign-type cdd (cl::* :double))

 
  (ff:def-foreign-call
   (dd_mul  "c_dd_mul")
   ((op1 cdd)(op2 cdd) (target cdd))
   :returning :void 		         	     
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call
   (dd_add  "c_dd_add")
   ((op1 cdd)(op2 cdd) (target cdd))
   :returning :void 		         	     
   :arg-checking nil :call-direct t)
  (ff:def-foreign-call
   (dd_sub  "c_dd_sub")
   ((op1 cdd)(op2 cdd) (target cdd))
   :returning :void 		         	     
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call
   (dd_div  "c_dd_div")
   ((op1 cdd)(op2 cdd) (target cdd))
   :returning :void 		         	     
   :arg-checking nil :call-direct t)
  )

;; extend the list of operations with other entry points as
;; needed. There are 120 external entry points for dd and dd
;; routines. See the file qd.lisp for more comments.

(defun init-dd() 
  (make-array 2 :element-type 'double-float :initial-element 0.0d0))

;;If we want to tag these guys we can do something like this:

(defstruct add :q)  
;; Here q is the "guts" of the repr. An array of doubles.
;; The structure "add" provides a wrapper or tag so that
;; we can hang methods on it, like print-object, +, * etc.

(defmethod print-object ((a add) stream)(format stream "~a"
						 (dd2string a)))

(defun alloc-dd()  ;; provides an empty dd, with a tag so lisp knows it
    (make-add :q (init-dd)))

;; The next program converts a dd number to a lisp rational.
;; probably a "more efficient" way to do this; note denom is a power of 2

(defmethod dd2lisp((x add))
  (or (excl::nan-p (aref (add-q x) 0)) ; if dd contains a NaN, use it.
      (apply #'+  (map 'list #'rational (add-q x)))))

;; To convert from an ordinary lisp "real" number one of these should do:

(defmethod lisp2dd((x rational)) ;to encode a lisp rational, divide num/den
  (let ((ans (init-dd))
	(nu (add-q (lisp2dd (numerator x))))
	(de (add-q (lisp2dd (denominator x)))))
    (dd_div nu de ans)
    (make-add :q ans)))

(defmethod lisp2dd((x fixnum))  ;small integers are easy.
  (lisp2dd (coerce x 'double-float)))

(defmethod lisp2dd ((x double-float)) ;so are double-floats
    (let ((ans (init-dd)))
    (setf (aref ans 0) x)
    (make-add :q ans)))

(defmethod lisp2dd((x single-float)) ;so are single-floats
 (lisp2dd (coerce x 'double-float)))
  
(defmethod lisp2dd ((x integer)) 
  ;; integers that are shorter than a double-float fraction are easy.
  (if (< (cl::abs x) #.(cl::expt 2 53)) (lisp2dd  (coerce x 'double-float))
    ;; the rest of this code is for when x is a bignum.
    ;; We convert it, 52-bit section by 52-bit section
    (let ((ans (init-dd))
	  (s (signum x))
	  (shifter (add-q (lisp2dd 1))) ;initially, shift by 1
	  (newdig 0)
	  (shiftam(add-q (lisp2dd #.(expt 2 52))) ))
      
      (if (< s 0)(setf x (- x)))
      (loop while (> x 0) do
	    (setf newdig (logand x #.(cl::1- (expt 2 52))))
	    (setf newdig (add-q (lisp2dd newdig))) ; grab some bits
	    (dd_mul shifter newdig newdig) ;newdig now a dd
	    (setf x (ash x -52)) ;remove them from x
	    (dd_add ans newdig ans)
	    (dd_mul shifter shiftam shifter))
      (if (< s 0)(dd_mul ans (lisp2dd -1) ans))
      (make-add :q ans))))

;; should be faster ways of shifting and changing sign in the above.
  

;; Converting from dd to lisp to decimal (or other base) string we
;; look at the first n decimal digits of a fraction, also show exponent.
;; We only use base 10 in dd2string.

(defun decimalize(r n &optional (base 10))	; r rational number >=0
  (let* ((expon  (if (= r 0) 0 (floor(cl::log (cl::abs r) base) )))
	 (frac (round (*(cl::abs r) (expt base (- n expon))))))
    
       (values   (signum r)
	        frac
		(if (= frac 0) 0 (cl::1+ expon)))))

;; make a formatted output . Something like this..
;; this is  used by print-object.

(defparameter *dd-digits-to-show* 33)

(defmethod dd2string((x add))
  ;; check if x is a NaN
  (if (excl::nan-p (aref (add-q x) 0)) "NaN(dd)"
    (multiple-value-bind (s r e h)  ;h is extra variable
	(decimalize (dd2lisp x) *dd-digits-to-show* 10)
      (format nil "~a0.~add~s" (if (< s 0)"-" "") 
	      (string-right-trim "0"  
				 (subseq (setf h(format nil "~a" r)) 0  
					(cl::min (length h) *dd-digits-to-show*)) )
				 e))))
    

(defmacro defarithmetic (op pgm)
  (let ((two-arg
	 (intern (concatenate 'string "two-arg-" (symbol-name op))
		 :ga )) )
    `(progn
       ;; new defmethods for dd. .. note order of args different from gmp
       (defmethod ,two-arg ((arg1 add) (arg2 add))
	 (let* ((r (alloc-dd)) (in (add-q r)))
	   (,pgm (add-q arg1)(add-q arg2) in) r))
       
       (defmethod ,two-arg ((arg1 real) (arg2 add))
	 (let* ((r (lisp2dd arg1))(in (add-q r)))
	   (,pgm in (add-q arg2) in) r))
       
       (defmethod ,two-arg ((arg1 add) (arg2 real))
	 (let* ((r (lisp2dd arg2))(in (add-q r))) 
	   (,pgm (add-q arg1) in in) r))
       
       (compile ',two-arg)
       (compile ',op)
       ',op)))

(defarithmetic + dd_add)
(defarithmetic - dd_sub)
(defarithmetic * dd_mul)
(defarithmetic / dd_div)

;; do analogous stuff for other entry points,

(defmacro r (op);;
  (let ((fun-name (intern op :ga ))
	(string-entry (format nil "~a~s" "c_dd_" op))
	(lisp-name-for-entry (intern (format nil "~a~s" "dd_" op))))
		      
    `(progn
       (ff:def-foreign-call
	   (,lisp-name-for-entry
	    ,string-entry);; e.g. c_dd_sin
	((op1 cdd)(target cdd))
	:returning :void 		         	     
	:arg-checking nil :call-direct t)
	   
       (defmethod ,fun-name ((arg add))
	 (let* ((h (alloc-dd)) (in (add-q h)))
	   (,lisp-name-for-entry (add-q arg) in) h))
       (compile ',fun-name))))

(r sin) (r cos)  (r tan)
(r atan)(r asin) (r acos)
(r sinh)(r cosh) (r atanh)
(r exp) (r log)  (r abs)


