;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: mpf; Base: 10 -*-

;; this is the gmp mpf   float stuff. mpfr not here.. 
;; look at mpfr for Reliable   or Rounding MPF.

;; Author: Richard Fateman, Jan, 2006
;; This file is based in part on lisp/generic/gmp.lisp

(eval-when '(load) (require "ga")(provide "mpf"))

;; move this defpackage elsewhere, later
(defpackage :mpf				;uses generic arithmetic
  (:use :ga :cl)
  (:shadowing-import-from  
   :ga    
   "+" "-" "/" "*" "expt"		;... n-ary arith
   "=" "/=" ">" "<" "<=" ">="		;... n-ary comparisons
   "1-" "1+" "abs" "incf" "decf"
   "min" "max"
   asin log tan atanh acos sin sinh cosh cos sqrt exp atan
   "tocl" "re-intern" 
   ) )
(in-package :mpf)

(eval-when (compile load)
  (declaim (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))
  )

;; find some working gmp library.
;; the next section's syntax is particular for Allegro CL.
;; If I learned about UFFI I suppose it could be done in a uniform way
;; for any of the participating Common Lisp implementations.

;;(load "h:/lisp/gmp-4.1-win32/libgmp-3.dll")
;(load "libgmp-3.dll")


(eval-when (compile load eval)
  (ff:def-foreign-type mpf (cl::* :int))

  (ff:def-foreign-call
   (mpf_init "__gmpf_init")
   ((x (cl::* :int))) 
   :returning :int 
   :arg-checking nil  :call-direct t)
  
  (ff:def-foreign-call
   (mpf_init2 "__gmpf_init2")
   ((x (cl::* :int))
    (y  :int));; set precision to at least y bits.
   :returning :int 
   :arg-checking nil  :call-direct t)

  (ff:def-foreign-call
   (mpf_set_default_prec "__gmpf_set_default_prec")
   ((prec  :int));; set precision to at least y>0 bits for all future variables or const.
   :returning :void 
   :arg-checking nil  :call-direct t)
  
    (ff:def-foreign-call
	(mpf_set_z "__gmpf_set_z")
	((target  mpf)
	 (somempz  (cl::* :int)) )
   :returning :void 
   :arg-checking nil  :call-direct t)
    
    (ff:def-foreign-call
	(mpf_set_d "__gmpf_set_d")
	((target  mpf)
	 (somedoub  :double))
   :returning :void 
   :arg-checking nil  :call-direct t)
    
  (ff:def-foreign-call 
   (mpf_set_str "__gmpf_set_str")
      ;;changes value of mpf x. string is mmm@nnn  
      ;; the fraction is .mmm, and the exponent is nnn.  base is 
      ;; 2 to 36 or -36 to -2, where negative base means exponent
      ;; is in decimal.  returns 0 if string is valid else -1.
   ((x mpf) 
    (s (cl::* :char))
    (base :int))			; usually 10
   :strings-convert t  :returning :int
   :arg-checking nil)
 
  (ff:def-foreign-call 
   (mpf_get_str "__gmpf_get_str")
   ((s :int)				;if nil, a result string will be allocated  sr
    (expptr (cl::* :char))		;? pointer to exponent
    (base :long) 
    (ndigits :int);;if 0, as many digits as significant
    (op mpf)) 
   :returning  ((cl::* :char)string )
   :arg-checking nil :call-direct nil)
  
  ;;I wonder if this will work?? Nah, not much chance
  ;;(ff:def-foreign-call
  ;;      (mpf_out_str "__gmpf_out_str")
  ;;       ((s (* :int))  ;;; a stream really
  ;;	(base :int)
  ;;	(ndigits :int)
  ;;	(op mpf))
  ;;       :returning :int)
 
  (ff:def-foreign-call 
   (mpf_get_d "__gmpf_get_d")
   ((op mpf)) 
   :returning  :double
   :arg-checking nil :call-direct t)
    
  (ff:def-foreign-call 
   (mpf_get_default_prec "__gmpf_get_default_prec")
   () ;;((v :void))  ;ignored
   :returning   :long 
   :arg-checking nil :call-direct t)
 
  (ff:def-foreign-call 
   (mpf_get_prec "__gmpf_get__prec")
   ((op mpf));; return the precision of the particular gmpf number here.
   :returning   :long 
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpf_set_prec "__gmpf_set_prec")
      ((op mpf);; set precision of the particular gmpf number here.
       (a :long))
   :returning   :void 
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call;; remove gmp number from memory
   (mpf_clear  "__gmpf_clear") ((x mpf)) 
   :returning :int
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpf_mul  "__gmpf_mul")
   ((target mpf)(op1 mpf) (op2 mpf))  
   :returning  :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpf_add  "__gmpf_add")
   ((target mpf)(op1 mpf) (op2 mpf))  
   :returning  :void
   :arg-checking nil :call-direct t)
  
    (ff:def-foreign-call 
   (mpf_sub  "__gmpf_sub")
   ((target mpf)(op1 mpf) (op2 mpf))  
   :returning  :void
   :arg-checking nil :call-direct t)
  
    (ff:def-foreign-call 
   (mpf_add_ui  "__gmpf_add_ui")
   ((target mpf)(op1 mpf) (op2 :long))  
   :returning  :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpf_cmp  "__gmpf_cmp")
   ((op1 mpf) (op2 mpf))  
   :returning  :int
   :arg-checking nil :call-direct t)
  

  (ff:def-foreign-call 
   (mpf_div "__gmpf_div");; quotient  of op1 div by op2
   ((q mpf) (op1 mpf) (op2 mpf))  
   :returning  :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpf_div_q_ui  "__gmpf_div_q_ui");; quotient and remainder of op1 div by op2
   ((q mpf) (op1 mpf) (op2 :long))  
   :returning  :void
   :arg-checking nil :call-direct t)
  ;; Function entries etc should all be in the gmp.h file
  ;; A GMPF object p represents a float:  p 
  )

(defstruct gmpf f)
;;Here f is the "guts" of the repr. A 4-word array whose data
;;are managed by GMP library.

;;How to print a gmpf number so that it is distinguishable from
;;a Lisp number? 

;;Set this format.
(defparameter gmpfformat "~a0.~a*10^(~a)" )  ;; -0.123*10^(45)

;; next print method works for low precision: just convert to ordinary Lisp's
;; double float as best as we can.

#+ignore
(defmethod print-object ((a gmpf) stream)(format stream "~s"
						 (create_double_from_mpf a)))
;; a better method

(defmethod print-object ((a gmpf) stream)
  (let ((sign ""))
  (multiple-value-bind (frac expon)
      (create_string_from_mpf a)
    (cond((eql (aref frac 0) #\-) ;; check for negative fraction
	  (setf frac (subseq frac 1))(setf sign "-")))
    (format stream gmpfformat sign frac expon))))

;; Storage deallocation for the gmpf objects is done the same as
;; for the gmz objects described in gmp.lisp.

(defun alloc-gmpf()  ;; provides an empty gmp object for us to use, init to zero
  (let ((inside (make-array 4 :element-type 
			     '(signed-byte 32) 
			     :initial-element 0)))
    (mpf_init inside) 
    (excl:schedule-finalization inside #'mpf_clear)   ;could be 'mpf_clear
    (make-gmpf :f inside)))

;; create space for a float number and put zero in it.
(defun create_mpf_zero() (alloc-gmpf))

(defmethod create_mpf_from_string((s string) (e string))  
  ;; s is a string like "123"
  ;; e is a string like "4"
  ;; produces   .123 X 10^4
  (let* ((r (alloc-gmpf))
	 (inside (gmpf-f r)))
    (mpf_set_str inside 
		 (concatenate 'string s "@" e)
		 10) ;; base 10 number conversion
    r))

(defmethod create_mpf_from_string((s integer) (e integer))  ;; s isn't a string...
  (lisp2gmpf2 s e))

(defun csf(x e)(create_mpf_from_string x e)) ;short name for above
(defvar stexpon (make-array 1 :element-type '(signed-byte 32) :initial-element 0))

(defmethod create_string_from_mpf((m gmpf)) 
  ;; return 2 values, the fraction and the exponent, each as a string.
    (let ((r (mpf_get_str 0 stexpon 10 0 (gmpf-f m)))) 
   ; (excl:schedule-finalization r 'mpf_free_str) ;; find this entry..
  (values
   r
   (elt stexpon 0))))


(defun cs(m)(create_string_from_mpf m))

;;; convert gmp number to a lisp number
;;; this is an inefficient hack. How often is it used though?

;;(defmethod gmpf2lisp((x gmpf))(parse-integer (create_mpf_from_string x e)))

;;; convert lisp string or number to gmpf. Bignums treated later.
(defmethod lisp2gmpf2((x string) (e string))
  (create_mpf_from_string x e))

(defmethod lisp2gmpf2 ((x integer) (e integer))
  (create_mpf_from_string (format nil "~s" x) (format nil"~s" e)))

(defmethod lisp2gmpf ((x integer))(lisp2gmpf2 x 0))

(defmethod lisp2gmpf ((x rational))
  (/ (lisp2gmpf (numerator x))(denominator x)))

(defmethod lisp2gmpf((x gmp::gmpz))
  (let ((s1 (alloc-gmpf)))
    (mpf_set_z  (gmpf-f s1) (gmp::gmp-z x))
    s1))
  
(defmethod lisp2gmpf ((x double-float))
    (let ((s1 (alloc-gmpf)))
    (mpf_set_d (gmpf-f s1) x)
    s1))

(defmethod lisp2gmpf ((x single-float))
  (lisp2gmpf (* 1.0d0 x)))

(defmethod create_double_from_mpf((m gmpf)) ;;  creates a double precision version of mpf
  (mpf_get_d (gmpf-f m)))

(defun cd(m)(create_double_from_mpf m))	; short version of above

;; Now we have to do the generic dispatch for two-arg functions,
;; same drill for + - * /
;; for expt, slightly different.  

;; we might also want to have native access to
;; (op gmpf  double)
;; (op gmpf  signed-long-int)
;; (op double gmpf)
;; (op sign-long-int gmpf)
;; instead of converting the non-gmpf to a gmpf.  (Not done here)

;;; also note that the ordinary functional interface allocates
;;; a new gmpf object for the target, and the precision of this target
;;; determines the precision of the result.  How to set the precision?

(defmacro defarithmetic (op pgm)
  (let ((two-arg
	 (intern (concatenate 'string "two-arg-" (symbol-name op))
		 :ga )) )
    `(progn
       ;; new defmethods for gmpf. 
       (defmethod ,two-arg ((arg1 gmpf) (arg2 gmpf))
	 (let* ((r (create_mpf_zero)) (in (gmpf-f r)))
	   (,pgm in (gmpf-f arg1)(gmpf-f arg2)) r))
       
       (defmethod ,two-arg ((arg1 integer) (arg2 gmpf))
	 (let* ((r (lisp2gmpf arg1))(in (gmpf-f r)))
	   (,pgm in in (gmpf-f arg2)) r))
       
       (defmethod ,two-arg ((arg1 gmpf) (arg2 integer))
	 (let* ((r (lisp2gmpf arg2))(in (gmpf-f r)))   
	   (,pgm in (gmpf-f arg1) in) r))
       
       (compile ',two-arg)
       (compile ',op)
       ',op)))

(defarithmetic + mpf_add)
(defarithmetic - mpf_sub)
(defarithmetic * mpf_mul)
(defarithmetic / mpf_div)

;;what about (defarithmetic expt)
#+ignore
(defmethod ga::expt ((x gmpf) (n integer))) ;; there is mpf_pow_ui for pos n
;; otherwise need log, exp?

;;; some tests!
  
;;  ;; factorial gmpf version, vs usual
(defun f(x)(if (zerop x) (csf 1 0) (* x (f (1- x))))) ;functional version
(defun g(x)(if (zerop x) 1 (* x (g (1- x))))) ; ordinary bignum version

(defun h(x) ;; the real way to do this using gmpf numbers
  (let ((ans (csf 1 0))
	(m (csf 0 0)))
    (dotimes (i x ans)
      (mpf_add_ui (gmpf-f m)(gmpf-f m) 1) ;; add 1 to m
      (mpf_mul  (gmpf-f ans)(gmpf-f ans) (gmpf-f m))))) ;; set ans <- m*ans


;; for factorial of 20,000, f uses 1.6 sec,  ;; pentium4 windsome
;;  260,179 cons cells, 3,354,128 other bytes, 0 static bytes

;; h uses  0.15 sec  MUCH FASTER
;;  31 cons cells, 328 other bytes, 0 static bytes

;; oh, the answer is about
;;0.181920632023034509021*10^(77338)

;; the ordinary lisp version, g gets the exact answer
;; in .998+.2(gc) sec
;;  40,034 cons cells, 303,495,064 other bytes, 48 static bytes

;; at least as reported by lisp.  So using functional interface
;; makes a big difference here, a factor of 10.
;; using gmp-4.1.4 compiled on mingw for pentium4.


(defmethod ga::1+((z gmpf))(+ 1 z))
(defmethod ga::1-((z gmpf))(- z 1))

;; we could do mpf_sqrt, mpf_neg mpf_abs
;; mpfmul_2exp(a b intc)  a<-b*2^c.  i.e. a shift left
;; mpfdiv_2exp(a b intc)  a<-b*2^(-c).  i.e. a shift right

(defmacro defcomparison (op)
  (let ((two-arg (intern (concatenate 'string "two-arg-" 
				      (symbol-name op))    :ga ))
        (cl-op (tocl op)))
    `(progn
        ;; only extra methods not in ga are defined here.
       (defmethod ,two-arg ((arg1 gmpf) (arg2 gmpf))   
	 (,cl-op (mpf_cmp(gmpf-f arg1)(gmpf-f arg2)) 0))
       (defmethod ,two-arg ((arg1 real) (arg2 gmpf))
	 (let ((x (lisp2gmpf arg1)))
	   (,cl-op (mpf_cmp(gmpf-f x)(gmpf-f arg2)) 0)))
       (defmethod ,two-arg ((arg1 gmpf) (arg2 real))
	 (let ((y (lisp2gmpf arg2)))
	   (,cl-op (mpf_cmp(gmpf-f arg2) (gmpf-f y)) 0)))
      (compile ',two-arg)
      (compile ',op)
      ',op)))
(defcomparison >)
(defcomparison =)
(defcomparison <)
(defcomparison <=)
(defcomparison >=)


