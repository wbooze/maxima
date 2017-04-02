;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: gmp; Base: 10 -*-
;; Author: Richard Fateman, Jan, 2006
;; This file is based in part on lisp/loadgmp2.cl  ?2002?
;; which also has polynomial stuff in it.
;; Working univariate polynomial stuff is in lisp/polymultx.cl though.

(eval-when '(load) (require "ga")(provide "gmp"))

;; move this elsewhere, later

(in-package :gmp)

(eval-when (compile load)
  (declaim (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))
  )

;; find a some working gmp library.
;; the next section's syntax is particular for Allegro CL.
;; If I learned about UFFI I suppose it could be done in a uniform way
;; for any of the participating Common Lisp implementations.

;;(load "h:/lisp/gmp-4.1-win32/libgmp-3.dll")
;;(load "libgmp-3.dll")
;;(load "gmp.dll") ;; one should work..

(eval-when (compile load eval)
  (ff:def-foreign-type mpz (cl::* :int))

  (ff:def-foreign-call
   (mpz_init "__gmpz_init")
   ((x (cl::* :int))) 
   :returning :int 
   :arg-checking nil  :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_set_str "__gmpz_set_str")	;changes value of mpz x.
   ((x mpz) 
    (s (cl::* :char))
    (base :int)) 
   :strings-convert t  :returning :int
   :arg-checking nil)
  ;;   :call-direct t
     
  (ff:def-foreign-call 
   (mpz_get_str "__gmpz_get_str")
   ((s :int)
    (base :int) 
    (op mpz)) 
   ;;I think it is ok to let  GMP allocate the space for this string.
   :returning  ((cl::* :char) )
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_get_d  "__gmpz_get_d") ((x mpz)) 
   :returning :double
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call;; remove gmp number from memory
   (mpz_clear  "__gmpz_clear") ((x mpz)) 
   :returning :int
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call;; compare
   (mpz_cmp  "__gmpz_cmp") ((x mpz) (y mpz) ); returns pos if x>y, zero if x=y, neg if x<y
   :returning :int
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_mul  "__gmpz_mul")
   ((target mpz)(op1 mpz)(op2 mpz))
   :returning :void 		         	     
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_pow_ui  "__gmpz_pow_ui") ;;  gmpz^(unsigned int)
   ((target mpz)(op1 mpz)(op2 :long))
   :returning :void 		         	     
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_ui_pow_ui  "__gmpz_ui_pow_ui") ;;  (ui) z^(unsigned int)
   ((target mpz)(op1 :long)(op2 :long))
   :returning :void 		         	     
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_pow_ui  "__gmpz_pow-ui") ;;  gmpz^(unsigned int)
   ((target mpz)(op1 mpz)(op2 :long))
   :returning :void 		         	     
   :arg-checking nil :call-direct t)
    
  
    (ff:def-foreign-call 
   (mpz_mul_si  "__gmpz_mul_si")
   ((target mpz)(op1 mpz)(op2 :int))
   :returning :void 		         	     
   :arg-checking nil :call-direct t)
    
  (ff:def-foreign-call 
   (mpz_add  "__gmpz_add") 
   ((target mpz)(op1 mpz)(op2 mpz)) 
   :returning :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_add_ui  "__gmpz_add_ui");; add unsigned int.  target <- op1 + op2
   ((target mpz)(op1 mpz)(op2 :long)) 
   :returning :void
   :arg-checking nil :call-direct t)
  
    (ff:def-foreign-call 
   (mpz_mul_ui  "__gmpz_mul_ui");; mul unsigned int.  target <- op1* op2
   ((target mpz)(op1 mpz)(op2 :long)) 
   :returning :void
   :arg-checking nil  :call-direct t)

  (ff:def-foreign-call 
   (mpz_sub  "__gmpz_sub") 
   ((target mpz)(op1 mpz)(op2 mpz)) 
   :returning :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_addmul  "__gmpz_addmul");;rop <-rop+op1*op2
   ((rop mpz)(op1 mpz)(op2 mpz)) 
   :returning :void
   :arg-checking nil :call-direct t)
    
  (ff:def-foreign-call;; convert fixnum y to mpz and store in x
   (mpz_set_si  "__gmpz_set_si") ((x mpz) (y :fixnum) )
   :returning :void
   :arg-checking nil    :call-direct t   )

  (ff:def-foreign-call 
   (mpz_mul_2exp  "__gmpz_mul_2exp");; set target to op1*2^op2
   ((target mpz)(op1 mpz)(op2 :long))	;****
   :returning :void
   :arg-checking nil :call-direct t)
  
  ;; sgn is not available because mpz_sgn is a macro
  (ff:def-foreign-call 
   (mpz_size  "__gmpz_size");; number of limbs
   ((op1 mpz))				;****
   :returning :fixnum
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_neg  "__gmpz_neg");; set targ to -op1
   ((targ mpz)(op1 mpz)) 
   :returning :void
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_limbn  "__gmpz_getlimbn");; unsigned byte-32
   ((op1 mpz) (op2 :long))  
   :returning  :long			;**** might be negative?? must make unsigned
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_fdiv_q_2exp  "__gmpz_fdiv_q_2exp");; quotient of op1 div by 2^op2
   ((target mpz)(op1 mpz) (op2 :long))  
   :returning  :void
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_fdiv_r_2exp  "__gmpz_fdiv_r_2exp");; remainder of op1 div by 2^op2
   ((target mpz)(op1 mpz) (op2 :long))  
   :returning  :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_fdiv_qr  "__gmpz_fdiv_qr");; quotient and remainder of op1 div by op2
   ((q mpz) (r mpz)(op1 mpz) (op2 mpz))  
   :returning  :void
   :arg-checking nil :call-direct t)
  (ff:def-foreign-call 
   (mpz_fdiv_q_ui  "__gmpz_fdiv_q_ui");; quotient and remainder of op1 div by op2
   ((q mpz) (op1 mpz) (op2 :long))  
   :returning  :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_sizeinbase "__gmpz_sizeinbase")	;number of base digits in op2 or 1 more
   ((op1 mpz) (op2 :long));; e.g. ceiling log 2 would be (mpz_sizeinbase x 2)
   :returning  :fixnum
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_gcd "__gmpz_gcd")		;greatest common divisor
   ((rop mpz)(op1 mpz) (op2 mpz));;return op is always positive
   :returning  :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_gcdext "__gmpz_gcdext");; extended greatest common divisor
   ((g mpz) (u mpz) (v mpz) (a mpz) (b mpz)) 
   ;; inputs are a and b. Outputs are g, u, v.
   ;; a*u+b*v=g.  g is always positive.
   :returning  :void
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_nextprime "__gmpz_nextprime")	
   ((rop mpz) (op1 mpz))		;set rop to probably the next highest prime
   :returning  :void
   :arg-checking nil :call-direct t)
  
  ;; This is the place to add more interface program declarations.
  ;; We're not even using all the ones above, yet.
  
  ;; There are about 165 gmpz signed integer functions,
  ;; About 35 gmpq rational functions,
  ;; Function entries etc should all be in the gmp.h file
  )

;; If we want to use other parts of gmp, e.g. rationals or reals, we
;; could overload mpfr separately; e.g. another file for mpfr for
;; floating point versions. mpfr makes more sense than integers if we
;; really intend to use sine/cosine/ etc or maybe even interval
;; versions.



;; A GMPZ object p represents an integer:  p in Z, the integers.

;; We set up the Lisp system to keep track of all accessible GMPZ
;; objects with its own garbage collector, but in an indirect fashion.
;; When a GMPZ object is not accessible we signal Lisp to call the GMP
;; library program to clear its own memory. This is done through the;; use of finalizations, which seem to be a feature in many Common
;; Lisp implementations. Here we are using Allegro CL. Other
;; implementations which include finalizations equivalent to Allegro's
;; include LispWorks and Gnu CLISP.  Finalizations not ANSI Standard
;; CL.

;; We define gmpz, GMP integer structure as a structure with only one
;; entry to allow us to use defmethod.  The one entry turns
;; out to be an array that is the gmpz datum.

;; GMPZ defines the structure for a gmp integer, gmpz. 

(defstruct gmpz z)
;;Here z is the "guts" of the repr. A 3-word array whose data
;;are managed by GMP library.

;;How to print a gmpz number so that it is distinguishable from
;;a Lisp number?  Set this format. The first choice makes 123{g}.
;;The second choice makes it indistinguishable from Lisp.
(defvar gmpzformat "~a\{g\}" )  ;; 123{g}
;(defvar gmpzformat "~a" )

(defmethod print-object ((a gmpz) stream)(format stream gmpzformat
						 (create_string_from_mpz a)))

;; Storage deallocation for the gmpz objects requires some delicacy. The
;; guts of a gmz object is NOT garbage-collected automatically by
;; Lisp because it is allocated in GMP-library controlled space.
;; But when the wrapper is about to be reclaimed because no one
;; is looking at it, the finalization method is run on the guts of it,
;; which tells the GMP library to deallocate the major part of the
;; number-- the sequence of "limbs" constituting the integer.  This is
;; handled in alloc-gmpz below.

(defun alloc-gmpz()  ;; provides an empty gmp object for us to use, init to zero
  (let ((inside (make-array 3 :element-type 
			     '(signed-byte 32) 
			     :initial-element 0)))
    (mpz_init inside) 
    ;;This next line sets up Lisp to keep track of this object.
    ;;It sets up GC for what to do later, in case this object
    ;;is not accessible: The entries in the array "inside" will go away.
    ;;The array "inside" as well as the gmpz header will be GC'd later.
    (excl:schedule-finalization inside #'mpz_clear) 
    (make-gmpz :z inside)))

;; Note that mpz_clear is called only when a GC g has found that some
;; given gmpz object Z is inaccessible from Lisp. At the end of that
;; GC, the finalization program is run on Z, so the guts are returned
;; to the GMP storage allocator. At the NEXT GC after g, the object Z
;; goes away as well.


;; create space for a number and put zero in it.
(defun create_mpz_zero() (alloc-gmpz))

(defmethod create_mpz_from_string((s string))  ;; s is a string like "123"
  (let* ((r (alloc-gmpz))
	 (inside (gmpz-z r)))
    (mpz_set_str inside s 10) ;; base 10 number conversion
    r))

(defmethod create_mpz_from_string((s integer))  ;; s isn't a string...
  (lisp2gmpz s))

(defun cs(x)(create_mpz_from_string x))	;short name for above

;; given mpz number 123, return "123"
(defmethod create_string_from_mpz((m gmpz))  (mpz_get_str 0 10 (gmpz-z m)) )

;;; convert gmp number to a lisp number
;;; this is an inefficient hack. How often is it used though?

(defmethod gmpz2lisp((x gmpz))(parse-integer (create_string_from_mpz x)))
(defmethod ga::outof((x gmpz)) (gmpz2lisp x))

;;; convert lisp string or number to gmpz. Bignums treated later.
(defmethod lisp2gmpz((x string));; we hope, a string of decimal digits
  (let* ((a (alloc-gmpz))
	 (r (gmpz-z a)))
     (mpz_set_str r x 10)
     a))

(defmethod lisp2gmpz ((x fixnum))
  (let* ((a (alloc-gmpz))
	 (r (gmpz-z a)))
    (mpz_set_si r x)
    a))

;;; the program below is a slow hack, which sort of writes and then
;;; re-reads the number into a lisp number.  Decomposing a lisp number
;;; word by word and clumping these together to make a GMPZ is
;;; done below in a more elaborate way using btoa.

#+ignore  ;; use only if later definition is no good in your lisp.
(defmethod lisp2gmpz ((x integer))	
    (let* ((a (alloc-gmpz))
	   (r (gmpz-z a)))
      (mpz_set_str r (format nil "~s" x) 10)
      a))

;; we could convert floats which happen to be integers, too.  e.g.
(defmethod lisp2gmpz ((x float)) (lisp2gmpz (round x)))

;; round trip test, returns yes if successful
(defun rtgmpzlisp (x) (if (= x (gmpz2lisp(lisp2gmpz x))) 'yes 'no))

(defmethod create_double_from_mpz((m gmpz)) ;;  creates a double precision version of mpz
  (mpz_get_d (gmpz-z m)))

(defun cd(m)(create_double_from_mpz m))	; short version of above

;; Now we have to do the generic dispatch for two-arg functions,
;; same drill for + - * .
;; for / expt, slightly different.  For / do we want rational?  or
;; truncate?  for expt do we want ordinary integer exponent?

(defmacro defarithmetic (op pgm)
  (let ((two-arg
	 (intern (concatenate 'string "two-arg-" (symbol-name op))
		 :ga )) )
    `(progn
       ;; new defmethods for gmp. 
       (defmethod ,two-arg ((arg1 gmpz) (arg2 gmpz))
	 (let* ((r (create_mpz_zero)) (in (gmpz-z r)))
	   (,pgm in (gmpz-z arg1)(gmpz-z arg2)) r))
       
       (defmethod ,two-arg ((arg1 integer) (arg2 gmpz))
	 (let* ((r (lisp2gmpz arg1))(in (gmpz-z r)))    (,pgm in in (gmpz-z arg2)) r))
       
       (defmethod ,two-arg ((arg1 gmpz) (arg2 integer))
	 (let* ((r (lisp2gmpz arg2))(in (gmpz-z r)))    (,pgm in (gmpz-z arg1) in) r))
       
       (compile ',two-arg)
       (compile ',op)
       ',op)))

(defarithmetic + mpz_add)
(defarithmetic - mpz_sub)
(defarithmetic * mpz_mul)
;;(defarithmetic / mpz_fdiv_qr) ; not yet linked up : returns quotient and remainder both

;; I think qr is like CL's truncate. At least if a,b >0. If b=0, what do we do?
(defmethod qr((a gmpz)(b gmpz))
  (let ((r (create_mpz_zero))
	(q (create_mpz_zero)))
    (mpz_fdiv_qr (gmpz-z q)(gmpz-z r)(gmpz-z a)(gmpz-z b)) 
    (values r q))
  )
;;(defarithmetic expt)

;; We should implement links for (expt GMP  integer)  and (expt GMP GMP)

(defmethod ga::1+((z gmpz))(+ 1 z))
(defmethod ga::1-((z gmpz))(- z 1))

(defmacro defcomparison (op)
  (let ((two-arg (intern (concatenate 'string "two-arg-" 
				      (symbol-name op))    :ga ))
        (cl-op (tocl op)))
    `(progn
        ;; only extra methods not in ga are defined here.
       (defmethod ,two-arg ((arg1 gmpz) (arg2 gmpz))   
	 (,cl-op (mpz_cmp(gmpz-z arg1)(gmpz-z arg2)) 0))
       (defmethod ,two-arg ((arg1 real) (arg2 gmpz))
	 (let ((x (lisp2gmpz arg1)))
	   (,cl-op (mpz_cmp(gmpz-z x)(gmpz-z arg2)) 0)))
       (defmethod ,two-arg ((arg1 gmpz) (arg2 real))
	 (let ((y (lisp2gmpz arg2)))
	   (,cl-op (mpz_cmp(gmpz-z arg2) (gmpz-z y)) 0)))
      (compile ',two-arg)
      (compile ',op)
      ',op)))
(defcomparison >)
(defcomparison =)
(defcomparison <)
(defcomparison <=)
(defcomparison >=)

;; programs that decompose an  Allegro CL number into an
;; array of 16 bit quantities suitable for piling into GMP are
;; in the appendix to papers/polysbyGMP.tex. Copied here, too.

(defmethod lisp2gmpz ((x integer))	;; but not fixnum
  (let* ((out (create_mpz_zero))	;the wrapper
	 (r (gmpz-z out))		;the inside
	 (a (btoa x));; bignum to array.
	 (out2(create_mpz_zero))
	 (h (gmpz-z out2)))
      (do ((i (1- (length a))(1- i)))
	  ((< i 0) r)
	(declare (fixnum i))
	(mpz_set_si h (aref a i))
	(mpz_mul_2exp r r 16)
	(mpz_add r r h))
         (if (< x 0) (setf (aref r 1)(- (aref r 1))))
      out))

;;B to A:  Bignum or fixnum to Array of bigits, 16 bits
(defun btoa(x)				
  (if (excl::fixnump x) (vector (abs x))
  (let* ((size (bignum-size x))
	 (ans (make-array size :element-type '(unsigned-byte 16))))
    (declare (optimize (speed 3)(safety 0)(debug 0))(fixnum size))
    (do ((i 0 (1+ i)))
	((= i size) ans)
      (declare (fixnum i))
      (setf (aref ans i)(nth-bigit x i))))))

(eval-when (compile load)(require :llstructs))
(defun nth-bigit (x n) (sys:memref x -14 (* 2 n) :unsigned-word))
(defun bignum-size(x) ;; in 16-bit quantities 
  (excl::bm_size x))

;; this program is not used...
(defun atob(a)				; array to POSITIVE lisp (bignum)
  (let ((num 0))
    (do ((i (1- (length a))  (1- i)))
	((< i  0) num)
    (setf num (+ (aref a i) (ash num 16))))))

#|
The model for generic arithmetic and overloading here is not congruent
with the gmp design in the following way, which mostly affects efficiency.

We are superimposing a "functional" model on a "state-based" one, and
in the transition we are creating temporary objects too often.  With
GC finalizations, these extra objects will go away without additional
programmer effort, but they are still created.  A closer adherence to the
computation model would probably be better.
Example:
in our model we can compute

(setf a (+ a (* b c)))

but before we complete the operation we must compute
separately, t1=b*c, t2=a+t1, and then pointing a to t2, and then
clearing the old value of a. This can be done without any
intermediate (Lisp) storage by this idiom:

(mpz_addmul (gmpz-z a) (gmpz-z b) (gmpz-z c))

This assumes that no other access route is associated with a's value.

.............

A digression on Factorial functions, benchmarks, timing, etc.


;;  ;; factorial gmp version, vs usual
(defun f(x)(if (zerop x) (cs 1) (* x (f (1- x))))) ;functional version
(defun g(x)(if (zerop x) 1 (* x (g (1- x))))) ; ordinary bignum version

(defun h(x) ;; closer to the real way to do this using gmp numbers
  (let ((ans (cs 1))
	(m (cs 0)))
    (dotimes (i x ans)
      (mpz_add_ui (gmpz-z m)(gmpz-z m) 1) ;; add integer 1 to gmp m
      (mpz_mul  (gmpz-z ans)(gmpz-z ans) (gmpz-z m))))) ;; set ans <- m*ans

;; hacked up version of h. actually m isn't likely to be
;; larger than a fixnum. Who can compute factorial of (2^29) ?
(defun h7(x);;  compute factorial with gmpz
  (assert (and (excl::fixnump x) (> x 0)))
  (let* ((ans (cs 1))     ;; make space for the answer.
	 (g (gmpz-z ans))) ;; g is the inside of gmpz number ans.
    (declare (fixnum m x))
    (do ((i 1 (cl::1+ i)))
	((cl::= i x) ans) 
      (declare (fixnum i))
      (mpz_mul_si g g i)  ;; compute g <- g*i
      )))

;;; an observation: we are not really timing  Bignum X Bignum, since
;;; if you look at how we are computing factorial, we really are
;;; doing Bignum X signed-32bit-integer.  This may or may not be
;;; what you really want to test.


;;; Important note on times.  The time taken by some of these tests
;;; relies heavily on which version of the gmp dll is used.  The
;;; generic C  (no assembler?  or at least no assembler that depends
;;; on which implementation of i86 architecture you have) is definitely
;;; slower.  For example, running on Windsor, a pentium3, the time
;;; to compute (h7 20000) is 2.16 seconds using gc-gmp.dll.
;;; If we use instead the pentium-2 specific version, the time is .48 seconds,
;;; an improvement of 5.4X.

;; for factorial of 20,000, f uses .6 sec, ;; sony vaio time 40,013
;; cons cells, 303,494,640 other bytes, as reported by lisp on
;; windsor, 1.62+0.07 sec and
;;; 60,025 cons cells, 1,600,104 other bytes, 0 static bytes ; newer lib-gmp3.dll from trinity

;; for factorial of 20,000, g uses 1.6 sec + 0.92 sec in GC as well as
;;  40,013 cons cells, 303,494,640 other bytes. 
;; on windsor, 2.98+1.31

;; for factorial of 20,000, h uses .5 sec, 3 cons cells, 160 other bytes
;; on windsor, 0.481 sec
;;  3 cons cells, 160 other bytes, 0 static bytes

;;; This massive massive  reduction in cons cells from 40,000 to 3
;;; and "other bytes" from 3.0e8 to 1.6e2 of six orders of magnitude
;;; is partly illusory: some space is being allocated outside the
;;; view of the GC, in GMP's C-allocated "foreign" space.

;; at least as reported by lisp.  So using functional interface
;; does not seem to slow it down in this test, but see another
;; test later in this file -- same programs-- different gmp
;; where the result is different.


More times, this comparing  a pentium-4 specific version of GMP on 
Windsome, a 2.5GHz pentium to the C-only version.
factorial of 20,000.
f: 0.49+0.14 sec   60,000 +1.6million    ;; vs 1.6+0.04sec with non assembler
g: 1.1 +0.63 sec   40,000 +303 million   ;; ordinary lisp
h: 0.188sec        3 cons cells 160 other bytes  ;; vs 1.3+0  with non assembler

Mathematica 5.1, which probably uses GMP for this, 
reports that it computes factorial of 20,000 in 0.047 sec on Windsome
Maple 7 reports a time of 0.297 sec. which is probably also from GMP.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Why is Mathematica 5.1 so fast at computing factorial? 

Simple: it does not use the naive factorial algorithm, but a
variant using recursive-splitting for computing n!. A fairer
Mathematica calculation to compare to our naive factorial would be
Product[i,{i,1,20000}] which takes twice as long as 20000!, but is
still rather fast.

Here's a recursive factorial that is especially nice for large
numbers if it is efficient to multiply two numbers of about equal size,
and probably what Mathematica uses.

(defun k (n m) ;; (k n 1) is n!
  (if (<= n m) (cs n)
    (* (k n (* 2 m))
       (k (- n m)(* 2 m)))))

;; or declared  better  and using shift instead of mult by 2 ...

(defun k2 (n m) ;; (k n 1) is n!
  (declare (fixnum n m))
  (if (cl::<= n m) (cs n)
    (* (k2 (cl::- n m)(setf m (ash m 1)))
       (k2 n m)
       )))

;; Now here's a mysterious factorial which uses no extra storage, but its
;; not faster because it multiplies small X big.

(defun kfact(n)(kfact1 n 1 1))

;; version for integers
(defun kfact1 (n m ans) ;; (kfact n 1 1) is n!
  (declare (fixnum n m))
  (if (<= n m) (* n ans)
    (let ((q (ash m 1)))
    (kfact1 n q (kfact1 (- n m) q ans)))))

;;
;; Here's a nice version showing how a minor piece of
;; representation hacking in GMPZ could be done
(defun k3top(n)   ;;(k3top n)  is n! pretty fast
  (let ((a (alloc-gmpz)))
    (k3 n 1 (gmpz-z a))
    a))
(defun k3 (n m ans)	;; ans is a gmp number "insides"
  (declare (fixnum n m))
  (if(cl::<= n m)(mpz_set_si ans n)
	(let ((b (gmpz-z(alloc-gmpz))))
	  (k3 (- n m) (setf m(ash m 1)) b)
	  (k3 n m ans)
	  (mpz_mul ans ans b))))

;; the problem with k3 above is that it uses lots of storage, O(n),
;; by use of (alloc-gmpz), most of it briefly. 
;;  Actually only a few storage locations O(log_2(n)) are needed
;; at any one time. The next program pre-allocates sufficient
;; cells and then manages them explicitly.
;; I also used labels and local environment to capture the resource res.

(defun k4(n);; using resource instead of allocation each time. Faster!!
  (declare (fixnum n)(optimize (speed 3)(safety 0)))
  (let ((res nil))
    (labels
	((k4i (n m ans)			; ans is a gmp number.
	   (declare (fixnum n m)
		    (type (simple-array  (signed-byte 32) (3))  ans))
	   (if(cl::<= n m)(mpz_set_si ans n)
	    (let ((b (vector-pop res)))
	       (declare (type (simple-array  (signed-byte 32) (3))  b))
	       (k4i (cl::- n m) (setf m(ash m 1)) b)
	       (k4i n m ans)
	       (mpz_mul ans ans b)
	       (incf (fill-pointer res)) ; (vector-push b gmpres)
	       ))))
      (let* ((L (ceiling (cl::log n 2)))
	     (a (alloc-gmpz)))
	(declare (fixnum L))
	(setf res (make-array L  :fill-pointer 0))
	;; allocate enough space for temps.
	;; fill the temps with gmp numbers, zeros.  Just the insides though.
	(dotimes (i L)(vector-push  (gmpz-z (alloc-gmpz)) res))
	(k4i n 1 (gmpz-z a))
	a))))

;; (k  20000 1) takes  .241 sec on windsor. 
;; (k2 20000 1) takes  .190 sec on windsor. 
;; (k3top 20000)takes  .130 sec on windsor using p2-libgmp-3.dll
;; (k4 20000)  takes   .071 sec on windsor using  ditto
;;  Mathematica takes  .07  sec on windsor.

;; Here's another variant. This version also works pretty hard..
;; but is not faster.

(defun k4x(n);; 
  (declare (fixnum n))
  (let ((res nil))
    (labels
	((k4i (n m ans)			; ans is a gmp number.
	   (declare (fixnum n m))

	   (cond((cl::<= n m)(mpz_set_si ans n))
		;; try to factor out 2^q from factorial 		
		((and (cl::= m 2)(evenp n))
		 (let ((q (ash n -1)))
		   (declare (fixnum q))
		      (k4i q 1 ans)
		      (mpz_mul_2exp ans ans q)))
		((and (cl::> m 1)(cl::= 0 (mod n m)))
		 (let ((b (vector-pop res))
		       (q (cl:/ n m)))
		   (declare (fixnum q))
		   ;; try to factor out n^q from factorial 		
		   (mpz_ui_pow_ui b m q)
		   (k4i q 1 ans)
		   (mpz_mul ans ans b)
		   (incf (fill-pointer res))))
		   
		(t(let ((b (vector-pop res)))
		       (k4i (cl::- n m) (setf m(ash m 1)) b)
		       (k4i n m ans)
		       (mpz_mul ans ans b)
		       (incf (fill-pointer res)) ; (vector-push b gmpres)
		  )))))
      (let* ((L (* 2 (ceiling (cl::log n 2))))
	     (a (alloc-gmpz)))
	(declare (fixnum L))
	(setf res (make-array L  :fill-pointer 0))
	;; allocate enough space for temps.
	;; fill the temps with gmp numbers, zeros.  Just the insides though.
	(dotimes (i L)(vector-push  (gmpz-z (alloc-gmpz)) res))
	(k4i n 1 (gmpz-z a))
	a))))
(defun k4x (n m ans)			; ans is a gmp number.
  (declare (fixnum n m))
  (cond ((cl::<= n m)(mpz_set_si (gmpz-z ans) n) ans)
	;; this next clause factors out 2^n.  but doesn't speed up answer!
	((and (cl::= m 2)(evenp n))
	 (let  ((q (ash n -1)))
	   (declare (fixnum q m))
	   (k4x q 1 ans)
	   (mpz_mul_2exp (gmpz-z ans)(gmpz-z ans) q)
	   ans
	  ))
	(t
	(let ((b (alloc-gmpz)))
	  (k4x (- n m) (setf m(ash m 1)) b)
	  (k4x n m ans)
	  (mpz_mul (gmpz-z ans)(gmpz-z ans)(gmpz-z b))
	  ans))))

There are many more methods, some involving factoring: to compute 100! all
we need to compute is 2^97 * 3^48 * 5^24 * 7^16 * 11^9 * 13^7 * 17^5 *
19^5 * 23^4 * 29^3 * 31^3 * 37^2 * 41^2 * 43^2 * 47^2 * 53 * 59 * 61 *
67 * 71 * 73 * 79 * 83 * 89 * 97.  see
http://www.luschny.de/math/factorial/index.html 

This bit of discussion here goes to show that this benchmark, as is
the case for other benchmarks, does not represent the most efficient
way of computing the answer, but just a way of using computer
resources in some systematic fashion, watching how some resource
utilization varies as we modify some relatively simple parameter such
as language implementation, language usage, choice of computer, etc.

... end of digression on Factorial computation.

Further notes:  after writing the QD interface, and revising the
MPFR interface to look like the QD interface, the question arises as to
whether we should revise GMP interface to look the same.  Since the GMP
and MPFR arithmetic are likely to be much slower in their actual
computing, inefficiency in the linkage would be much more tolerable.

Do we need to write with-temps and dsetv for GMP? I think not.

We could do a wrapper for gmp rationals, I guess. Or make interfaces
for the number-theoretic stuff in gmp.


Trying to clear the air about different versions of GMP 4.1.4 for
different architectures.

Consider the computation of 20,000!  by the program k4, above.
This needs access to the GMP library.
Which library?  We tested this all on a pentium 4.

(time (progn (dotimes (i 10)(k4 20000)) nil))

with generic code for any i86, written in C:  1,219 ms.
with some code specifically for pentium 0:    1,250 ms.
with some code specifically for pentium 3:      641 ms.
with some code specifically for pentium 4:      437 ms.



|#

;; maxfactx can be done nicely in gmpz, and is 
;; about 5X faster on windsor with generic gmp library;
;; with pentium-2 version, it is 14X to 16X faster.
;; cutting short the iteration by memoization would also
;; help.  20,000! in 70ms.

(setf  *print-circle* t)

(defun gmaxfactx (n)
  (declare (fixnum n)(optimize (speed 3)(safety 0)))
  (let* ((z (if (< n 100) 5 100))
	 (aloop (gcloop z))) ; heuristic
    (loop for  i from (1+ z)  to n
	do  ;;(setf (car aloop) (* (car aloop) i))
	  (mpz_mul_si (car aloop) (car aloop) i)
	    (setf aloop (cdr aloop)))
    (gloopprod aloop)))

(defun gcloop(n) ;;   (cloop 5) is #1=(5 4 3 2 1 . #1#) with gmpz insides 
  (let*((start (list (gmp::gmpz-z (gmp::into 1))))
	(end start))
    (dotimes (i (1- n) (setf (cdr end) start))
      (setq start (cons (gmpz-z (into (+ 2 i))) start)))))

(defun gloopprod (l) 
  ;; efficient product of all the approximately equal
  ;; numbers in the loop trying to keep the sizes of inputs
  ;; approximately balanced.  the circular loop l is destroyed in the
  ;; process.
  (declare (optimize (speed 3)(safety 0)))
  (cond ((eq(cdr l) l)(make-gmpz :z (car l)))
	(t ;;(setf (car l)(* (car l)(cadr l)))
	 (mpz_mul (car l)(car l)(cadr l))
	   (setf (cdr l)(cddr l))
	   (gloopprod (cdr l)))))

;;;; here is a lisp-only version of the above, for comparison.


(defun cloop(n) ;;   (cloop 5) is #1=(5 4 3 2 1 . #1#)
  (let*((start (list 1))
	(end start))
    (dotimes (i (1- n) (setf (cdr end) start))
      (setq start (cons (+ 2 i) start)))))

(defun loopprod (l) 
  ;; efficient product of all the approximately equal
  ;; numbers in the loop trying to keep the sizes of inputs
  ;; approximately balanced.  the circular loop l is destroyed in the
  ;; process.
  (declare (optimize (speed 3)(safety 0)))
  (cond ((eq(cdr l) l)(car l))
	(t (setf (car l)(* (car l)(cadr l)))
	   (setf (cdr l)(cddr l))
	   (loopprod (cdr l)))))


(defun maxfactx (n)
  (declare (fixnum n)(optimize (speed 3)(safety 0)))
  (let* ((z (if (< n 100) 5 100))
	 (aloop (cloop z))) ; heuristic
    (loop for  i from (1+ z)  to n
	do  (setf (car aloop) (* (car aloop) i))
	    (setf aloop (cdr aloop)))
    (loopprod aloop)))





;;;; trying to keep powers of 2 out of the loop... ;; broken

;;; do only the ODD numbers, i.e. 
#|
(defun cloop(n begin step) ;;   (begin, begin+step , ... wrap around, with n items)
  (let*((start (list begin))
	(end start)
	(count (+ begin (* step n))))
    
    (do((i (+ step begin) (+ i step)))
	((>= i count)  (setf (cdr end) start))
      (setq start (cons i start)))))

(defun loopprod (l) 
  ;; efficient product of all the approximately equal
  ;; numbers in the loop trying to keep the sizes of inputs
  ;; approximately balanced.  the circular loop l is destroyed in the
  ;; process.
  (declare (optimize (speed 3)(safety 0)))
  (cond ((eq(cdr l) l)(car l))
	(t (setf (car l)(* (car l)(cadr l)))
	   (setf (cdr l)(cddr l))
	   (loopprod (cdr l)))))


(defun maxfactx (n)
  (declare (fixnum n)(optimize (speed 3)(safety 0)))
  (let* ((z (if (< n 100) 1 20))
	 (aloop (cloop z 3 2))
	 (shift (floor z 2))
	 )	
    (loop for  i from (+ 3 (* 2 z)) to n
	do  (setf (car aloop) (* (car aloop) i))
	    (setf aloop (cdr aloop)))
    (print shift)
    (ash (loopprod aloop) (+ shift (floor n 2)))))





|#

;;play games with kg

(defun kg (n m) 
  (declare (fixnum n m)) ;; tell lisp that n, m are not very big.
    (cond ((and (= m 2)(evenp n)) 
           (ash (kg (ash n -1) 1) (ash n -1)))
          ((<= n m) n)
          (t (* (kg n (ash m 1))
                (kg (- n m)(ash m 1))))))

(defun kg (n m) 
  (declare (fixnum n m)) ;; tell lisp that n, m are not very big.
  (cond ((and (= m 2)(evenp n)) 
           (ash (kg (ash n -1) 1) (ash n -1)))
          ((<= n m) n)
          (t (* (kg n (ash m 1))
                (kg (- n m)(ash m 1))))))

(in-package :user)

(defun kgtop (n) 
  (let ((shift 0))
    (declare(special shift))
    (ash (kg1 n 1) shift)))

(defun kg1 (n m)
  (declare (fixnum n m)
	   (special shift))
  (cond ((and (= m 2)(evenp n)) 
	 (incf shift (ash n -1))
           (kg1 (ash n -1) 1))
          ((<= n m) n)
          (t (* (kg1 n (ash m 1))
                (kg1 (- n m)(ash m 1))))))




(defun gkg(n &optional (aa (alloc-gmpz)));put answer in 2nd arg, if there
  (declare (fixnum n)(optimize (speed 3)(safety 0)(debug 0)))
  (let ((res nil) (shift 0)
        (L (ceiling (cl::log n 2)))
        (a (gmpz-z aa)))
    (declare (fixnum L shift n))
    (labels
        ((k (n m ans res)
	    (declare (fixnum n m))
	    (cond
	     ((and (cl::evenp n)(cl::> m 1)) 
	      (cl::incf shift (cl::ash n -1))
	      (k (cl::ash n -1) (cl::ash m -1) ans res))
	     ((cl::<= n m)(mpz_set_si ans n))
	     (t(let ((b (car res)))
		 (k (cl::- n m) (setq m (cl::ash m 1)) b (cdr res))
		 (k n m ans (cdr res))
		 (mpz_mul ans ans b)
		 ans)))))

      (dotimes (i L)(push (gmpz-z (alloc-gmpz)) res));set up resource
      (k n 1 a res)
      (mpz_mul_2exp a a shift)
      aa)))



;;;;;;;;;;;;;;
;;;; combining gmp, memoization, split-recursive, 

(defun kgmin (n m min) ;; (k n 1 1) is n!
  (declare (fixnum n m min))
  (cond ((< n min) 1)
	((<= n m) n)
	((and (evenp m)(evenp n)(evenp min))
	 (cl::ash (kgmin (cl::ash n -1) (cl::ash m -1) (cl::ash min -1)) 
	      (1+ (cl::ash (- n min) -1))))
	(t (* (kgmin n (cl::ash m 1) min)
	      (kgmin (- n m)(cl::ash m 1) min)))))

;; (* (gkgmin 100 1 50)(kgmin 49 1 1))  is the same as (gkg 100 1 1) 

(defun gkgmin(n min &aux (aa (alloc-gmpz)));;   
  (declare (fixnum n)(optimize (speed 3)(safety 0)(debug 0)))
  (let ((res nil) (shift 0)
        (L (ceiling (cl::log n 2)))
        (a (gmpz-z aa)))
    (declare (fixnum L shift n))
    (labels
        ((k (n m ans res min)
	    (declare (fixnum n m))
	    (cond
	     ((cl::< n min) (mpz_set_si ans 1))
	     ((cl::<= n m)(mpz_set_si ans n))
	     ((and (evenp n)(evenp m)(evenp min))
	      (cl::incf shift (1+ (cl::ash (- n min) -1)))
	      (k (cl::ash n -1) (cl::ash m -1) ans res (cl::ash min -1)))

	     (t(let ((b (car res)))
		 (k (cl::- n m) (setq m (cl::ash m 1)) b (cdr res) min)
		 (k n m ans (cdr res) min)
		 (mpz_mul ans ans b)
		 ans)))))

      (dotimes (i L)(push (gmpz-z (alloc-gmpz)) res));set up resource
      (k n 1 a res min)
      (mpz_mul_2exp a a shift)
      aa)))
;;main program.
(defun gkmemfac(n)
  (let ((z (lookupfact n)))
  ;; find z, the largest integer <= n such that we know z!
  (if (= (car z) n)(cdr z)
    (rememberfact n 
		  (* (gkgmin n (1+ (car z)))
		     (cdr z))))))

(defun lookupfact(n)
  (loop for i from 0 to (length oldfacts) 
	do (if (<=  (car (aref oldfacts i)) n)
	       (return  (aref oldfacts  i)))))

(defun rememberfact(n f)
  (vector-push-extend (cons n f) oldfacts)
  (setf oldfacts (sort oldfacts #'> :key #'car))
  f)

(defparameter oldfacts (make-array 0 :adjustable t :fill-pointer t))

(defun clearfact()(setf oldfacts (make-array 0 :adjustable t :fill-pointer t))
       (vector-push-extend (cons 0 (into 1)) oldfacts))
(clearfact)				; clear it now.
;;vs

