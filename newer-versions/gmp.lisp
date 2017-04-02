;;; -*- mode: lisp; syntax: common-lisp; package: gmp; base: 10 -*-
;; author: richard fateman, jan, 2006
;; this file is based in part on lisp/loadgmp2.cl  ?2002?
;; which also has polynomial stuff in it.
;; working univariate polynomial stuff is in lisp/polymultx.cl though.

(eval-when (:load-toplevel) (require "ga")(provide "gmp"))
(in-package :gmp)

(eval-when (:compile-toplevel :load-toplevel )
  (declaim (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))
  )


;; if i learned about uffi i suppose it could be done in a uniform way
;; for any of the participating common lisp implementations.
;; load a working gmp library.
;;(load "h:/lisp/generic/p4/gmp.dll") ;; if this is a pentium 4
;;(load "h:/lisp/generic/p3/gmp.dll") ;; if this is a pentium 3
;;(load "h:/lisp/generic/p0/gmp.dll") ;; if this is a pentium 

#|trying to clear the air about different versions of gmp 4.1.4 for
different architectures. 

consider the computation of 20,000!  by the program k4, below.
how much does it matter which library we use?  

we tested this first bunch on a pentium 4.  (we do the computation 10 times)

(time (progn (dotimes (i 10)(k4 20000)) nil))

with generic code for any i86, written in c:  1,219 ms.   pentium-generic/gmp.dll
with some code specifically for pentium 0:    1,250 ms.   p0/gmp.dll
with some code specifically for pentium 3:      641 ms.   p3/gmp.dll
with some code specifically for pentium 4:      437 ms.   p4/gmp.dll

using a pentium 3

with generic code for any i86, written in c:  2,083 ms.   pentium-generic/gmp.dll
with some code specifically for pentium 0:      792 ms.   p0/gmp.dll
with some code specifically for pentium 3:      791 ms.   p3/gmp.dll
with some code specifically for pentium 4:      <coredump>p4/gmp.dll

oh, using mpz_fac, directly from gmp            661 ms.

;;(progn (setf x (into 0))(mpzfac (gmpz-z x) 20000))
using sprfact, 588 ms.
using gkg, 831 ms. ??
using gmaxfactx 779 ms ??


|#

;;(load "gmp.dll") ;; one should work..
;; the next section's syntax is particular for allegro cl.
(eval-when (compile load eval)
  (ff:def-foreign-type mpz (:array :int))

  (ff:def-foreign-call
   (mpz_init "__gmpz_init")
   ((x (cl::* :int))) 
   :returning :int 
   :arg-checking nil  :call-direct t)
  
    (ff:def-foreign-call
   (mpz_init2 "__gmpz_init2")
	((x (cl::* :int))
	 (len :int) )  ;; space for how many bits?
   :returning :int 
   :arg-checking nil  :call-direct t)
    
        (ff:def-foreign-call
   (mpz_fac "__gmpz_fac_ui") ;; factorial
	((x (cl::* :int))
	 (n :int) )  ;; compute n!
   :returning :int 
   :arg-checking nil  :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_set_str "__gmpz_set_str")	;changes value of mpz x.
   ((x mpz  (simple-array (signed-byte 32) (3))) 
    (s (cl::* :char))
    (base :int)) 
   :strings-convert t  :returning :int
   :arg-checking nil)
  ;;   :call-direct t
     
  (ff:def-foreign-call 
   (mpz_get_str "__gmpz_get_str")
   ((s :int)
    (base :int) 
    (op mpz  (simple-array (signed-byte 32) (3)))) 
   ;;i think it is ok to let gmp allocate the space for this string.
   :returning  ((cl::* :char) )
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_get_d  "__gmpz_get_d") ((x mpz  (simple-array (signed-byte 32) (3)))) 
   :returning :double
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call;; remove gmp number from memory
   (mpz_clear  "__gmpz_clear") ((x mpz  (simple-array (signed-byte 32) (3)))) 
   :returning :int
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call;; compare
   (mpz_cmp  "__gmpz_cmp") ((x mpz  (simple-array (signed-byte 32) (3))) (y mpz  (simple-array (signed-byte 32) (3))) ); returns pos if x>y, zero if x=y, neg if x<y
   :returning :int
   :arg-checking nil :call-direct t)
  

    (ff:def-foreign-call 
   (mpz_mul  "__gmpz_mul")
	((target mpz (simple-array (signed-byte 32) (3)))
	 (op1 mpz (simple-array (signed-byte 32) (3)))
	 (op2 mpz (simple-array (signed-byte 32) (3))))
   :returning :void 		         	     
   :arg-checking nil :call-direct t :strings-convert nil)
  
  
  (ff:def-foreign-call 
   (mpz_pow_ui  "__gmpz_pow_ui");;  gmpz^(unsigned int)
   ((target mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3)))(op2 :long))
   :returning :void 		         	     
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_ui_pow_ui  "__gmpz_ui_pow_ui");;  (ui) z^(unsigned int)
   ((target mpz  (simple-array (signed-byte 32) (3)))(op1 :long)(op2 :long))
   :returning :void 		         	     
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_mul_si  "__gmpz_mul_si")
   ((target mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3)))(op2 :int))
   :returning :void 		         	     
   :arg-checking nil :call-direct t)
    
  (ff:def-foreign-call 
   (mpz_add  "__gmpz_add") 
   ((target mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3)))(op2 mpz  (simple-array (signed-byte 32) (3)))) 
   :returning :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_add_ui  "__gmpz_add_ui");; add unsigned int.  target <- op1 + op2
   ((target mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3)))(op2 :long)) 
   :returning :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_mul_ui  "__gmpz_mul_ui");; mul unsigned int.  target <- op1* op2
   ((target mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3)))(op2 :long)) 
   :returning :void
   :arg-checking nil  :call-direct t)

  (ff:def-foreign-call 
   (mpz_sub  "__gmpz_sub") 
   ((target mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3)))(op2 mpz  (simple-array (signed-byte 32) (3)))) 
   :returning :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_addmul  "__gmpz_addmul");;rop <-rop+op1*op2
   ((rop mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3)))(op2 mpz  (simple-array (signed-byte 32) (3)))) 
   :returning :void
   :arg-checking nil :call-direct t)
    
  (ff:def-foreign-call;; convert fixnum y to mpz and store in x
   (mpz_set_si  "__gmpz_set_si") ((x mpz  (simple-array (signed-byte 32) (3))) (y :fixnum) )
   :returning :void
   :arg-checking nil    :call-direct t   )


  (ff:def-foreign-call 
   (mpz_mul_2exp  "__gmpz_mul_2exp");; set target to op1*2^op2
   ((target mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3)))(op2 :long))	;****
   :returning :void
   :arg-checking nil :call-direct t)
  
  
  (ff:def-foreign-call 
      (mpz_divexact_ui  "__gmpz_divexact_ui");; set target to op1/op2
      ;; if not evenly divisible, gives nonsense.
   ((target mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3)))(op2 :long))	;****
   :returning :void
   :arg-checking nil :call-direct t)
  
    (ff:def-foreign-call 
   (mpz_divexact  "__gmpz_divexact");; set target to op1/op2
   ((target mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3)))(op2 mpz  (simple-array (signed-byte 32) (3))))	;****
   :returning :void
   :arg-checking nil :call-direct t)
    
  (ff:def-foreign-call 
   (mpz_rshift  "__gmpz_tdiv_q_2exp");; like rightshift for pos op1
   ((target mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3)))(op2 :long))	;****
   :returning :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_divisible_2exp_p  "__gmpz_divisible_2exp_p");; returns non-zero if divisble by 2^u
   ((op1 mpz  (simple-array (signed-byte 32) (3)))(u :long))	;****
   :returning :int
   :arg-checking nil :call-direct t)
  
  
  (ff:def-foreign-call 
      (mpz_probab_prime_p  "__gmpz_probab_prime_p")
      ;; returns 2 if definitely prime, 1 if probably prime,
      ;; 0 if definitely composite (not prime)
      ((op1 mpz  (simple-array (signed-byte 32) (3)))(repetitions :long))
    ;; how many tests to run. 5-10 is good.
    ;; more will reduce chance of composite being labeled probably prime
   :returning :int
   :arg-checking nil :call-direct t)
  
  
  
  
  ;; sgn is not available because mpz_sgn is a macro
  (ff:def-foreign-call 
   (mpz_size  "__gmpz_size");; number of limbs
   ((op1 mpz  (simple-array (signed-byte 32) (3))))				;****
   :returning :fixnum
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_neg  "__gmpz_neg");; set targ to -op1
   ((targ mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3)))) 
   :returning :void
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_limbn  "__gmpz_getlimbn");; unsigned byte-32
   ((op1 mpz  (simple-array (signed-byte 32) (3))) (op2 :long))  
   :returning  :long			;**** might be negative?? must make unsigned
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_fdiv_q_2exp  "__gmpz_fdiv_q_2exp");; quotient of op1 div by 2^op2
   ((target mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3))) (op2 :long))  
   :returning  :void
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_fdiv_r_2exp  "__gmpz_fdiv_r_2exp");; remainder of op1 div by 2^op2
   ((target mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3))) (op2 :long))  
   :returning  :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_fdiv_qr  "__gmpz_fdiv_qr");; quotient and remainder of op1 div by op2
   ((q mpz  (simple-array (signed-byte 32) (3))) (r mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3))) (op2 mpz  (simple-array (signed-byte 32) (3))))  
   :returning  :void
   :arg-checking nil :call-direct t)
  (ff:def-foreign-call 
   (mpz_fdiv_q_ui  "__gmpz_fdiv_q_ui");; quotient and remainder of op1 div by op2
   ((q mpz  (simple-array (signed-byte 32) (3))) (op1 mpz  (simple-array (signed-byte 32) (3))) (op2 :long))  
   :returning  :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_sizeinbase "__gmpz_sizeinbase")	;number of base digits in op2 or 1 more
   ((op1 mpz  (simple-array (signed-byte 32) (3))) (op2 :long));; e.g. ceiling log 2 would be (mpz_sizeinbase x 2)
   :returning  :fixnum
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_gcd "__gmpz_gcd")		;greatest common divisor
   ((rop mpz  (simple-array (signed-byte 32) (3)))(op1 mpz  (simple-array (signed-byte 32) (3))) (op2 mpz  (simple-array (signed-byte 32) (3))));;return op is always positive
   :returning  :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_gcdext "__gmpz_gcdext");; extended greatest common divisor
   ((g mpz  (simple-array (signed-byte 32) (3))) (u mpz  (simple-array (signed-byte 32) (3))) (v mpz  (simple-array (signed-byte 32) (3))) (a mpz  (simple-array (signed-byte 32) (3))) (b mpz  (simple-array (signed-byte 32) (3)))) 
   ;; inputs are a and b. outputs are g, u, v.
   ;; a*u+b*v=g.  g is always positive.
   :returning  :void
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_nextprime "__gmpz_nextprime")	
   ((rop mpz  (simple-array (signed-byte 32) (3))) (op1 mpz  (simple-array (signed-byte 32) (3))))		;set rop to probably the next highest prime
   :returning  :void
   :arg-checking nil :call-direct t)
  
  ;; this is the place to add more interface program declarations.
  ;; we're not even using all the ones above, yet.
  
  ;; there are about 165 gmpz signed integer functions,
  ;; about 35 gmpq rational functions,
  ;; function entries etc should all be in the gmp.h file
  )

;; if we want to use other parts of gmp, e.g. rationals or reals, we
;; could overload separately; e.g. another file for mpfr for
;; floating point versions. mpfr makes more sense than integers if we
;; really want to use sine/cosine/ etc or maybe even interval
;; versions.  just gmpz has some functions of interest to number theory. 

;; a gmpz object p represents an integer:  p in z, the integers.

;; we set up the lisp system to keep track of all accessible gmpz
;; objects with its own garbage collector, but in an indirect fashion.
;; when a gmpz object is found, by the garbage collector, to be
;; inaccessible, the lisp gc can't just free it; it doesn't know how.
;; instead we signal lisp to call the gmp library program to clear its
;; own memory. this is done through the use of finalizations, which
;; are not required by the ansi standard but seem to be a feature in
;; many common lisp implementations. here we are using allegro
;; cl. other implementations which include finalizations equivalent to
;; allegro's include lispworks and gnu clisp.

;; we define gmpz, gmp integer structure as a structure with only one
;; entry to allow us to use defmethod.  the one entry turns
;; out to be an array that is the gmpz datum.

(defstruct gmpz z)
;;here z is the "guts" of the repr. a 3-word array whose data
;;are managed by gmp library.

;;how to print a gmpz number so that it is distinguishable from
;;a lisp number?  set this format. the first choice makes 123{g}.
;;the second choice makes it indistinguishable from lisp.
(defvar gmpzformat "~a\{g\}" )  ;; 123{g}
;(defvar gmpzformat "~a" )  ;;another possibility

(defmethod print-object ((a gmpz) stream)(format stream gmpzformat
						 (create_string_from_mpz a)))

;; storage deallocation for the gmpz objects requires some delicacy. the
;; guts of a gmz object is not garbage-collected automatically by
;; lisp because it is allocated in gmp-library controlled space.
;; but when the wrapper is about to be reclaimed because no one
;; is looking at it, the finalization method is run on the guts of it,
;; which tells the gmp library to deallocate the major part of the
;; number-- the sequence of "limbs" constituting the integer.  this is
;; handled in alloc-gmpz below.

(defun alloc-gmpz()  ;; provides an empty gmp object for us to use, init to zero
  (let ((inside (make-array 3 :element-type 
			     '(signed-byte 32) 
			     :initial-element 0)))
    (mpz_init inside) 
    ;;this next line sets up lisp to keep track of this object.
    ;;it sets up gc for what to do later, in case this object
    ;;is not accessible: the entries in the array "inside" will go away.
    ;;the array "inside" as well as the gmpz header will be gc'd later.
    (excl:schedule-finalization inside #'mpz_clear) 
    (make-gmpz :z inside)))

(defun alloc-gmpz2(n)  ;; same as alloc-gmpz but initialize to n bits long
  (let ((inside (make-array 3 :element-type 
			     '(signed-byte 32) 
			     :initial-element 0)))
    (mpz_init2 inside n) 
    (excl:schedule-finalization inside #'mpz_clear) 
    (make-gmpz :z inside)))

;; note that mpz_clear is called only when a gc g has found that some
;; given gmpz object z is inaccessible from lisp. at the end of that
;; gc, the finalization program is run on z, so the guts are returned
;; to the gmp storage allocator. at the next gc after g, the object z
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
(defun into(x)(lisp2gmpz x))

(defmethod lisp2gmpz((x gmpz)) x)

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
;;(defun rtgmpzlisp (x) (if (= x (gmpz2lisp(lisp2gmpz x))) 'yes 'no))

(defmethod create_double_from_mpz((m gmpz)) ;;  creates a double precision version of mpz
  (mpz_get_d (gmpz-z m)))

;;(defun cd(m)(create_double_from_mpz m))	; short version of above

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

;; rational division is not possible with ring of integers.
;;(defarithmetic / mpz_fdiv_qr) ; not yet linked up : returns quotient and remainder both

;; I think qr is like CL's truncate. At least if a,b >0. If b=0, what do we do?
(defmethod qr((a gmpz)(b gmpz))
  (let ((r (create_mpz_zero))
	(q (create_mpz_zero)))
    (mpz_fdiv_qr (gmpz-z q)(gmpz-z r)(gmpz-z a)(gmpz-z b)) 
    (values r q))
  )
;;(defarithmetic expt) hm. can't do negative exponent.
;; not sure we want to do 
;; (expt GMP GMP)

(defmethod ga::two-arg-expt ((z gmpz) (u fixnum))
  (assert (>= u 0))
  (let ((ans (alloc-gmpz)))
    (mpz_pow_ui (gmpz-z ans) (gmpz-z z) u)
    ans))

(defmethod ga::two-arg-expt ((z gmpz) (u gmpz))
  (error "not implemented gmpz ^gmpz")
  (assert (>= u 0))
  (let ((ans (alloc-gmpz)))
    ;; not implemented. if u is small, use previous.
   ;; (mpz_pow (gmpz-z ans) (gmpz-z z)(gmpz-z u))
    ans))


(defmethod ga::1+((z gmpz))(+ 1 z))
(defmethod ga::1-((z gmpz))(- z 1))

(defun fact(x)(let((ans (alloc-gmpz))) ;; factorial
		(mpz_fac (gmpz-z ans) x)
		ans))

(defmethod ga::ash((z gmpz)(n fixnum))  ;; arithmetic shift
  (let((ans (alloc-gmpz)))
    (cond ((> n 0) (mpz_mul_2exp (gmpz-z ans) (gmpz-z z) n) ans)
	  ((< n 0) (mpz_rshift (gmpz-z ans) (gmpz-z z) (- n)) ans)
	  (t z))))

(defun probably_primep(z &optional(reps 5))
  ;; 2=yes, prime.  0= no, composite.  1= probably prime.
  ;; increase reps for more repetitions of the test.
  (setf z (gmp::into z))
  (mpz_probab_prime_p (gmpz-z z) reps))

(defun next_prime(z)
  ;; 2=yes, prime.  0= no, composite.  1= probably prime.
  ;; increase reps for more repetitions of the test.
  (let ((ans (alloc-gmpz)))
    (setf z (gmp::into z))
    (mpz_nextprime (gmpz-z ans) (gmpz-z z))
    ans))

(defun gmp_gcd  (a b)
  (let((ans (alloc-gmpz)))
    (setf a (gmpz-z(into a)))
    (setf b (gmpz-z(into b))) ;; make both into gmpz numbers if not already
    (mpz_gcd (gmpz-z ans) a b)
    ans))

(defun gmp_gcdext  (a b)
  (let((ss (alloc-gmpz))
       (tt (alloc-gmpz))
       (gg (alloc-gmpz)))
    (setf a (gmpz-z(into a)))
    (setf b (gmpz-z(into b))) ;; make both into gmpz numbers if not already
    (mpz_gcdext  (gmpz-z gg) (gmpz-z ss)(gmpz-z tt) a b)
    (values gg ss tt)))  ;returns g,s,t  such that g=a*s+b*t and g>0


(defmethod ga::evenp((z gmpz))  
  (> (mpz_divisible_2exp_p (gmpz-z z)  1) 0))

(defmethod ga::oddp((z gmpz))  
    (= (mpz_divisible_2exp_p (gmpz-z z)  1) 0))

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


(defmethod lisp2gmpz ((x integer))	;  x is lisp bignum, answer is gmp number ;;8/14/2010
  (declare (optimize (speed 3)(safety 0)))
  (let* ((out (create_mpz_zero))	;the wrapper
	 (r (gmpz-z out))		;the inside
	 (negsign (if (< x 0) t)))
    (if negsign (setf x (- x)))
    (if (typep x 'fixnum)(mpz_add_ui r r x)
      (do ((i (1-(excl::bm_size x))(1- i)))
	  ((< i 0) r)
	(declare (fixnum i))
	(mpz_mul_2exp r r 16)
	(mpz_add_ui  r r  (nth-bigit x (+ i i)))	))
    (if negsign (mpz_neg r r))
    out))


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


;;;;;;;;;;;;;; programs for largish factorials
;;; just using gmp::
(defun gmpfac(x)
 (let ((r (into 0)))
  (assert (typep x 'fixnum))
  (mpz_fac (gmpz-z r) x)
  r))
;;;; combining gmp, memoization, split-recursive, 

(defparameter oldfacts (make-array 0 :adjustable t :fill-pointer t))

(defun kgmin (n m min) ;; (k n 1 1) is n!
  (declare (fixnum n m min))
  (cond ((< n min) 1)
	((<= n m) n)
	((and (evenp m)(evenp n)(evenp min))
	 (ash (kgmin (ash n -1) (ash m -1) (ash min -1)) 
	      (1+ (ash (- n min) -1))))
	(t (* (kgmin n (ash m 1) min)
	      (kgmin (- n m)(ash m 1) min)))))

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
	      (cl::incf shift (1+ (ash (- n min) -1)))
	      (k (ash n -1) (ash m -1) ans res (ash min -1)))

	     (t(let ((b (car res)))
		 (k (cl::- n m) (setq m (ash m 1)) b (cdr res) min)
		 (k n m ans (cdr res) min)
		 (mpz_mul ans ans b)
		 ans)))))

      (dotimes (i L)(push (gmpz-z (alloc-gmpz)) res));set up resource
      (k n 1 a res min)
      (mpz_mul_2exp a a shift)
      aa)))
;;main program.
(defun gkmemfac(n)
  (if (null oldfacts) (clearfact))
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


(defun clearfact()(setf oldfacts (make-array 0 :adjustable t :fill-pointer t))
       (vector-push-extend (cons 0 (into 1)) oldfacts))
;(clearfact)				; clear it now.


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

;;A digression on Factorial functions, benchmarks, timing, etc.

;;see paper .. h:/papers/factorial.tex  or 
;;http://www.cs.berkeley.edu/~fateman/papers/factorial.pdf

;; factorial gmp version, vs usual
;; These functions are variants on the usual, as it happens, not
;; very fast ways of computing factorial. We include these to
;; show how to take a simple function and elaborate upon it
;; to make better (i.e. more efficient) use of gmp.

(defun f(x)(if (zerop x) (cs 1) (* x (f (1- x))))) ;functional version
(defun g(x)(if (zerop x) 1 (* x (g (1- x))))) ; ordinary bignum version

(defun h(x) ;;  a version that avoids allocating extra space.
  (let ((ans (into 1))
	(m (into 0)))
    (dotimes (i x ans)
      (mpz_add_ui (gmpz-z m)(gmpz-z m) 1) ;; add integer 1 to gmp m
      (mpz_mul  (gmpz-z ans)(gmpz-z ans) (gmpz-z m))))) ;; set ans <- m*ans

;; A hacked up version of h, Since m isn't likely to be
;; larger than a fixnum. Who can compute factorial of (2^29) ?

(defun h7(x);;  compute factorial with gmpz
  (assert (and (typep x 'fixnump) (> x 0)))
  (let* ((ans (cs 1))     ;; make space for the answer.
	 (g (gmpz-z ans))) ;; g is the inside of gmpz number ans.
    (declare (fixnum m x))
    (do ((i 1 (cl::1+ i)))
	((cl::= i x) ans) 
      (declare (fixnum i))
      (mpz_mul_si g g i)  ;; compute g <- g*i
      )))

;;; an observation: we are not really benchmarking  Bignum X Bignum, since
;;; if you look at how we are computing factorial, we really are
;;; doing Bignum X signed-32bit-integer.  This may or may not be
;;; what you really want to test.

;;; On a pentium 3, (h7 20000) takes 489 ms. The GMP  factorial
;;; program takes 67 ms.  Is this because C is faster than Lisp?
;;; Well, that's not the reason, because a DIFFERENT way of
;;; computing factorial, in Lisp, but using arithmetic from GMP
;;; is about as fast.

Here's a recursive factorial that is especially nice for large
numbers if it is efficient to multiply two numbers of about equal size,
and probably what Mathematica uses. Actually, even on ordinary Lisp
it is a better algorithm because much of the work in the lower levels
of the tree that is eventually all multiplied together, is done in
ordinary single-word fixnum arithmetic, so some of the dealing with bigfloats
is avoided.  Other approaches are almost immediately thrown into the bignum
arena having to multiply smallish numbers (like n) by large large numbers:
so most work is bignum arithmetic.

(defun k (n m) ;; (k n 1) is n!
  (if (<= n m) (cs n)
    (* (k n (* 2 m))
       (k (- n m)(* 2 m)))))

;; or declared better and using shift instead of mult by 2 ...

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


;; Here's a nicer version showing how a minor piece of
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

;; The problem with k3 above is that it uses lots of storage, O(n). It is
;; using (alloc-gmpz) too much. Most of it very briefly.  In reality
;; only a few distinct storage locations, O(log_2(n)) are needed at any
;; one time. The next program k4 pre-allocates sufficient cells and then
;; manages them explicitly.  We also used labels and local environment
;; to capture the resource, res, which is a vector of scratch space for gmpzs.
;; It was surprisingly easy to set up scratch space here. This program k4
;; is about 3X faster than k3.

(defun k4(n);; using resource instead of allocation each time. Faster!!
  (declare (fixnum n))
  (let ((res nil))
    (labels
	((k4i (n m ans)			; ans is a gmp number.
	   (declare (fixnum n m))

	   (if(cl::<= n m)(mpz_set_si ans n)
	     (let ((b (vector-pop res)))
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
	;; the spaces in this vector will get enlarged as needed, pretty fast.
	(k4i n 1 (gmpz-z a))
	a))))

;;; from luschny, translated from Java to Lisp

(defun gsprfact(n)
  (declare (fixnum n))  
  (let*  ((pp (into 1)) (p (gmpz-z pp))
	  (rr (into 1)) (r (gmpz-z rr))
	  (NN 1)
	  (mlog2n (-(floor (log n 2))))
	  (h 0) (shift 0) (high 1) (len 0) (ans (gmpz-z (into 1)))
	  (res (make-array(1+ (- mlog2n)) :fill-pointer 0)))
    (declare (fixnum mlog2n log2n h shift high len NN))
    (labels ((gprod(n ans)
	       (declare (fixnum n))
	       (let ((m (ash n -1)))
		 (cond ((cl::= m 0) 
			(mpz_set_si ans (incf NN 2)))
		       ((cl::= n 2)
			(mpz_set_si ans (cl::* (cl::incf NN 2)(cl::incf NN 2))))
		       (t (let ((b (vector-pop res))) ;b is a temporary gmpz
			     (gprod m b) ;set b
				(gprod (cl::- n m) ans) ;set ans
				(mpz_mul ans b ans) ;set ans
				(cl::incf (fill-pointer res)) ; remove b
				)))
		     ans)))
      
  	(dotimes (i  (1+ (- mlog2n)))
	  (vector-push  (gmpz-z (alloc-gmpz2 256)) res)) ;initialize

	(loop while (cl::/= h n) do
	      (cl::incf shift h)
	      (setf h (ash n mlog2n))
	      (cl::incf mlog2n)
	      (setf len high)
	      (setf high (if (oddp h) h (cl::1- h)))
	      (setf len (ash (cl::- high len) -1))
	      (cond ((cl::> len 0)
			(mpz_mul p p (gprod len ans))
			(mpz_mul r r p)
			)))
	(mpz_mul_2exp r r shift)
	rr)))

;; split-recursive like the program above, but more lisp-like.
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
To see many more ideas for how to compute factorials of very large
numbers relatively fast, some involving factoring, see

http://www.luschny.de/math/factorial/index.html 
or our own paper www.cs.berkeley.edu/~fateman/papers/factorial.pdf

This bit of discussion here goes to show that our attempt to use a
traditional factorial program for a benchmark does not represent the
most efficient way of computing the answer, but just a way of using
computer resources in some systematic fashion, watching how some
resource utilization varies as we modify some relatively simple
parameter such as language implementation, language usage, choice of
computer, etc.

... end of digression on Factorial computation.

Further notes:  after writing the QD interface, and revising the
MPFR interface to look like the QD interface, the question arises as to
whether we should revise GMP interface to look the same.  Since the GMP
and MPFR arithmetic are likely to be much slower in their actual
computing, inefficiency in the linkage would be much more tolerable.

Do we need to write with-temps and dsetv for GMP? I think not.

We could do a wrapper for gmp rationals, I guess. Or make interfaces
for the number-theoretic stuff in gmp.

|#



