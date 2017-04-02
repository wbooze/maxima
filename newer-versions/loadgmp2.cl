;;; WHY BROKEN, 8/28/03 RJF??
;;; fixed 5/8/05
;;;USE POLYMULTX.CL  in this directory for working polynomial pgms.
;; Using GMP (Gnu Multi Precision) from Allegro CL.
(eval-when (compile load)
  (declaim (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))
  (declaim (inline svref = eql make-array)))

;; at home
;;(load "c:/gmp41/Debug/gmp41.dll")
					;(load "c:/lisp/gmp-4.1-win32/libgmp-3.dll")

;(load "h:/lisp/gmp-4.1-win32/libgmp-3.dll")

(eval-when (compile load eval)
  (ff:def-foreign-type mpz (* :int))

  (ff:def-foreign-call
   (mpz_init "__gmpz_init")
   ((x (* :int))) 
   :returning :int 
   :arg-checking nil 
   :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_set_str "__gmpz_set_str")	;changes value of mpz x.
   ((x mpz) 
    (s (* :char))
    (base :int)) 
   :strings-convert t  :returning :int
   :arg-checking nil)
  ;;   :call-direct t
     
  (ff:def-foreign-call 
   (mpz_get_str "__gmpz_get_str")
   ((s :int);;let it allocate the space
    (base :int) 
    (op mpz)) 
   :returning  ((* :char) )
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_get_d  "__gmpz_get_d") ((x mpz)) 
   :returning :double
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call;; remove gmp number from memory
   (mpz_clear  "__gmpz_clear") ((x mpz)) 
   :returning :int
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call;; remove gmp number from memory
   (mpz_cmp  "__gmpz_cmp") ((x mpz) (y mpz) ); returns pos if x>y, zero if x=y, neg if x<y
   :returning :int
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_mul  "__gmpz_mul")
   ((target mpz)(op1 mpz)(op2 mpz))
   :returning :void 		         	     
   :arg-checking nil :call-direct t)

  (ff:def-foreign-call 
   (mpz_add  "__gmpz_add") 
   ((target mpz)(op1 mpz)(op2 mpz)) 
   :returning :void
   :arg-checking nil :call-direct t)
  
(ff:def-foreign-call 
   (mpz_add_ui  "__gmpz_add")  ;; add unsigned int.  target <- op1 + op2
   ((target mpz)(op1 mpz)(op2 :long)) 
   :returning :void
   :arg-checking nil :call-direct t)

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
   :arg-checking nil 
   :call-direct t
   )

  (ff:def-foreign-call 
   (mpz_mul_2exp  "__gmpz_mul_2exp");; set target to op1*2^op2
   ((target mpz)(op1 mpz)(op2 :long))	;****
   :returning :void
   :arg-checking nil :call-direct t)
  
  (ff:def-foreign-call 
   (mpz_sign  "__gmpz_sign");; return -1, 0, 1
   ((op1 mpz))				;****
   :returning :fixnum
   :arg-checking nil :call-direct t)
  
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
   (mpz_gcd "__gmpz_gcd")	;greatest common divisor
   ((rop mpz)(op1 mpz) (op2 mpz)) ;;return op is always positive
   :returning  :void
   :arg-checking nil :call-direct t)
  
(ff:def-foreign-call 
   (mpz_gcdext "__gmpz_gcdext")  ;; extended greatest common divisor
      ((g mpz) (u mpz) (v mpz) (a mpz) (b mpz)) 
    ;; inputs are a and b. Outputs are g, u, v.
    ;; a*u+b*v=g.  g is always positive.
   :returning  :void
   :arg-checking nil :call-direct t)

(ff:def-foreign-call 
   (mpz_nextprime "__gmpz_nextprime")	
   ((rop mpz) (op1 mpz))  ;set rop to probably the next highest prime
   :returning  :void
   :arg-checking nil :call-direct t)
  
  ;; add more interface program declarations here
  )

;;;; REFERENCE COUNT VERSION  6/25/02
;; do  :ld gmp41.dll  to get gmp 4.1 into the lisp.
;; latest version is gmp4.1.  6/14/02 rjf

;; There are about 165 gmpz  signed integer functions,
;; About 35 gmpq rational functions,
;; Function entries etc should all be in the gmp.h file

;; write some simple management of gmp numbers as resource.
(defparameter gmpfl nil) ; gmp free list

(defun return-gmp-object(x) ;; puts gmp object on freelist if refcount decrements to 0
  (declare (type (simple-array (integer -2147483648 2147483647) (4)) x))
  
  (let ((c (decf (aref x 3)))) ;; decrement the reference count by 1
    (declare (fixnum c))
    (if (< c 0) (error "negative ref count ~s" x)) ;; debugging info
    (if (= c 0) (push x gmpfl)) ;; save this gmp number on free list to recycle
    0))

(defun vaporize-gmp-object(x) ;; have gmp  reclaim memory if refcount decrements to 0
  (declare (type (simple-array (integer -2147483648 2147483647) (4)) x))
  (let ((c (decf (aref x 3))))
    (declare (fixnum c))
    (if (< c 0) (error "negative ref count ~s" x)) ;; debugging info
    (if (eql c 0) (mpz_clear x)) ;; return to gmp
    0))

#+ignore ;; in case there is a bug, don't do this..
(defun forget-gmp-object(x) ;; return to fl if refcount is now 0
  (declare (type (simple-array (integer -2147483648 2147483647) (4)) x))
  (let ((c (aref x 3)))
    (if (eql c 0) (push x gmpfl)) ;; return to free list
    0))
;; debug.. don't forget anything

(defun forget-gmp-object(x) ;; return to fl if refcount is now 0
    0)


(defun next-gmp-object()  ;; provides a gmp object for us to use
  (if gmpfl 
       (let((newobj (pop gmpfl)))
	 (mpz_set_si newobj 0) newobj) ;; set the value of the gmp number to 0
    ;; otherwise, we need to make a new object out of whole cloth.
    ;; a new object is 3 words for the gmp header; we add one more for reference
    ;; count.  
     (let((newobj (make-array 4 :element-type '(signed-byte 32) :initial-element 0)))
	 (mpz_init newobj) ;; this sets up gmp to know about this object.
	 newobj)))

;; everything on gmpfl has 0 refcount, so don't even look
(defun forget-gmpfl()(map 'nil #'mpz_clear gmpfl)(setf gmpfl nil))

;; create space for a number and put zero in it.
(defun create_mpz_zero()
  (next-gmp-object))

(defun create_mpz_from_string(s)  ;; s is a string like "123"
 (let ((r (next-gmp-object)))
   (mpz_set_str r s 10) ;; base 10 number conversion
  r))

(defun cs(x)(create_mpz_from_string x))	;short name for above

;; given mpz number 123, return "123"
(defun create_string_from_mp(m)  (mpz_get_str 0 10 m)) 

(defun cm(m)(create_string_from_mp m))	; short name for above

;;; the program below is a slow hack, which sort of writes and then re-reads
;;; the number into a lisp number. For really really big gmp numbers you might
;;; be beyond the capacity of the lisp bignum system.

;;; convert gmp number to a lisp number

(defun gmp2lisp(x)(parse-integer (cm x)))

;;; the reverse is also slow, at least for bignums.
(defun lisp2gmp(x)(let ((r (next-gmp-object)))
		    (cond ((fixnump x) (mpz_set_si r x)) ;; works faster for fixnums
			  (t (mpz_set_str r (format nil "~s" x) 10)))
		    r))



;; round trip test, returns yes if successful
(defun rtgmplisp (x) (if (= x (gmp2lisp(lisp2gmp x))) 'yes 'no))


(defun create_double_from_mp(m) ;;  creates a double precision version of mpz
  (mpz_get_d m))

(defun cd(m)(create_double_from_mp m))	; short version of above

;; this function computes and returns op1*op2

(defun mp*(x y)(let ((r (create_mpz_zero)))
		 ;; this leaves r around forever. GMP and lisp might have to
		 ;; work together to remove useless / inaccessible numbers.
		 (mpz_mul r x y) r))

(defun mp+(x y)(let ((r (create_mpz_zero)))
		 ;; this leaves r around forever. GMP and lisp might have to
		 ;; work together to remove useless / inaccessible numbers.
		 (mpz_add r x y) r))

#+allegro(defun profit(r)(prof:start-profiler)(eval r)(prof:show-flat-profile))

;;;;;;;;;;;;;;; the polynomial stuff
;;; a polynomial is either a coef or a vector #(number  poly poly ....poly)
;;; the number is the "variable order"  e.g. x=1, y=2, z=3.

;; test for (probably) a gmp number. Could be some other thing though.
(defmacro coefp(x) `(typep ,x '(simple-array (integer -2147483648 2147483647) (*))))

;; the vap-poly function take a polynomial p and reduces the reference count
;; of each gmp item by one.  If it reduces it to zero, it puts it on the
;; gmp free list.  It does not deallocate this space.


(defun vap-poly(p)			; vaporize polynomial
  (cond ((numberp p) nil) ;; just the variable order...
	((coefp p)(return-gmp-object p) nil)
	(t (map nil #'vap-poly p) nil)))

(defun forget-poly(p)			;
  (cond ((numberp p) nil) ;; just the variable order...
	((coefp p)(forget-gmp-object p) nil)
	(t (map nil #'forget-poly p) nil)))
 
(defun incref(p);; increment the reference count in a polynomial
  (cond ((numberp p) p)
	((coefp p)(incf (aref p 3)) p)
	(t (map nil #'incref p) p)))

;; mainvar:  extract the main variable from a polynomial

(defmacro mainvar (x) `(svref (the simple-vector ,x) 0))

(defmacro csetf (x y) (if (and (symbolp x)(not(boundp x)))
			  `(setf ,x (incref ,y))
			
			   `(let ((uuu ,x)(vvv ,y))
			      (setf ,x (setf vvv (incref vvv)))
			      (cond ((null uuu) nil)
				    (t(vap-poly uuu)))
			      vvv)))


;; in gmp3.1, there is no addmul. Just in gmp4.1
;;(defparameter temp (next-gmp-object))
;;(defmacro mpz_addmul (tar x y)
;; `(progn (mpz_mul temp ,x ,y)
;;	  (mpz_add ,tar ,tar temp)
;;      ,tar))


;; samevar: see if two polynomials have the same main variable

(defmacro samevar (x y) `(eq (mainvar ,x) (mainvar ,y)))

(defmacro var> (x y) `(> ,x ,y))

;;  The next set of macro definitions supplies the arithmetic
;;  for coefficients.  In general, any domain that can support
;;  the coefficient operations below is fair game for the package.
;;  More advanced operations may place additional requirements
;;  on the coefficient domain (E.g. they form an algebraic Ring or Field).

(defmacro coef+ (x y)   `(mp+ ,x ,y))

(defmacro coef* (x y)  `(mp* ,x ,y))

;; Tests for zero and unity are important, as are constructors

(defmacro coefzero () `(create_mpz_zero))               ;;  zero in the coefficient domain

(defmacro coefzerop (x) `(and (coefp ,x)
			      (eql 0 (aref (the(simple-array (signed-byte 32) (4)),x) 1))))

(defun coefzerofunp (x) (coefzerop x))    ;;  to funcall, we need a function

;;; This product function preserves both arguments and returns the product
;;; of two polynomials.

;; p*: return the product of v1*v2

(defun p* (v1 v2)
  (declare (inline make-array))
  (cond ((coefp v1) (p*cv v1 v2));; call function to multiply coef*poly
	((coefp v2) (p*cv v2 v1));; call function to multiply poly*coef
	((samevar v1 v2)
	 ;; two polynomials in the same main variable
	 ;; do  n X m multiplications
	 (let* ((ilim (1- (length v1))) ;;number of terms in v1
		(jlim (1- (length v2))) ;;number of terms in v2
		(index 0);;(index 0)
		(ival 0) ;; temp for v1[i] in loop below
		(res ;; the result array size, including header for var name
		 (make-array (+ ilim jlim) :initial-element nil)))
	   
	   (declare (fixnum ilim jlim index i j))
	   
	   (setf (svref res 0) (mainvar v1)) ;; set the variable name in result
	   (do ((i 1 (1+ i)))
	       ((> i ilim) res)
	     (setq ival (svref v1 i)) ;; step through v1
	     (format t "~%ival = ~s" ival)
	     (cond ((coefp ival)
		    (do ((j 1 (1+ j)))
			((> j jlim) res)
		      (setf index (+ i j -1)) ;; fix 5/8/05		       
		      (let* ((ri (svref res index))
			     (v2j (svref v2 j)))
			(format t "~% v2j=~s ri=~s" (pr v2j) ri)
			(cond ((and (coefp v2j)(coefp ri))
			       (mpz_addmul ri v2j ival))
			      (t
			       (format t "~% aha  ri=~s"  ri)
			       (let* ((r (p* ival v2j))
	;			      (foom (format t "~% foom r=~s"(pr r)))
				      (s (if (null ri) r
					   (prog1 
					       (p+ ri r)
					     (forget-poly ri)
					     (forget-poly r)))))
				 (format t "~%s=~s spr=~s" s (pr s))
				 (setf (svref res index) s) ))))))
		   (t	    ;; ival is not a coefficient
				 
		    (do ((j 1 (1+ j)))
			((> j jlim) res)
		      (let* ((ri (svref res index))
			     (r (p* ival (svref v2 j)))
			     (s (if (null ri) r
				  (prog1 
				      (p+ ri r)
				    (forget-poly ri)
				    (forget-poly r)))))
			(setf (svref res index) s)
			)))))))
		
	((var> (mainvar v1) (mainvar v2)) (p*cv v2 v1))
	(t (p*cv v1 v2))))

;; p*cv: coefficient times polynomial vector;
;;       preserves both inputs and although the result can
;;       share substructure with the vector v, the top-level
;;       vector is new. (true recursively)

(defun p*cv (c v)
  (declare (inline make-array))
  (cond
   ((coefp v) 
    (coef* c v) ;; create a new number
    )
   ((coefzerop c) (coefzero)) ;; 0 * anything is 0	
   ;; run down the length of the vector, multiplying.
   ;; p* is not destructive of its arguments either.
   (t (let* ((v v) (len (length (the simple-vector v))) (u (make-array len)))
      (declare (simple-vector v u) (fixnum len) (inline svref setf mainvar))
      (setf (mainvar u) (mainvar v))
      (do
       ((i (1- len) (1- i)))
       ((= i 0) u)
       (declare (fixnum i))
	(setf (svref u i)  (p* c (svref v i))) ;;leave v around.
	)))))

(defun p+ (v1 v2)
  (cond ((coefp v1) (p+cv v1 v2))
        ((coefp v2) (p+cv v2 v1)) ;; reverse args
        ((samevar v1 v2) ;; same main var
	 (let
	   ((lv1 (length (the simple-vector v1)))
	    (lv2 (length (the simple-vector v2)))
	    (v1 v1) (v2 v2)) ;; not redundant
	   (declare (simple-vector v1 v2) (fixnum lv1 lv2))
	   (cond ((> lv1 lv2)
		  (p+into v2  v1 lv2 lv1)) ;; v1 is longer
		 (t (p+into v1 v2 lv1 lv2)))))
	((var> (mainvar v1) (mainvar v2)) (p+cv v2 v1))
	(t (p+cv v1 v2))))

;; p+cv: add coeff to vector, 

(defun p+cv(c v)
  (if (coefp v)(coef+ c v)
      (let ((v (copy-seq v)))
	(declare (simple-vector v))
	(setf (svref v 1)(prog1
			     (p+ c (svref v 1))
			   (forget-poly (svref v 1))
			   (forget-poly c)))
	v)))


(defun p+into (v1 v2 shorter longer)
  (let (res)
    (declare (fixnum shorter longer)
	     (simple-vector v1 v2 res)
	     (inline dp+vv-zero-check))
      (setq res (make-array longer :initial-contents v2))
    (do ((i 1 (1+ i)))
	((= i shorter) (pnorm res))	
      (declare (fixnum i)) 
      (setf (svref res i) (prog1
			      (p+ (svref v1 i) (svref v2 i))
			    (forget-poly (svref v1 i))
			    (forget-poly (svref v2 i)))))))


;; pnorm converts a polynomial into a normal form in case it is
;; really zero or a constant or has trailing (high degree) zero
;; coeffs.  pnorm is destructive. pnorm is Not recursive except
;; in converting constant terms to manifest non-vectors.
;; Assume x is an arbitrary main-variable index:
;; #(x 5) -> 5.  #(x 5 4 3 0 0) -> #(x 5 4 3).  #(x 0 0) -> 0. 
;; #(x 0 1) -> #(x 0 1) [no change]

;; pnorm: return the normal form of x

(defun pnorm (x)
  (if (coefp x)
      x
      (let ((x x) pos)
	(declare (simple-vector x) (fixnum pos)
		 (inline position-if-not coefzerofunp delete-if))
	(setq pos (position-if-not #'coefzerofunp x :from-end t))
	(cond ((= pos 0)
	       (coefzero)) ;; nothing left but the header: zero polynomial
	      ((= pos 1) ;; just the constant itself
	       (pnorm (svref x 1))) ;; constant polynomial
	      ((= pos (1- (length x))) x)
	      (t (delete-if #'coefzerofunp x :start pos)
		 
		 )))))

;; p^v: this may seem like a dumb way to compute power, but it's not
;; Repeated multiplication is generally faster than squaring.  In this
;; representation, binomial expansion, a good bet for sparse representation,
;; is only sometimes advantageous, and then not by very much, usually.

#+ignore
(defun p^ (x n)             ;; x^n -  n integer, x polynomial
  (cond ((integerp n)
	 (cond ((minusp n) (error "negative powers not allowed"))
	       ((zerop n) 1) ;; x^0 = 1 (even if x = 0)
	       ((eql n 1) x) ;; x^1 = x
	       (t (p* x (p^ x (1- n))))))
	(t (error "only integer powers allowed"))))


(defun p^ (x n)             ;; x^n -  n integer, x polynomial
  (cond ((integerp n)
	 (cond ((minusp n) (error "negative powers not allowed"))
	       ((zerop n) 1) ;; x^0 = 1 (even if x = 0)
	       ((eql n 1) x) ;; x^1 = x
	       (t (let ((r (p^ x (1- n))))
		    (prog1 (p* x r)
		      (forget-poly r))))))
	(t (error "only integer powers allowed"))))

;;;; testing
(defparameter xp nil)
(defparameter yp nil)
(defparameter zp nil)
(defparameter xyz nil)
(progn
  (setf xp nil yp nil zp nil)
  (csetf xp  (vector 1 (coefzero)  (cs "1"))) ;;x
  (csetf yp  (vector 2 (coefzero) (cs "1")));; y
  (csetf zp  (vector 3 (cs"1") (cs "1") )) ;; z+1
  )

(csetf xyz (p+ (p+ xp yp) zp))  ;; x+y+z+1

(defun pr(r)(cond ((coefp r)(if (coefp r) (gmp2lisp r) r)) ;; print the poly
		  (t (cons (plookup (elt r 0))
			   (map 'list #'pr (subseq r 1))))))
(defun plookup(r)
  (elt '(? x y z) r) )
(defun fxyz ()   ;; re-create x+y+z+1
  (let
      ((xp  (vector 1 (coefzero)  (cs "1")));;x
       (yp  (vector 2 (coefzero) (cs "1")));; y
       (zp  (vector 3 (cs"1") (cs "1") ));; z+1
       )
  
    (p+ (p+ xp yp) zp)))




;; answer for xyz ^ 4   pr..
#|

(z (y (x 1 4 6 4 1) (x 4 12 12 4) (x 6 12 6) (x 4 4) 1)
 (y (x 4 12 12 4) (x 12 24 12) (x 12 12) 4) (y (x 6 12 6) (x 12 12) 6)
 (y (x 4 4) 4) 1)

14,736k
2.8+.422 sec = 3.3
33,508k 
34,500k
52,436k ...  20
|#







#| set up testing speed of multiply
(defun t0 (m)
  (setq arand (random (expt 2 1024))
	ag (lisp2gmp arand)
	target (coef* ag ag)))

(defun t1(m)
  (declare (fixnum m)(special target ag))
    (dotimes (i m)(mpz_mul target ag ag))))
|#

;; var should be #(1) for example

(defun headit(var rest)(concatenate 'simple-array var rest))

;; this is not right
(defun p*1vark (pp qq);; karatsuba mult, one var
  (let ((plim (1- (length pp)));;number of terms in pp
	 (qlim (1- (length qq)));;number of terms in qq
	)    
    (declare (special *karatlim*))
    (if (< (min plim qlim) *karatlim*)
	(p*1var pp qq)		; don't use for small polys
      (let* ((var (subseq pp 0 1))	; presumably same as main var in qq
	     aa
	     (h (truncate (max plim qlim) 2))
	     (A (headit var (subseq pp (1+ (min h plim)) plim)))
	     (B (subseq pp 0 (1+(min h plim))))
	     ;;; I think we need to copy these arrays /contents...
	     (C (headit var (subseq qq (1+(min h qlim)) qlim)))
	     (D (subseq qq 0 (1+ (min h qlim))))
	  ;   (foo (print (list 'foo A B C D)))
	     (u (p+ A B))
	     (v (p+ C D))
	   ;  (foo2 (print (list 'foo2 A B C D u v)))
	     (U (p*1vark u v ))
	     (TT (p*1vark A C))
	     (S (p*1vark B D)))
;	(break "aha")
	
	     
	(psub-into S U 0)		; change U to  U-S
	(psub-into TT U 0)		; change U to  U-S-T
	(setf aa (pshiftleft TT (ash h 1))) ;aa = T*x^(2h)
	(padd-into U aa h)		; change aa to T*x^(2h)+U*x^h
	(padd-into S aa 0)		; change aa to T*x^(2h)+U*x^h +S
	(pnorm aa)
	aa
	))))

(defun pshiftleft(p k)		; like multiplying by x^k
  ;; k is positive integer, p is a polynomial with header
  (let* ((oldlen (length p))
	 (newlen (+ oldlen k))
	 (newpoly (make-array newlen)))
    (dotimes (k newlen)(setf (aref newpoly k)(create_mpz_zero)))
    (do ((i  (1- oldlen) (1- i)))
	((<= i 0) )
      (setf (aref newpoly (+ k i))(aref p i)))
    (setf (aref newpoly 0)(aref p 0))
    newpoly))

(defun padd-into (p aa start)  ;; hacked up a bit..
  ;; p is shorter than answer aa 
  ;; p is unchanged.  aa is changed to aa+ p*x^start
  (declare (fixnum start))
  (do ((i (+ (length p) start -1) (1- i)))
      ((< i start) aa)
    (declare (fixnum i))
    (let ((m (aref p (- i start))))
      (mpz_add  (aref aa i) (aref aa i) m)))
  aa)

(defun psub-into (p aa start)  ;; hacked up a bit..
  ;; p is shorter than answer aa 
  ;; p is unchanged.  aa is changed to aa- p*x^start
  (declare (fixnum start))
  (do ((i (+ (length p) start -1) (1- i)))
      ((< i start) aa)
    (declare (fixnum i))
    (let ((m (aref p (- i start))))
      (mpz_sub (aref aa i) (aref  aa i) m))
    aa))

;; this is not quite right
(defun p*1var (v1 v2);; classical mult, one var
  (declare (inline make-array))
  (let* ((ilim (1- (length v1)));;number of terms in v1
	 (jlim (1- (length v2)));;number of terms in v2
	 (index 0);;(index 0)
	 (ival 0);; temp for v1[i] in loop below
	 (res;; the result array size, including header for var name
	  (make-array (+ ilim jlim) :initial-element nil)))
    (declare (fixnum ilim jlim index i j))
    (setf (svref res 0) (mainvar v1));; set the variable name in result
    (do ((i 1 (1+ i)))
	((> i ilim) res)
      (setq ival (svref v1 i));; step through v1
      (do ((j 1 (1+ j)))
	  ((> j jlim) res)
	(setf index (+ i j -1));; fix 5/8/05		       
	(let* ((ri (svref res index))
	       (v2j (svref v2 j)))
	  (if ri			;not nil
	      (mpz_addmul ri v2j ival) ;; destructive addmul
	    (setf (svref res index)  (mp* ival v2j))))))))

