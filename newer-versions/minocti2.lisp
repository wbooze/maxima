;;; -*- Mode:Common-Lisp; Package:octi; Base:10 -*-
;;; A quadruple-precison floating-point arithmetic package.
;;; minimum components for oct implementation.
;;; see octi.lisp for documentation, license, etc.

;;; Other parts in the works include user-overloaded 
;;; data types, elementary functions, complex, etc.

;;; 10/27/07 RJF


(defpackage :octi
  (:use  :cl)
  (:export  lisp2octi oct2lisp
	   add-oct-t add-oct-d-t add-oct
	   sub-oct-t sub-oct-d-t sub-oct
	   mul-oct-t mul-oct-d-t mul-oct
	   div-oct-t div-oct-d-t div-oct
	   sqr-oct-t sqr-oct-d-t sqr-oct
	   neg-oct-t neg-oct-d-t neg-oct	   
	   abs-oct-t abs-oct-d-t abs-oct	   
	   pow-oct-i pow-oct-i-t
	   1--oct-t
	   1+-oct-t))

(eval-when (:execute :load-toplevel :compile-toplevel :execute)
  (declaim (special  +oct-zero+ +oct-one+ ))
  
(deftype %quad-double ()   '(simple-array double-float (4)))

(defmacro oct-0(x) `(aref ,x 0))
(defmacro oct-1(x) `(aref ,x 1))
(defmacro oct-2(x) `(aref ,x 2))
(defmacro oct-3(x) `(aref ,x 3))


(defmacro float-infinity-p(x)`(= (abs ,x) #.excl::*infinity-double*))
  
;;qts = quick-two-sum 

(defmacro %make-oct(a b c d)
  (let ((r (gensym)))
  `(let ((,r 
	  #-allegro
	  (make-array 4 :element-type 'double-float )
	  #+allegro 
	  (make-array 4 :element-type 'double-float 
		      :allocation :lispstatic-reclaimable)))
     (setf (oct-0 ,r) ,a)
     (setf (oct-1 ,r) ,b)
     (setf (oct-2 ,r) ,c)
     (setf (oct-3 ,r) ,d)
     ,r)))

(defconstant +oct-one+ (%make-oct 1d0 0d0 0d0 0d0))
(defconstant +oct-zero+ (%make-oct 0d0 0d0 0d0 0d0))


(defmacro oct-partsmac (s0 s1 s2 s3 rr) ;; pick apart a OCT
  `(let() 
     (setf ,s0 (aref ,rr 0))
     (setf ,s1 (aref ,rr 1))
     (setf ,s2 (aref ,rr 2))
     (setf ,s3 (aref ,rr 3))))

(defun scale-float-oct (oct k)
  (let ((target (%make-oct 0d0 0d0 0d0 0d0)))
    (scale-float-oct-t oct k target)))

(defun scale-float-oct-t (oct k target)
  
  (declare (type %quad-double oct)
	   (type fixnum k)
	   (optimize (speed 3) (space 0)))
  (let((a0 0d0)(a1 0d0)(a2 0d0)(a3 0d0))
    (declare (double-float a0 a1 a2 a3))
      (oct-partsmac a0 a1 a2 a3  oct)
      (setf (oct-0 target)(scale-float a0 k))
      (setf (oct-1 target)(scale-float a1 k))
      (setf (oct-2 target)(scale-float a2 k))
      (setf (oct-3 target)(scale-float a3 k))
      target))

(defmacro qtsmac (s e x y);; s, e should be symbols.
  (let ((a (gensym))
	(b (gensym)))
    `(let* ((,a ,x)
	    (,b ,y))
       (declare (double-float ,s ,e ,a ,b) (optimize (speed 3)(safety 1)))
       (setf ,s (+ ,a ,b))
       (setf ,e (- ,b (- ,s ,a))))))

(defmacro renorm-4mac (s0 s1 s2 s3;; output
		       %c0 %c1 %c2 %c3;; input
		       )
  (let ((c0 (gensym))
	(c1 (gensym))
	(c2 (gensym))
	(c3 (gensym)))
    `(let ((,c0 ,%c0)
	   (,c1 ,%c1)
	   (,c2 ,%c2)
	   (,c3 ,%c3)
	   )
       (declare (double-float ,s0 ,s1 ,s2 ,s3 ,c0 ,c1 ,c2 ,c3)
		(optimize (speed 3) (safety 1)))
       (setf ,s2 0.0d0 ,s3 0.0d0)
       (qtsmac ,s0 ,c3 ,c2 ,c3)
       (qtsmac ,s0 ,c2 ,c1 ,s0)
       (qtsmac ,c0 ,c1 ,c0 ,s0)
       (setf ,s0 ,c0 ,s1 ,c1)
       (cond ((/= ,s1 0.0d0)
	      (qtsmac ,s1 ,s2 ,s1 ,c2)
	      (if (/= ,s2 0.0d0)
		  (qtsmac ,s2 ,s3 ,s2 ,c3)
		(qtsmac ,s1 ,s2 ,s1 ,c3)))
	     (t   
	      (qtsmac ,s0 ,s1 ,s0 ,c2)
	      (if (/= ,s1 0)
		  (qtsmac ,s1 ,s2 ,s1 ,c3)
		(qtsmac ,s0 ,s1 ,s0 ,c3)
		;;(values s0 s1 s2 s3)
		))))))

(defmacro renorm-5mac (s0 s1 s2 s3
		       %c0 %c1 %c2 %c3 %c4)
       
  (let ((c0 (gensym))
	(c1 (gensym))
	(c2 (gensym))
	(c3 (gensym))
	(c4 (gensym)))
    `(let ((,c0 ,%c0)
	   (,c1 ,%c1)
	   (,c2 ,%c2)
	   (,c3 ,%c3)
	   (,c4 ,%c4))
 (declare (double-float ,s0 ,s1 ,s2 ,s3 ,c0 ,c1 ,c2 ,c3)
	  (optimize (speed 3) (safety 1)))
 (setf ,s2 0d0 ,s3 0d0)
 (qtsmac ,s0 ,c4 ,c3 ,c4)
 (qtsmac ,s0 ,c3 ,c2 ,s0)
 (qtsmac ,s0 ,c2 ,c1 ,s0)
 (qtsmac ,c0 ,c1 ,c0 ,s0)
 (setf ,s0 ,c0 ,s1 ,c1)
 (qtsmac ,s0 ,s1 ,c0 ,c1)
 (cond ((/= ,s1 0)

	(qtsmac ,s1 ,s2 ,s1 ,c2)
	(cond ((/= ,s2 0)
	       (qtsmac ,s2 ,s3 ,s2 ,c3)
	       (if (/= ,s3 0) (incf ,s3 ,c4)	 (incf ,s2 ,c4)))
	      (t (qtsmac ,s1 ,s2 ,s1 ,c3)
		 (if (/= ,s2 0) (qtsmac ,s2 ,s3 ,s2 ,c4)
		   (qtsmac ,s1 ,s2 ,s1 ,c4)))))
       (t
	(qtsmac ,s0 ,s1 ,s0 ,c2)
	(cond ((/= ,s1 0)
	       (qtsmac ,s1 ,s2 ,s1 ,c3)
	       (if (/= ,s2 0)(qtsmac ,s2 ,s3 ,s2 ,c4)
		 (qtsmac ,s1 ,s2 ,s1 ,c4)))
	      (t
	       (qtsmac ,s0 ,s1 ,s0 ,c3)
	       (if (/= ,s1 0) (qtsmac ,s1 ,s2 ,s1 ,c4)
		 (qtsmac ,s0 ,s1 ,s0 ,c4)))))))))

(defmacro copy-octi-into-octi (from to)  
  ;; not sanitized
  (let ((ans (list 'progn)))
  (dotimes (i 4)
    (push `(setf (aref ,to ,i)(aref ,from ,i)) ans))
  (push to ans)
  (reverse ans)))

(defun %make-oct-d (a0 &optional (a1 0d0 a1-p) (a2 0d0) (a3 0d0))
  "Create a %quad-double from four double-floats, appropriately
  normalizing the result from the four double-floats.
"
  (let ((s0 0d0)(s1 0d0) (s2 0d0) (s3 0d0))
  (declare (double-float a0 a1 a2 a3 s0 s1 s2 s3)
	   (optimize (speed 3)(safety 1)
		     #+cmu
		     (ext:inhibit-warnings 3)))
  (cond(a1-p
	(renorm-4mac s0 s1 s2 s3 a0 a1 a2 a3)
	(%make-oct s0 s1 s2 s3))
       (t(%make-oct a0 0d0 0d0 0d0)))))


(defmacro splitmac (a-hi a-lo %a) ;; set a-hi and a-lo
  "Split the double-float number a into-octi a-hi and a-lo such that a =
  a-hi + a-lo and a-hi contains the upper 26 significant bits of a and
  a-lo contains the lower 26 bits."
  
  (let ((a (gensym))
	(tmp (gensym)))
    `(let ((,a ,%a))
       (declare (double-float ,a)
		(optimize (speed 3)(safety 1)))
       ;; do unusual case out of line.
       (cond ((> (abs ,a) #.(scale-float 1d0 #.(- 1023 27))) 
	      (let((,tmp  (splitmacr1 ,a)))
		(setf ,a-hi (car ,tmp))
		(setf ,a-lo (cdr ,tmp))))
	     (t
	      ;; Yozo's algorithm.
	      (let* ((,tmp (* ,a #.(+ 1 (expt 2 27)))))
		(declare (double-float ,tmp))
		(setf ,a-hi (- ,tmp (- ,tmp ,a)))
		(setf ,a-lo (- ,a ,a-hi))))))))
(defun splitmacr1 (a)
  (let* ((tmp (* a #.(+ 1.0d0 (scale-float 1d0 -27))))
	 (as (* a #.(scale-float 1d0 -27)))
	 (a-hi (* (- tmp (- tmp as)) #.(expt 2 27)))
	 (a-lo (- a a-hi)))
    (declare (double-float tmp as a-hi a-lo)(optimize (speed 3)(safety 1)))
    (cons a-hi a-lo)))


(defmacro two-prodmac (p-hi p-lo %a %b)
  "Compute fl(a*b) and err(a*b)"  ;; something like this..
  (let ((a (gensym))
	(b (gensym))
	(a-hi (gensym))
	(a-lo (gensym))
	(b-hi (gensym))
	(b-lo (gensym)))
  `(let ((,a ,%a)
	 (,b ,%b)
	 (,a-hi 0.0d0)(,a-lo 0.0d0)(,b-hi 0.0d0)(,b-lo 0.0d0))
       (declare (double-float ,a ,b ,p-hi ,p-lo ,a-hi ,a-lo ,b-hi ,b-lo)
		(optimize (speed 3)(safety 1)))
       (setf ,p-hi (* ,a ,b))
       (splitmac ,a-hi ,a-lo ,a)
       (splitmac ,b-hi ,b-lo ,b)
       (setf ,p-lo (+ (+ (- (* ,a-hi ,b-hi) ,p-hi)
		   (* ,a-hi ,b-lo)
		   (* ,a-lo ,b-hi))
		      (* ,a-lo ,b-lo))))))

(defmacro two-summac (s e %a %b)
  "Computes fl(a+b) and err(a+b)"
      (let ((a (gensym))
	    (b (gensym))
	    (v (gensym)))
    `(let ((,a ,%a)
	   (,b ,%b)
	   (,v 0.0d0))

  (declare (double-float ,a ,b ,v ,s ,e) (optimize (speed 3)(safety 1)(debug 0))) ;rjf 9/16/07
    (setf ,s (+ ,a ,b))
    (setf ,v (- ,s ,a))
    (setf ,e (+ (- ,a (- ,s ,v))
		(- ,b ,v))))))




(defmacro three-summac (newa newb newc %a %b %c) ;; newa,b,c are symbols
      (let ((a (gensym))
	    (b (gensym))
	    (c (gensym))
	    (t1 (gensym))
	    (t2 (gensym))
	    (t3 (gensym)))
    `(let ((,a ,%a)
	   (,b ,%b)
	   (,c ,%c)
	   (,t1 0.0d0)(,t2 0.0d0)(,t3 0.0d0))
	 (declare (double-float ,t1 ,t2 ,t3 ,a ,b ,c ,newa ,newb ,newc)
		  (optimize (speed 3)(safety 1)))
	 (two-summac ,t1 ,t2 ,a ,b)
	 (two-summac ,newa ,t3 ,c ,t1)
	 (two-summac ,newb ,newc ,t2 ,t3))))


(defmacro three-sum2mac (newa newb %a %b %c) ;; newa,b,c are symbols
      (let ((a (gensym))
	    (b (gensym))
	    (c (gensym))
	    (t1 (gensym))
	    (t2 (gensym))
	    (t3 (gensym)))
    `(let ((,a ,%a)
	   (,b ,%b)
	   (,c ,%c)
	   (,t1 0.0d0)(,t2 0.0d0)(,t3 0.0d0))
	 (declare (double-float ,t1 ,t2 ,t3 ,a ,b ,c ,newa ,newb)
		  (optimize (speed 3)(safety 1)))
	 (two-summac ,t1 ,t2 ,a ,b)
	 (two-summac ,newa ,t3 ,c ,t1)
	 ;; next line is simpler
	 (setf ,newb (cl:+ ,t2 ,t3) ))))






;;oct-to-decimal decoder. 
;;; this is OCT's oct, which is an array


(defun oct-decode(q &optional (digits 65) (base 10)) 
  ;; returns 3 integers: sign; fraction to right of dec.point; base-10 expon.
  (let ((r (apply #'cl::+ (map 'list  #'cl::rational (coerce q
							     'list)))))
  (decimalize  (abs r)
	       digits ;; could be more than 65 if you have 1.0+1.0q-80 !
	       (signum r)
	       base)))

;; convert non-neg. rational r to a decimal [or other base] number with n digits
;; showing in the fraction. If r=0, then just 0.
;; return sign=1,-1, or 0;  fraction is an integer, exponent is an integer.

(defun decimalize(r n sign &optional (base 10)); r rational number >=0, sign is +-1 or 0
  (let* ((expon  (largest-power-less-than r base))
	 (frac   (truncate;; was, round. causes tricky edge cases.
		  (cl:* (cl::abs r) (cl::expt base (cl::- n expon 1))))))
    (values     sign  frac
	      (cl:1+ expon))))

;; return z such that base^z<=r,  
;; not completely general. Works for
;; rational r, where r can be converted without overflow to double-float
;; Should be OK for oct numbers.

(defun largest-power-less-than(r base) ;; or equal
  (if (cl::= r 0) -1
  (let ((guess (cl::floor(cl::log (cl::* 1.0d0 (cl::abs r)) base))))
    (if (> (expt base guess) r) (1- guess) guess))))


;;(loop for i from 10 while (< i 20) do (print (list i (multiple-value-list (oct-decode (- 1 (expt #q10 (- i)))))) ))

;; make a formatted output . Something like this does the job

(defparameter *oct-digits-to-show* 66) ; MAX number of digits to show. Trailing zeros are omitted.

;; octi's oct ... just an array
(defmethod oct2string((x array))
  ;; check if x is a NaN
  (if (excl::nan-p (aref  x 0)) "NaN(oct)"
    (multiple-value-bind (s r e h)  ;h is extra variable
	(decimalize (abs(oct2lisp x)) *oct-digits-to-show* (signum (oct-0 x)) 10)
      (declare (ignore s))
      (format nil "~a0.~aQ~s" (if (cl:< (oct-0 x) 0)"-" "") 
	      (string-right-trim 
	       "0"  
	       (subseq (setf h(format nil "~a" r)) 0  
		       (cl::min (length h) *oct-digits-to-show*)) )
	      e))))

(defmethod oct2lisp((x array))
  (if (excl::nan-p (aref  x 0)) (aref x 0)
					; if oct contains a NaN, use it.
    (apply #'cl::+  (map 'list #'cl::rational  x ))))

(defmethod oct2lisp(y) y)

(defun rational-oct(x)(oct2lisp x));; 

;; this is octi::into

(defun into(r &optional (where (%make-oct 0d0 0d0 0d0 0d0)))
  (lisp2octi r where))

;; The following methods REQUIRE a place to put the answer, and so
;; are less convenient than "into" to use directly.

(defmethod lisp2octi((x ratio) (ans array)) ;to encode a lisp rational, divide num/den
 (div-oct-t (octi::into (numerator x))(octi::into (denominator x)) ans))

(defmethod lisp2octi((x fixnum) (ans array))  ;small integers are easy.
  (lisp2octi (coerce x 'double-float) ans))

(defmethod lisp2octi ((x float) (ans array)) ;single-float or double-floats fit here
  ;;(format t "~% float array to lisp2octi ~s, ~s" x ans)
  (loop for i from 1 to 3 do 
	(setf (aref ans i) 0.0d0))
  (setf (aref ans 0) (coerce x 'double-float))
  ans)


;; this works for bignums, also rationals
(defmethod lisp2octi ((x integer) (ans array))
  ;; bignum integers that are shorter than a double-float fraction are easy.
  (if (< (cl::abs x) #.(cl::expt 2 53)) (lisp2octi  (coerce x 'double-float) ans)

    (let* ((p (coerce x 'double-float))
	   (ans (lisp2octi p ans)))
      (dotimes (i 3 ans)
	(setf p (coerce (decf x (cl::rational p)) 'double-float))
	(add-oct-d-t ans p ans)))))

(defmethod lisp2octi((x array)(ans array)) ;; assume it is an octi
  (copy-octi-into-octi x ans)
  ans)

;;; ARITHMETIC, no targets


;; generate "functional" arithmetic programs like (add-oct x y) from the
;; programs (add-oct-t x y target),  which are implemented separately.

;;(define-compiler-macro add-oct(a b &optional (c (%make-oct 0d0 0d0 0d0 0d0)))  `(add-oct-t ,a ,b ,c))

(defmacro ss(name count)		;  make the define-compiler-macro stuff for name..
  (let* ((short-name (intern name :octi))
	 (targname(intern (concatenate 'string name "-t") :octi))
	 (arglist (case count 
		   (1 '(a &optional (c (%make-oct 0d0 0d0 0d0 0d0))))
		   (2 '(a &optional b (c (%make-oct 0d0 0d0 0d0 0d0))))
		   (otherwise (error "illegal count for ss macro" count))))
	 (thecall (case count
		    (1 (list targname 'a 'c))
		    (2 (list targname 'a 'b 'c)))))

    `(progn  ;; next line clumsy but works. or does it
   #+ignore    ,(subst short-name 'jj
	       (subst targname 
		      'kk 
		      `(define-compiler-macro jj ,arglist `(kk ,a ,b ,c))))
       (defun ,short-name ,arglist ,thecall))))

(ss "add-oct" 2)(ss "add-oct-d" 2)
(ss "mul-oct" 2)(ss "mul-oct-d" 2)
(ss "sub-oct" 2)(ss "sub-oct-d" 2)
(ss "div-oct" 2)(ss "div-oct-d" 2)

(ss "invert-oct" 1)(ss "invert-oct-d" 1)
(ss "sqr-oct" 1) (ss "abs-oct" 1) (ss "neg-oct" 1)
(ss "1+-oct" 1) (ss "1--oct" 1)

;;(ss "hypot-oct" 2)(ss "expt-oct" 2)
;;(ss "sin-oct" 1) (ss "cos-oct" 1)(ss "tan-oct" 1) 
;;(ss "sinh-oct" 1) (ss "cosh-oct" 1)(ss "tanh-oct" 1)
;;(ss "asin-oct" 1) (ss "acos-oct" 1) (ss "atan-oct" 1 )
;;(ss "log-oct" 1) 
;;(ss "exp-oct" 1) (ss "sqrt-oct" 1)

;; see below for ffloor, fceiling, ftruncate, fround, which we don't 
;; provide -t  versions for.

;;; here are the arithmetic routines (with targets)

(defun add-oct-t (a b target) ;; compare to add-oct
  (declare (type %quad-double a b target)
	   (optimize (speed 3) (space 0) (safety 1)))
  

  ;; ray's notes:
  ;; This is the version that is NOT IEEE.  Should we use the IEEE
  ;; version?  It's quite a bit more complicated.
  ;;
  ;; In addition, this is reorganized to minimize data dependency.
  (let ((a0 0d0)(a1 0d0)(a2 0d0)(a3 0d0)
	(b0 0d0)(b1 0d0)(b2 0d0)(b3 0d0))
    (declare(double-float a0 a1 a2 a3 b0 b1 b2 b3))
    
      (oct-partsmac a0 a1 a2 a3 a)
      (oct-partsmac b0 b1 b2 b3 b)
      (let* ((s0 (cl:+ a0 b0))	     (s1 (cl:+ a1 b1))	     (s2 (cl:+ a2 b2))	     (s3 (cl:+ a3 b3))
	     (v0 (cl:- s0 a0))	     (v1 (cl:- s1 a1))	     (v2 (cl:- s2 a2))	     (v3 (cl:- s3 a3))
	     (u0 (cl:- s0 v0))	     (u1 (cl:- s1 v1))	     (u2 (cl:- s2 v2))	     (u3 (cl:- s3 v3))
	     (w0 (cl:- a0 u0))	     (w1 (cl:- a1 u1))	     (w2 (cl:- a2 u2))	     (w3 (cl:- a3 u3)))
	(let* (    (u0 (cl:- b0 v0))  	      (u1 (cl:- b1 v1))	      (u2 (cl:- b2 v2))	      (u3 (cl:- b3 v3))
		   (t0 (cl:+ w0 u0))	      (t1 (cl:+ w1 u1))	      (t2 (cl:+ w2 u2))	      (t3 (cl:+ w3 u3)))

	  (declare (double-float t0 t1 t2 t3 u0 u1 u2 u3))
	  (two-summac s1 t0 s1 t0)
	  (three-summac  s2 t0 t1 s2 t0 t1)
	  (three-sum2mac s3 t0 s3 t0 t2)
	  (setf t0 (cl:+ t0 t1 t3))
	  ;; Renormalize
	 ;; (format t "~% renorm ~s ~s ~s ~s ~s" s0 s1 s2 s3 t0)
	  (renorm-5mac s0 s1 s2 s3   s0 s1 s2 s3 t0))
	(cond ((and (zerop a0) (zerop b0))
	       (setf (aref target 0) 0d0) (setf (aref target 1) 0d0) (setf (aref target 2) 0d0) (setf (aref target 3) 0d0))
	      (t
	       (setf (aref target 0) s0)  (setf (aref target 1) s1)  (setf (aref target 2) s2)  (setf (aref target 3) s3)))
	target)))


(defun mul-oct-t (a b target)  ;;; compare to mul-oct!
  (declare (type %quad-double a b target)
	   (optimize (speed 3)
		     (safety 1)))
  (let ((a0 0d0)(a1 0d0)(a2 0d0)(a3 0d0)
	(b0 0d0)(b1 0d0)(b2 0d0)(b3 0d0)
	(p0 0d0)(p1 0d0)(p2 0d0)(p3 0d0)(p4 0d0)(p5 0d0)
	(q0 0d0)(q1 0d0)(q2 0d0)(q3 0d0)(q4 0d0)(q5 0d0)
	)
    (declare(double-float a0 a1 a2 a3 b0 b1 b2 b3 p0 p1 p2 p3 p4 p5
			  q0 q1 q2 q3 q4 q5))
    (oct-partsmac a0 a1 a2 a3 a)
    (oct-partsmac b0 b1 b2 b3 b)
    (two-prodmac p0 q0 a0 b0)
  
    (cond((float-infinity-p p0)
	  (setf (aref target 0) p0)
	  (setf (aref target 1) 0d0)
	  (setf (aref target 2) 0d0)
	  (setf (aref target 3) 0d0)
	  (return-from mul-oct-t target)))
    (two-prodmac p1 q1 a0 b1)
    (two-prodmac p2 q2 a1 b0)
    (two-prodmac p3 q3 a0 b2)
    (two-prodmac p4 q4 a1 b1)
    (two-prodmac p5 q5 a2 b0)
    ;; Start accumulation
    (three-summac p1 p2 q0 p1 p2 q0)

    ;; six-three-sum of p2, q1, q2, p3, p4, p5
    (three-summac p2 q1 q2 p2 q1 q2)
    (three-summac p3 p4 p5 p3 p4 p5)
    ;; Compute (s0,s1,s2) = (p2,q1,q2) + (p3,p4,p5)
    (let ((s0 0d0)(s1 0d0)
	  (t0 0d0)(t1 0d0)
	  (r0 0d0)(r1 0d0)
	  (s2(cl:+ q2 p5)))
      (declare (double-float s0 s1 s2 t0 t1 r0 r1))
      (two-summac s0 t0 p2 p3)
      (two-summac s1 t1 q1 p4)
      (two-summac s1 t0 s1 t0)
      (incf s2 (cl:+ t0 t1))
	;; O(eps^3) order terms.  This is the sloppy
	;; multiplication version.  Should we use
	;; the precise version?  It's significantly
	;; more complex.
			  
      (incf s1 (cl:+ (cl:* a0 b3)
		     (cl:* a1 b2)
		     (cl:* a2 b1)
		     (cl:* a3 b0)
		     q0 q3 q4 q5))
     ;; (format t "p0,p1,s0,s1,s2 = ~a ~a ~a ~a ~a~%"	      p0 p1 s0 s1 s2)
      (renorm-5mac r0 r1 s0 s1   p0 p1 s0 s1 s2)
      (if (zerop r0)
	  (progn
	    (setf (aref target 0) p0)
	    (setf (aref target 1) 0d0)
	    (setf (aref target 2) 0d0)
	    (setf (aref target 3) 0d0))
	(progn
	  (setf (aref target 0) r0)
	  (setf (aref target 1) r1)
	  (setf (aref target 2) s0)
	  (setf (aref target 3) s1)))))
  target)
      

;; this is an example of a hand-coded program using destructive
;; operations.

(defun sqrt-oct-t(a ans)
  (let* ((as (oct-0 a))			; approx to z, the first 53 bits.
	 (sqr0 (sqrt as))		; appx squareroot of z
	 (sqr1 (%make-oct-d sqr0))	; convert approx to oct
	 )
    (cond ((cl::zerop sqr0)(copy-octi-into-octi +oct-zero+ ans))
	  (t				;3 Newton iterations.
	 ;;  (copy-octi-into-octi +oct-zero+ ans)
	   (mul-oct-d-t (add-oct-t sqr1 (div-oct-t a sqr1 ans) ans) 0.5d0 sqr1)
	   (mul-oct-d-t (add-oct-t sqr1 (div-oct-t a sqr1 ans) ans) 0.5d0 sqr1)
	   (mul-oct-d-t (add-oct-t sqr1 (div-oct-t a sqr1 ans) ans) 0.5d0 ans)
	   ans))))

;; inversion is 1/x

(defun invert-oct-t(v h2) ;;h2 is the target.
  ;; a quartic newton iteration for 1/v
  ;; to invert v, start with a good guess, x.
  ;; let h= 1-v*x  ;; h is small
  ;; return x+ x*(h+h^2+h^3) . compute h^3 in double-float
  ;; enough accuracy.

  (let* 
      ((x (%make-oct (cl:/ (aref v 0)) 0d0 0d0 0d0))
       (h (%make-oct 0d0 0d0 0d0 0d0)) ;;allocate space for temp
       (h3 h)
       )
    (declare  (type %quad-double v h h2)(double-float h3))    
    (add-oct-d-t  (neg-oct-t(mul-oct-t v x h) h) 1.0d0 h) ;set h
    (mul-oct-t h h h2)		;also use h2 for target
    (setf h3 (* (aref h 0)(aref h2 0)))
    (add-oct-t x 
	      (mul-oct-t x
			(add-oct-t h (add-oct-d-t h2 
					      h3 
					      h2) h2) h2) h2) ))

(defun invert-oct-d-t(v h2) ;; v is a double-float h2 is the target.
  ;; a quartic newton iteration for 1/v
  ;; to invert v, start with a good guess, x.
  ;; let h= 1-v*x  ;; h is small
  ;; return x+ x*(h+h^2+h^3) . compute h^3 in double-float
  ;; enough accuracy.

  (let* 
      ((x (%make-oct (cl:/ v) 0d0 0d0 0d0))
       (h(%make-oct 0d0 0d0 0d0 0d0)) ;;allocate space for temp
       (h3 h))
    (declare  (type %quad-double h h2)(double-float v h3))    
    (add-oct-d-t  (neg-oct-t
		  ;;(mul-oct-d-t x v h)
		  (mul-oct-d x v)
		  h)1.0d0 h)		;set h
    (setf h2 (mul-oct h h))		;also use h2 for target
    (setf h3 (* (aref h 0)(aref h2 0)))
    (add-oct-t x 
	      (mul-oct-t x
			(add-oct-t h (add-oct-d-t h2 
					      h3 
					      h2) h2) h2) h2)))
				 
;; maybe check for division by zero or infinity?

(defun div-oct-t(u v ans);;
  (mul-oct-t u (invert-oct-t v #.(%make-oct 0d0 0d0 0d0 0d0)) ans))

(defun div-oct-d-t(u v ans);;
    (mul-oct-t u (invert-oct-d-t v ans) ans))

(defun sqr-oct-t(a ans)
    ;; cheap hack for a wasteful program
  (mul-oct-t a a ans)
  ans)

;; compute power of oct to an integer, store in ans
(defun pow-oct-i-t(a n ans)			; n is positive integer
  ;; this is generalized in expt-oct.
  (cond((cl:zerop n) (setf (aref ans 0) 1d0) ans)
       ((cl:evenp n)(sqr-oct-t (pow-oct-i-t a (cl:ash n -1) ans) ans))
       (t (mul-oct-t a (pow-oct-i a (1- n)) ans))))

(defun pow-oct-i(a n) (pow-oct-i-t a n (%make-oct-d 0d0)))

(defun expt-oct-t(a b ans) ;; a^b, where a, b are both octs.
  ;;a^b = exp(b*log(a))
  ;; first see if b is really an integer.
  (let* ((r (oct-0 b))
	 (s (round r)))
       
    (if (and (= r s)(= 0d0 (oct-1 b) (oct-2 b) (oct-3 b)))
	(pow-oct-i-t a s ans)
      (exp-oct-t (mul-oct b (log-oct a)) ans))))

(defun mul-oct-d-t (a b ans)
  "Multiply quad-double A with B, result put in ans"
  (declare (type %quad-double a ans)
	   (double-float b)
	   (optimize (speed 3)(space 0)(safety 1)))
  (let ((p0 0d0)(q0 0d0)(p1 0d0)(q1 0d0) (p2 0d0)(q2 0d0))
    (declare (double-float p0 q0 p1 q1 p2 q2))
    (two-prodmac p0 q0 (oct-0 a) b)
    (when (float-infinity-p p0)
      (setf (oct-0 0)p0)
      (return-from mul-oct-d-t ans))
    (two-prodmac p1 q1 (oct-1 a) b)
    (two-prodmac p2 q2 (oct-2 a) b)
    (let* ((p3 (cl:* (oct-3 a) b))
	   (s0 p0)
	   (s1 p0)
	   (s2 p0))
      (declare (double-float s0 s1 s2 p3))
      (two-summac s1 s2 q0 p1)
      (three-summac s2 q1 p2 s2 q1 p2)
      (three-sum2mac q1 q2 q1 q2 p3)
      (let ((s3 q1)
	    (s4 (cl:+ q2 p2)))
	(renorm-5mac s0 s1 s2 s3  s0 s1 s2 s3 s4)
	(cond ((zerop s0)
	       (setf (aref ans 0)(float-sign p0 0d0))
	       (setf (aref ans 1) 0d0)
	       (setf (aref ans 2) 0d0)
	       (setf (aref ans 3) 0d0))
	      (t
	       (setf (aref ans 0) s0)
	       (setf (aref ans 1) s1)
	       (setf (aref ans 2) s2)
	       (setf (aref ans 3) s3)))
	ans))))

(defun neg-oct-t (a target)
  (let((a0 0d0)(a1 0d0)(a2 0d0)(a3 0d0))
    (declare (type %quad-double a target)
	     (double-float a0 a1 a2 a3)(optimize (speed 3)))
    (oct-partsmac a0 a1 a2 a3 a)
    (setf (aref target 0) (cl:- a0))
    (setf (aref target 1) (cl:- a1))
    (setf (aref target 2) (cl:- a2))
    (setf (aref target 3) (cl:- a3))
    target))

(defun neg-oct-d-t (a target)
  (declare (type %quad-double a target)
	   (optimize (speed 3)))
    (setf (oct-0 target) (cl:- a))
    (setf (oct-1 target) 0d0)
    (setf (oct-2 target) 0d0)
    (setf (oct-3 target) 0d0)
    target)

(defun abs-oct-t(a target)
  (if (two-arg-< a +oct-zero+) (neg-oct-t a target) 
    (copy-octi-into-octi a target)))

(defun abs-oct-d-t (a target)
  (declare (type %quad-double a target)
	   (optimize (speed 3)))
    (setf (oct-0 target) (cl:abs a))
    (setf (oct-1 target) 0d0)
    (setf (oct-2 target) 0d0)
    (setf (oct-3 target) 0d0)
    target)

(defun sub-oct-t (a b target)
  (add-oct-t a (neg-oct b) target))

(defun sub-oct-d-t (a b target)
  (add-oct-d-t a (- b) target))

(defun add-oct-d-t (a b ans)
  "Add a quad-double A and a double-float B, store in ans"
  (declare (type %quad-double a)
	   (double-float b)
	   (optimize (speed 3)(safety 1)))
  (let ((c0 0.0d0)(c1 0.0d0)(c2 0.0d0)(c3 0.0d0) (e 0.0d0)
	(r0 0.0d0)(r1 0.0d0)(r2 0.0d0)(r3 0.0d0)	)
    (declare(double-float c0 c1 c2 c3 e r0 r1 r2 r3))
    (two-summac c0 e (aref a 0) b)
    (when (float-infinity-p c0)
      (setf (aref ans 0) c0)
      (setf (aref ans 1) 0d0)
      (setf (aref ans 2) 0d0)
      (setf (aref ans 3) 0d0)
      (return-from add-oct-d-t  ans))
    (two-summac c1 e (aref a 1) e)
    (two-summac c2 e (aref a 2) e)
    (two-summac c3 e (aref a 3) e)
    (renorm-5mac  r0 r1 r2 r3 c0 c1 c2 c3 e)
    (cond ((and (zerop (aref a 0)) (zerop b))
	         (setf (aref ans 0) c0)
		 (setf (aref ans 1) 0d0)
		 (setf (aref ans 2) 0d0)
		 (setf (aref ans 3) 0d0))
	  (t
	         (setf (aref ans 0) r0)
		 (setf (aref ans 1) r1)
		 (setf (aref ans 2) r2)
		 (setf (aref ans 3) r3)))
    ans))

(defun 1+-oct-t (a ans)(add-oct-d-t a 1d0 ans))
(defun 1--oct-t (a ans)(add-oct-d-t a -1d0 ans))


;;; end of octi arithmetic package

;;; see also oct/qd-methods for Ray's version of =, /=, >, etc etc
;;; Tiring to do it over again. Can we just use it?  I think I like my
;;; version with  ga::monotone.

(defun two-arg-< (a b)
  (declare (type (simple-array double-float (4)) a b)(optimize (speed 3)))
  (or (cl:< (oct-0 a)(oct-0 b))
      (and(cl:= (oct-0 a)(oct-0 b))
	  (cl:< (oct-1 a)(oct-1 b)))
      (and(cl:= (oct-1 a)(oct-1 b))
	  (cl:< (oct-2 a)(oct-2 b)))
      (and(cl:= (oct-2 a)(oct-2 b))
	  (cl:< (oct-3 a)(oct-3 b)))))

(defun two-arg-> (a b)
  (declare (type (simple-array double-float (4)) a b)(optimize (speed 3)))
  (or (cl:> (oct-0 a)(oct-0 b))
      (and(cl:= (oct-0 a)(oct-0 b))
	  (cl:> (oct-1 a)(oct-1 b)))
      (and(cl:= (oct-1 a)(oct-1 b))
	  (cl:> (oct-2 a)(oct-2 b)))
      (and(cl:= (oct-2 a)(oct-2 b))
	  (cl:> (oct-3 a)(oct-3 b)))))

(defun two-arg-= (a b)
  (declare (type (simple-array double-float (4)) a b)(optimize (speed 3)))
  (and(cl::= (oct-0 a)(oct-0 b))
      (cl::= (oct-1 a)(oct-1 b))
      (cl::= (oct-2 a)(oct-2 b))
      (cl::= (oct-3 a)(oct-3 b))))

(defun two-arg-/= (a b)(not (two-arg-= a b)))
(defun two-arg-<= (a b)(not(two-arg-> a b)))
(defun two-arg->= (a b)(not(two-arg-< a b)))
;; This does not work with NaNs; if we checked,  
;; all compares with NaNs are false (nil).

(defmacro plusp-oct (x)`(> (oct-0 ,x) 0d0)) ;sign will always be on 0th term
(defmacro minusp-oct (x)`(< (oct-0 ,x) 0d0))
(defun zerop-oct(x)(= (oct-0 x) 0d0))

(defun nint-oct (a)
  "Round the quad-float to the nearest integer, which is returned as a
  quad-float"
 (declare  (optimize (speed 3)(safety 1)))
  (let ((x0 (fround (oct-0 a)))
	(x1 0d0)
	(x2 0d0)
	(x3 0d0))
    (declare (double-float x0 x1 x2 x3))
    (cond ((= x0 (oct-0 a))
	   ;; First double is already an integer
	   (setf x1 (fround (oct-1 a)))
	   (cond ((= x1 (oct-1 a))
		  ;; Second is an integer
		  (setf x2 (fround (oct-2 a)))
		  (cond ((= x2 (oct-2 a))
			 ;; Third is an integer
			 (setf x3 (fround (oct-3 a))))
			(t
			 (when (and (zerop (abs (cl:- x2 (oct-2 a))))
				    (minusp (oct-3 a)))
			   (decf x2)))))
		 (t

		  (when (and (zerop (abs (cl:- x1 (oct-1 a))))
			     (minusp (oct-2 a)))
		    (decf x1)))))
	  (t
	   (when (and (zerop (abs (cl:- x0 (oct-0 a))))
		      (minusp (oct-1 a)))
	     (decf x0))))

    (let ((s0 0d0)(s1 0d0)(s2 0d0)(s3 0d0))
	  (renorm-4mac s0 s1 s2 s3  x0 x1 x2 x3)
	  (%make-oct-d s0 s1 s2 s3))))


(defun oct-=(x y)
  (declare (optimize (speed 3)))
  (and(= (the double-float (oct-0 x))(the double-float (oct-0 y)))
      (= (the double-float (oct-1 x))(the double-float (oct-1 y)))
      (= (the double-float (oct-2 x))(the double-float (oct-2 y)))
      (= (the double-float (oct-3 x))(the double-float (oct-3 y)))))

(defun incf-oct(x &optional (y +oct-one+) )
  (add-oct-t x (octi::into y) x ))

(defun decf-oct(x &optional (y +oct-one+) )
  (sub-oct-t x (octi::into y) x ))

(defun ffloor-oct  (num &optional (div 1))
  ;; return quotient q and remainder r such that q*div+r=num
  ;; quotient is returned as an oct. remainder is an oct.
  (multiple-value-bind (quo rem)
      (floor (oct2lisp num)(oct2lisp div))
    (values (into quo)(into rem))))

(defun fceiling-oct  (num &optional (div 1))
  ;; return quotient q and remainder r such that q*div+r=num
  ;; quotient is returned as an oct. remainder is an oct.
  (multiple-value-bind (quo rem)
      (ceiling (oct2lisp num)(oct2lisp div))
    (values (into quo)(into rem))))

(defun ftruncate-oct  (num &optional (div 1))
  ;; return quotient q and remainder r such that q*div+r=num
  ;; quotient is returned as an oct. remainder is an oct.
  (multiple-value-bind (quo rem)
      (truncate (oct2lisp num)(oct2lisp div))
    (values (into quo)(into rem))))

(defun fround-oct  (num &optional (div 1))
  ;; return quotient q and remainder r such that q*div+r=num
  ;; quotient is returned as an oct. remainder is an oct.
  (multiple-value-bind (quo rem)
      (round (oct2lisp num)(oct2lisp div))
    (values (into quo)(into rem))))

(defun float2form(f)(multiple-value-bind (signif expon sign)
			(integer-decode-float f)
		      (if (= signif 0) 0d0
		      `(scale-float (float ,(* sign signif) 1d0) ,expon))))

(defun dumpoct (a)
  `(%make-oct-d ,(float2form (oct-0 a))
		,(float2form (oct-1 a))
		,(float2form (oct-2 a))
		,(float2form (oct-3 a))))

(defun dumptab (q)
  `(make-array ,(length q) :initial-contents
	       ,(cons 'list
		     (map 'list #'dumpoct q))))

(defun drem-oct (a b)
  (declare (type %quad-double a b))
  (let ((n (nint-oct (div-oct a b))))
    (sub-oct a (mul-oct n b))))

(defun divrem-oct (a b)
  (declare (type %quad-double a b))
  (let ((n (nint-oct (div-oct a b))))
    (values n (sub-oct a (mul-oct n b)))))


(defun divrempos-oct (a b) ;; make remainder positive if a is pos.
  (declare (type %quad-double a b))
  (let* ((n (nint-oct (div-oct a b)))
	 (r (sub-oct a (mul-oct n b))))
    (when (and (cl:> (oct-0 a) 0d0) (cl:<  (oct-0 r) 0d0))
    ;  (format t "~% n=~s r=~s" n r)
      (sub-oct-d-t n 1d0 n)
      (add-oct-t r b r))
    (values n r )))
)

;; example:  (in-package :octi)  (add-oct (into 3)(into 4))

;;oct-to-decimal decoder. 
;;; this is OCT's oct, which is an array


(defun oct-decode(q &optional (digits 65) (base 10)) 
  ;; returns 3 integers: sign; fraction to right of dec.point; base-10 expon.
  (let ((r (apply #'cl::+ (map 'list  #'cl::rational (coerce q
							     'list)))))
  (decimalize  (abs r)
	       digits ;; could be more than 65 if you have 1.0+1.0q-80 !
	       (signum r)
	       base)))

;; convert non-neg. rational r to a decimal [or other base] number with n digits
;; showing in the fraction. If r=0, then just 0.
;; return sign=1,-1, or 0;  fraction is an integer, exponent is an integer.

(defun decimalize(r n sign &optional (base 10)); r rational number >=0, sign is +-1 or 0
  (let* ((expon  (largest-power-less-than r base))
	 (frac   (truncate;; was, round. causes tricky edge cases.
		  (cl:* (cl::abs r) (cl::expt base (cl::- n expon 1))))))
    (values     sign  frac
	      (cl:1+ expon))))

;; return z such that base^z<=r,  
;; not completely general. Works for
;; rational r, where r can be converted without overflow to double-float
;; Should be OK for oct numbers.

(defun largest-power-less-than(r base) ;; or equal
  (if (cl::= r 0) -1
  (let ((guess (cl::floor(cl::log (cl::* 1.0d0 (cl::abs r)) base))))
    (if (> (expt base guess) r) (1- guess) guess))))


;;(loop for i from 10 while (< i 20) do (print (list i (multiple-value-list (oct-decode (- 1 (expt #q10 (- i)))))) ))

;; make a formatted output . Something like this does the job

(defparameter *oct-digits-to-show* 66) ; MAX number of digits to show. Trailing zeros are omitted.

;; octi's oct ... just an array
(defmethod oct2string((x array))
  ;; check if x is a NaN
  (if (excl::nan-p (aref  x 0)) "NaN(oct)"
    (multiple-value-bind (s r e h)  ;h is extra variable
	(decimalize (abs(oct2lisp x)) *oct-digits-to-show* (signum (oct-0 x)) 10)
      (declare (ignore s))
      (format nil "~a0.~aQ~s" (if (cl:< (oct-0 x) 0)"-" "") 
	      (string-right-trim 
	       "0"  
	       (subseq (setf h(format nil "~a" r)) 0  
		       (cl::min (length h) *oct-digits-to-show*)) )
	      e))))

(defmethod oct2lisp((x array))
  (if (excl::nan-p (aref  x 0)) (aref x 0)
					; if oct contains a NaN, use it.
    (apply #'cl::+  (map 'list #'cl::rational  x ))))

(defmethod oct2lisp(y) y)

(defun rational-oct(x)(oct2lisp x));; 

;; this is octi::into

(defun into(r &optional (where (%make-oct 0d0 0d0 0d0 0d0)))
  (lisp2octi r where))

;; The following methods REQUIRE a place to put the answer, and so
;; are less convenient than "into" to use directly.

(defmethod lisp2octi((x ratio) (ans array)) ;to encode a lisp rational, divide num/den
 (div-oct-t (octi::into (numerator x))(octi::into (denominator x)) ans))

(defmethod lisp2octi((x fixnum) (ans array))  ;small integers are easy.
  (lisp2octi (coerce x 'double-float) ans))

(defmethod lisp2octi ((x float) (ans array)) ;single-float or double-floats fit here
  ;;(format t "~% float array to lisp2octi ~s, ~s" x ans)
  (loop for i from 1 to 3 do 
	(setf (aref ans i) 0.0d0))
  (setf (aref ans 0) (coerce x 'double-float))
  ans)


;; this works for bignums, also rationals
(defmethod lisp2octi ((x integer) (ans array))
  ;; bignum integers that are shorter than a double-float fraction are easy.
  (if (< (cl::abs x) #.(cl::expt 2 53)) (lisp2octi  (coerce x 'double-float) ans)

    (let* ((p (coerce x 'double-float))
	   (ans (lisp2octi p ans)))
      (dotimes (i 3 ans)
	(setf p (coerce (decf x (cl::rational p)) 'double-float))
	(add-oct-d-t ans p ans)))))

(defmethod lisp2octi((x array)(ans array)) ;; assume it is an octi
  (copy-octi-into-octi x ans)
  ans)

;;; ARITHMETIC, no targets


;; generate "functional" arithmetic programs like (add-oct x y) from the
;; programs (add-oct-t x y target),  which are implemented separately.

;;(define-compiler-macro add-oct(a b &optional (c (%make-oct 0d0 0d0 0d0 0d0)))  `(add-oct-t ,a ,b ,c))

(defmacro ss(name count)		;  make the define-compiler-macro stuff for name..
  (let* ((short-name (intern name :octi))
	 (targname(intern (concatenate 'string name "-t") :octi))
	 (arglist (case count 
		   (1 '(a &optional (c (%make-oct 0d0 0d0 0d0 0d0))))
		   (2 '(a &optional b (c (%make-oct 0d0 0d0 0d0 0d0))))
		   (otherwise (error "illegal count for ss macro" count))))
	 (thecall (case count
		    (1 (list targname 'a 'c))
		    (2 (list targname 'a 'b 'c)))))

    `(progn  ;; next line clumsy but works. or does it
   #+ignore    ,(subst short-name 'jj
	       (subst targname 
		      'kk 
		      `(define-compiler-macro jj ,arglist `(kk ,a ,b ,c))))
       (defun ,short-name ,arglist ,thecall))))

(ss "add-oct" 2)(ss "add-oct-d" 2)
(ss "mul-oct" 2)(ss "mul-oct-d" 2)
(ss "sub-oct" 2)(ss "sub-oct-d" 2)
(ss "div-oct" 2)(ss "div-oct-d" 2)

(ss "invert-oct" 1)(ss "invert-oct-d" 1)
(ss "sqr-oct" 1) (ss "abs-oct" 1) (ss "neg-oct" 1)
(ss "1+-oct" 1) (ss "1--oct" 1)

;;(ss "hypot-oct" 2)(ss "expt-oct" 2)
;;(ss "sin-oct" 1) (ss "cos-oct" 1)(ss "tan-oct" 1) 
;;(ss "sinh-oct" 1) (ss "cosh-oct" 1)(ss "tanh-oct" 1)
;;(ss "asin-oct" 1) (ss "acos-oct" 1) (ss "atan-oct" 1 )
;;(ss "log-oct" 1) 
;;(ss "exp-oct" 1) (ss "sqrt-oct" 1)

;; see below for ffloor, fceiling, ftruncate, fround, which we don't 
;; provide -t  versions for.

;;; here are the arithmetic routines (with targets)

(defun add-oct-t (a b target) ;; compare to add-oct
  (declare (type %quad-double a b target)
	   (optimize (speed 3) (space 0) (safety 1)))
  

  ;; ray's notes:
  ;; This is the version that is NOT IEEE.  Should we use the IEEE
  ;; version?  It's quite a bit more complicated.
  ;;
  ;; In addition, this is reorganized to minimize data dependency.
  (let ((a0 0d0)(a1 0d0)(a2 0d0)(a3 0d0)
	(b0 0d0)(b1 0d0)(b2 0d0)(b3 0d0))
    (declare(double-float a0 a1 a2 a3 b0 b1 b2 b3))
    
      (oct-partsmac a0 a1 a2 a3 a)
      (oct-partsmac b0 b1 b2 b3 b)
      (let* ((s0 (cl:+ a0 b0))	     (s1 (cl:+ a1 b1))	     (s2 (cl:+ a2 b2))	     (s3 (cl:+ a3 b3))
	     (v0 (cl:- s0 a0))	     (v1 (cl:- s1 a1))	     (v2 (cl:- s2 a2))	     (v3 (cl:- s3 a3))
	     (u0 (cl:- s0 v0))	     (u1 (cl:- s1 v1))	     (u2 (cl:- s2 v2))	     (u3 (cl:- s3 v3))
	     (w0 (cl:- a0 u0))	     (w1 (cl:- a1 u1))	     (w2 (cl:- a2 u2))	     (w3 (cl:- a3 u3)))
	(let* (    (u0 (cl:- b0 v0))  	      (u1 (cl:- b1 v1))	      (u2 (cl:- b2 v2))	      (u3 (cl:- b3 v3))
		   (t0 (cl:+ w0 u0))	      (t1 (cl:+ w1 u1))	      (t2 (cl:+ w2 u2))	      (t3 (cl:+ w3 u3)))

	  (declare (double-float t0 t1 t2 t3 u0 u1 u2 u3))
	  (two-summac s1 t0 s1 t0)
	  (three-summac  s2 t0 t1 s2 t0 t1)
	  (three-sum2mac s3 t0 s3 t0 t2)
	  (setf t0 (cl:+ t0 t1 t3))
	  ;; Renormalize
	 ;; (format t "~% renorm ~s ~s ~s ~s ~s" s0 s1 s2 s3 t0)
	  (renorm-5mac s0 s1 s2 s3   s0 s1 s2 s3 t0))
	(cond ((and (zerop a0) (zerop b0))
	       (setf (aref target 0) 0d0) (setf (aref target 1) 0d0) (setf (aref target 2) 0d0) (setf (aref target 3) 0d0))
	      (t
	       (setf (aref target 0) s0)  (setf (aref target 1) s1)  (setf (aref target 2) s2)  (setf (aref target 3) s3)))
	target)))


(defun mul-oct-t (a b target)  ;;; compare to mul-oct!
  (declare (type %quad-double a b target)
	   (optimize (speed 3)
		     (safety 1)))
  (let ((a0 0d0)(a1 0d0)(a2 0d0)(a3 0d0)
	(b0 0d0)(b1 0d0)(b2 0d0)(b3 0d0)
	(p0 0d0)(p1 0d0)(p2 0d0)(p3 0d0)(p4 0d0)(p5 0d0)
	(q0 0d0)(q1 0d0)(q2 0d0)(q3 0d0)(q4 0d0)(q5 0d0)
	)
    (declare(double-float a0 a1 a2 a3 b0 b1 b2 b3 p0 p1 p2 p3 p4 p5
			  q0 q1 q2 q3 q4 q5))
    (oct-partsmac a0 a1 a2 a3 a)
    (oct-partsmac b0 b1 b2 b3 b)
    (two-prodmac p0 q0 a0 b0)
  
    (cond((float-infinity-p p0)
	  (setf (aref target 0) p0)
	  (setf (aref target 1) 0d0)
	  (setf (aref target 2) 0d0)
	  (setf (aref target 3) 0d0)
	  (return-from mul-oct-t target)))
    (two-prodmac p1 q1 a0 b1)
    (two-prodmac p2 q2 a1 b0)
    (two-prodmac p3 q3 a0 b2)
    (two-prodmac p4 q4 a1 b1)
    (two-prodmac p5 q5 a2 b0)
    ;; Start accumulation
    (three-summac p1 p2 q0 p1 p2 q0)

    ;; six-three-sum of p2, q1, q2, p3, p4, p5
    (three-summac p2 q1 q2 p2 q1 q2)
    (three-summac p3 p4 p5 p3 p4 p5)
    ;; Compute (s0,s1,s2) = (p2,q1,q2) + (p3,p4,p5)
    (let ((s0 0d0)(s1 0d0)
	  (t0 0d0)(t1 0d0)
	  (r0 0d0)(r1 0d0)
	  (s2(cl:+ q2 p5)))
      (declare (double-float s0 s1 s2 t0 t1 r0 r1))
      (two-summac s0 t0 p2 p3)
      (two-summac s1 t1 q1 p4)
      (two-summac s1 t0 s1 t0)
      (incf s2 (cl:+ t0 t1))
	;; O(eps^3) order terms.  This is the sloppy
	;; multiplication version.  Should we use
	;; the precise version?  It's significantly
	;; more complex.
			  
      (incf s1 (cl:+ (cl:* a0 b3)
		     (cl:* a1 b2)
		     (cl:* a2 b1)
		     (cl:* a3 b0)
		     q0 q3 q4 q5))
     ;; (format t "p0,p1,s0,s1,s2 = ~a ~a ~a ~a ~a~%"	      p0 p1 s0 s1 s2)
      (renorm-5mac r0 r1 s0 s1   p0 p1 s0 s1 s2)
      (if (zerop r0)
	  (progn
	    (setf (aref target 0) p0)
	    (setf (aref target 1) 0d0)
	    (setf (aref target 2) 0d0)
	    (setf (aref target 3) 0d0))
	(progn
	  (setf (aref target 0) r0)
	  (setf (aref target 1) r1)
	  (setf (aref target 2) s0)
	  (setf (aref target 3) s1)))))
  target)
      

;; this is an example of a hand-coded program using destructive
;; operations.

(defun sqrt-oct-t(a ans)
  (let* ((as (oct-0 a))			; approx to z, the first 53 bits.
	 (sqr0 (sqrt as))		; appx squareroot of z
	 (sqr1 (%make-oct-d sqr0))	; convert approx to oct
	 )
    (cond ((cl::zerop sqr0)(copy-octi-into-octi +oct-zero+ ans))
	  (t				;3 Newton iterations.
	 ;;  (copy-octi-into-octi +oct-zero+ ans)
	   (mul-oct-d-t (add-oct-t sqr1 (div-oct-t a sqr1 ans) ans) 0.5d0 sqr1)
	   (mul-oct-d-t (add-oct-t sqr1 (div-oct-t a sqr1 ans) ans) 0.5d0 sqr1)
	   (mul-oct-d-t (add-oct-t sqr1 (div-oct-t a sqr1 ans) ans) 0.5d0 ans)
	   ans))))

;; inversion is 1/x

(defun invert-oct-t(v h2) ;;h2 is the target.
  ;; a quartic newton iteration for 1/v
  ;; to invert v, start with a good guess, x.
  ;; let h= 1-v*x  ;; h is small
  ;; return x+ x*(h+h^2+h^3) . compute h^3 in double-float
  ;; enough accuracy.

  (let* 
      ((x (%make-oct (cl:/ (aref v 0)) 0d0 0d0 0d0))
       (h (%make-oct 0d0 0d0 0d0 0d0)) ;;allocate space for temp
       (h3 h)
       )
    (declare  (type %quad-double v h h2)(double-float h3))    
    (add-oct-d-t  (neg-oct-t(mul-oct-t v x h) h) 1.0d0 h) ;set h
    (mul-oct-t h h h2)		;also use h2 for target
    (setf h3 (* (aref h 0)(aref h2 0)))
    (add-oct-t x 
	      (mul-oct-t x
			(add-oct-t h (add-oct-d-t h2 
					      h3 
					      h2) h2) h2) h2) ))

(defun invert-oct-d-t(v h2) ;; v is a double-float h2 is the target.
  ;; a quartic newton iteration for 1/v
  ;; to invert v, start with a good guess, x.
  ;; let h= 1-v*x  ;; h is small
  ;; return x+ x*(h+h^2+h^3) . compute h^3 in double-float
  ;; enough accuracy.

  (let* 
      ((x (%make-oct (cl:/ v) 0d0 0d0 0d0))
       (h(%make-oct 0d0 0d0 0d0 0d0)) ;;allocate space for temp
       (h3 h))
    (declare  (type %quad-double h h2)(double-float v h3))    
    (add-oct-d-t  (neg-oct-t
		  ;;(mul-oct-d-t x v h)
		  (mul-oct-d x v)
		  h)1.0d0 h)		;set h
    (setf h2 (mul-oct h h))		;also use h2 for target
    (setf h3 (* (aref h 0)(aref h2 0)))
    (add-oct-t x 
	      (mul-oct-t x
			(add-oct-t h (add-oct-d-t h2 
					      h3 
					      h2) h2) h2) h2)))
				 
;; maybe check for division by zero or infinity?

(defun div-oct-t(u v ans);;
  (mul-oct-t u (invert-oct-t v #.(%make-oct 0d0 0d0 0d0 0d0)) ans))

(defun div-oct-d-t(u v ans);;
    (mul-oct-t u (invert-oct-d-t v ans) ans))

(defun sqr-oct-t(a ans)
    ;; cheap hack for a wasteful program
  (mul-oct-t a a ans)
  ans)

;; compute power of oct to an integer, store in ans
(defun pow-oct-i-t(a n ans)			; n is positive integer
  ;; this is generalized in expt-oct.
  (cond((cl:zerop n) (setf (aref ans 0) 1d0) ans)
       ((cl:evenp n)(sqr-oct-t (pow-oct-i-t a (cl:ash n -1) ans) ans))
       (t (mul-oct-t a (pow-oct-i a (1- n)) ans))))

(defun pow-oct-i(a n) (pow-oct-i-t a n (%make-oct-d 0d0)))

(defun expt-oct-t(a b ans) ;; a^b, where a, b are both octs.
  ;;a^b = exp(b*log(a))
  ;; first see if b is really an integer.
  (let* ((r (oct-0 b))
	 (s (round r)))
       
    (if (and (= r s)(= 0d0 (oct-1 b) (oct-2 b) (oct-3 b)))
	(pow-oct-i-t a s ans)
      (exp-oct-t (mul-oct b (log-oct a)) ans))))

(defun mul-oct-d-t (a b ans)
  "Multiply quad-double A with B, result put in ans"
  (declare (type %quad-double a ans)
	   (double-float b)
	   (optimize (speed 3)(space 0)(safety 1)))
  (let ((p0 0d0)(q0 0d0)(p1 0d0)(q1 0d0) (p2 0d0)(q2 0d0))
    (declare (double-float p0 q0 p1 q1 p2 q2))
    (two-prodmac p0 q0 (oct-0 a) b)
    (when (float-infinity-p p0)
      (setf (oct-0 0)p0)
      (return-from mul-oct-d-t ans))
    (two-prodmac p1 q1 (oct-1 a) b)
    (two-prodmac p2 q2 (oct-2 a) b)
    (let* ((p3 (cl:* (oct-3 a) b))
	   (s0 p0)
	   (s1 p0)
	   (s2 p0))
      (declare (double-float s0 s1 s2 p3))
      (two-summac s1 s2 q0 p1)
      (three-summac s2 q1 p2 s2 q1 p2)
      (three-sum2mac q1 q2 q1 q2 p3)
      (let ((s3 q1)
	    (s4 (cl:+ q2 p2)))
	(renorm-5mac s0 s1 s2 s3  s0 s1 s2 s3 s4)
	(cond ((zerop s0)
	       (setf (aref ans 0)(float-sign p0 0d0))
	       (setf (aref ans 1) 0d0)
	       (setf (aref ans 2) 0d0)
	       (setf (aref ans 3) 0d0))
	      (t
	       (setf (aref ans 0) s0)
	       (setf (aref ans 1) s1)
	       (setf (aref ans 2) s2)
	       (setf (aref ans 3) s3)))
	ans))))

(defun neg-oct-t (a target)
  (let((a0 0d0)(a1 0d0)(a2 0d0)(a3 0d0))
    (declare (type %quad-double a target)
	     (double-float a0 a1 a2 a3)(optimize (speed 3)))
    (oct-partsmac a0 a1 a2 a3 a)
    (setf (aref target 0) (cl:- a0))
    (setf (aref target 1) (cl:- a1))
    (setf (aref target 2) (cl:- a2))
    (setf (aref target 3) (cl:- a3))
    target))

(defun neg-oct-d-t (a target)
  (declare (type %quad-double a target)
	   (optimize (speed 3)))
    (setf (oct-0 target) (cl:- a))
    (setf (oct-1 target) 0d0)
    (setf (oct-2 target) 0d0)
    (setf (oct-3 target) 0d0)
    target)

(defun abs-oct-t(a target)
  (if (two-arg-< a +oct-zero+) (neg-oct-t a target) 
    (copy-octi-into-octi a target)))

(defun abs-oct-d-t (a target)
  (declare (type %quad-double a target)
	   (optimize (speed 3)))
    (setf (oct-0 target) (cl:abs a))
    (setf (oct-1 target) 0d0)
    (setf (oct-2 target) 0d0)
    (setf (oct-3 target) 0d0)
    target)

(defun sub-oct-t (a b target)
  (add-oct-t a (neg-oct b) target))

(defun sub-oct-d-t (a b target)
  (add-oct-d-t a (- b) target))

(defun add-oct-d-t (a b ans)
  "Add a quad-double A and a double-float B, store in ans"
  (declare (type %quad-double a)
	   (double-float b)
	   (optimize (speed 3)(safety 1)))
  (let ((c0 0.0d0)(c1 0.0d0)(c2 0.0d0)(c3 0.0d0) (e 0.0d0)
	(r0 0.0d0)(r1 0.0d0)(r2 0.0d0)(r3 0.0d0)	)
    (declare(double-float c0 c1 c2 c3 e r0 r1 r2 r3))
    (two-summac c0 e (aref a 0) b)
    (when (float-infinity-p c0)
      (setf (aref ans 0) c0)
      (setf (aref ans 1) 0d0)
      (setf (aref ans 2) 0d0)
      (setf (aref ans 3) 0d0)
      (return-from add-oct-d-t  ans))
    (two-summac c1 e (aref a 1) e)
    (two-summac c2 e (aref a 2) e)
    (two-summac c3 e (aref a 3) e)
    (renorm-5mac  r0 r1 r2 r3 c0 c1 c2 c3 e)
    (cond ((and (zerop (aref a 0)) (zerop b))
	         (setf (aref ans 0) c0)
		 (setf (aref ans 1) 0d0)
		 (setf (aref ans 2) 0d0)
		 (setf (aref ans 3) 0d0))
	  (t
	         (setf (aref ans 0) r0)
		 (setf (aref ans 1) r1)
		 (setf (aref ans 2) r2)
		 (setf (aref ans 3) r3)))
    ans))

(defun 1+-oct-t (a ans)(add-oct-d-t a 1d0 ans))
(defun 1--oct-t (a ans)(add-oct-d-t a -1d0 ans))


;;; end of octi arithmetic package

;;; see also oct/qd-methods for Ray's version of =, /=, >, etc etc
;;; Tiring to do it over again. Can we just use it?  I think I like my
;;; version with  ga::monotone.

(defun two-arg-< (a b)
  (declare (type (simple-array double-float (4)) a b)(optimize (speed 3)))
  (or (cl:< (oct-0 a)(oct-0 b))
      (and(cl:= (oct-0 a)(oct-0 b))
	  (cl:< (oct-1 a)(oct-1 b)))
      (and(cl:= (oct-1 a)(oct-1 b))
	  (cl:< (oct-2 a)(oct-2 b)))
      (and(cl:= (oct-2 a)(oct-2 b))
	  (cl:< (oct-3 a)(oct-3 b)))))

(defun two-arg-> (a b)
  (declare (type (simple-array double-float (4)) a b)(optimize (speed 3)))
  (or (cl:> (oct-0 a)(oct-0 b))
      (and(cl:= (oct-0 a)(oct-0 b))
	  (cl:> (oct-1 a)(oct-1 b)))
      (and(cl:= (oct-1 a)(oct-1 b))
	  (cl:> (oct-2 a)(oct-2 b)))
      (and(cl:= (oct-2 a)(oct-2 b))
	  (cl:> (oct-3 a)(oct-3 b)))))

(defun two-arg-= (a b)
  (declare (type (simple-array double-float (4)) a b)(optimize (speed 3)))
  (and(cl::= (oct-0 a)(oct-0 b))
      (cl::= (oct-1 a)(oct-1 b))
      (cl::= (oct-2 a)(oct-2 b))
      (cl::= (oct-3 a)(oct-3 b))))

(defun two-arg-/= (a b)(not (two-arg-= a b)))
(defun two-arg-<= (a b)(not(two-arg-> a b)))
(defun two-arg->= (a b)(not(two-arg-< a b)))
;; This does not work with NaNs; if we checked,  
;; all compares with NaNs are false (nil).

(defmacro plusp-oct (x)`(> (oct-0 ,x) 0d0)) ;sign will always be on 0th term
(defmacro minusp-oct (x)`(< (oct-0 ,x) 0d0))
(defun zerop-oct(x)(= (oct-0 x) 0d0))

(defun nint-oct (a)
  "Round the quad-float to the nearest integer, which is returned as a
  quad-float"
 (declare  (optimize (speed 3)(safety 1)))
  (let ((x0 (fround (oct-0 a)))
	(x1 0d0)
	(x2 0d0)
	(x3 0d0))
    (declare (double-float x0 x1 x2 x3))
    (cond ((= x0 (oct-0 a))
	   ;; First double is already an integer
	   (setf x1 (fround (oct-1 a)))
	   (cond ((= x1 (oct-1 a))
		  ;; Second is an integer
		  (setf x2 (fround (oct-2 a)))
		  (cond ((= x2 (oct-2 a))
			 ;; Third is an integer
			 (setf x3 (fround (oct-3 a))))
			(t
			 (when (and (zerop (abs (cl:- x2 (oct-2 a))))
				    (minusp (oct-3 a)))
			   (decf x2)))))
		 (t

		  (when (and (zerop (abs (cl:- x1 (oct-1 a))))
			     (minusp (oct-2 a)))
		    (decf x1)))))
	  (t
	   (when (and (zerop (abs (cl:- x0 (oct-0 a))))
		      (minusp (oct-1 a)))
	     (decf x0))))

    (let ((s0 0d0)(s1 0d0)(s2 0d0)(s3 0d0))
	  (renorm-4mac s0 s1 s2 s3  x0 x1 x2 x3)
	  (%make-oct-d s0 s1 s2 s3))))


(defun oct-=(x y)
  (declare (optimize (speed 3)))
  (and(= (the double-float (oct-0 x))(the double-float (oct-0 y)))
      (= (the double-float (oct-1 x))(the double-float (oct-1 y)))
      (= (the double-float (oct-2 x))(the double-float (oct-2 y)))
      (= (the double-float (oct-3 x))(the double-float (oct-3 y)))))

(defun incf-oct(x &optional (y +oct-one+) )
  (add-oct-t x (octi::into y) x ))

(defun decf-oct(x &optional (y +oct-one+) )
  (sub-oct-t x (octi::into y) x ))

(defun ffloor-oct  (num &optional (div 1))
  ;; return quotient q and remainder r such that q*div+r=num
  ;; quotient is returned as an oct. remainder is an oct.
  (multiple-value-bind (quo rem)
      (floor (oct2lisp num)(oct2lisp div))
    (values (into quo)(into rem))))

(defun fceiling-oct  (num &optional (div 1))
  ;; return quotient q and remainder r such that q*div+r=num
  ;; quotient is returned as an oct. remainder is an oct.
  (multiple-value-bind (quo rem)
      (ceiling (oct2lisp num)(oct2lisp div))
    (values (into quo)(into rem))))

(defun ftruncate-oct  (num &optional (div 1))
  ;; return quotient q and remainder r such that q*div+r=num
  ;; quotient is returned as an oct. remainder is an oct.
  (multiple-value-bind (quo rem)
      (truncate (oct2lisp num)(oct2lisp div))
    (values (into quo)(into rem))))

(defun fround-oct  (num &optional (div 1))
  ;; return quotient q and remainder r such that q*div+r=num
  ;; quotient is returned as an oct. remainder is an oct.
  (multiple-value-bind (quo rem)
      (round (oct2lisp num)(oct2lisp div))
    (values (into quo)(into rem))))

(defun float2form(f)(multiple-value-bind (signif expon sign)
			(integer-decode-float f)
		      (if (= signif 0) 0d0
		      `(scale-float (float ,(* sign signif) 1d0) ,expon))))

(defun dumpoct (a)
  `(%make-oct-d ,(float2form (oct-0 a))
		,(float2form (oct-1 a))
		,(float2form (oct-2 a))
		,(float2form (oct-3 a))))

(defun dumptab (q)
  `(make-array ,(length q) :initial-contents
	       ,(cons 'list
		     (map 'list #'dumpoct q))))

(defun drem-oct (a b)
  (declare (type %quad-double a b))
  (let ((n (nint-oct (div-oct a b))))
    (sub-oct a (mul-oct n b))))

(defun divrem-oct (a b)
  (declare (type %quad-double a b))
  (let ((n (nint-oct (div-oct a b))))
    (values n (sub-oct a (mul-oct n b)))))


(defun divrempos-oct (a b) ;; make remainder positive if a is pos.
  (declare (type %quad-double a b))
  (let* ((n (nint-oct (div-oct a b)))
	 (r (sub-oct a (mul-oct n b))))
    (when (and (cl:> (oct-0 a) 0d0) (cl:<  (oct-0 r) 0d0))
    ;  (format t "~% n=~s r=~s" n r)
      (sub-oct-d-t n 1d0 n)
      (add-oct-t r b r))
    (values n r )))


;; example:  (in-package :octi)  (add-oct (into 3)(into 4))

;; lacking:  treatment of infinity and NaN.

;; this file does not have nice OUTPUT of octi.
;; here's some stuff, commented out, because it isn't necessary for minimal set

#|

;;oct-to-decimal decoder. 
;;; this is OCTI's oct, which is an array

(defun oct-decode(q &optional (digits 65) (base 10)) 
  ;; returns 3 integers: sign; fraction to right of dec.point; base-10 expon.
  (let ((r (apply #'cl::+ (map 'list  #'cl::rational (coerce q
							     'list)))))
  (decimalize  (abs r)
	       digits ;; could be more than 65 if you have 1.0+1.0q-80 !
	       (signum r)
	       base)))

;; convert non-neg. rational r to a decimal [or other base] number with n digits
;; showing in the fraction. If r=0, then just 0.
;; return sign=1,-1, or 0;  fraction is an integer, exponent is an integer.

(defun decimalize(r n sign &optional (base 10)); r rational number >=0, sign is +-1 or 0
  (let* ((expon  (largest-power-less-than r base))
	 (frac   (truncate;; was, round. causes tricky edge cases.
		  (cl:* (cl::abs r) (cl::expt base (cl::- n expon 1))))))
    (values     sign  frac
	      (cl:1+ expon))))

;; return z such that base^z<=r,  
;; not completely general. Works for
;; rational r, where r can be converted without overflow to double-float
;; Should be OK for oct numbers.

(defun largest-power-less-than(r base) ;; or equal
  (if (cl::= r 0) -1
  (let ((guess (cl::floor(cl::log (cl::* 1.0d0 (cl::abs r)) base))))
    (if (> (expt base guess) r) (1- guess) guess))))

;; make a formatted output . Something like this does the job

(defparameter *oct-digits-to-show* 66) ; MAX number of digits to show. Trailing zeros are omitted.

(defmethod oct2string((x array))
  ;; check if x is a NaN
  (if (excl::nan-p (aref  x 0)) "NaN(oct)"
    (multiple-value-bind (s r e h)  ;h is extra variable
	(decimalize (abs(oct2lisp x)) *oct-digits-to-show* (signum (oct-0 x)) 10)
      (declare (ignore s))
      (format nil "~a0.~aQ~s" (if (cl:< (oct-0 x) 0)"-" "") 
	      (string-right-trim 
	       "0"  
	       (subseq (setf h(format nil "~a" r)) 0  
		       (cl::min (length h) *oct-digits-to-show*)) )
	      e))))

(oct2string  (div-oct (into 1)(into 3))) ==> "0.33333333333333333333333333333333333333333333333333333333333333333Q0"

|#








