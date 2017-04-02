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
	 (setf ,newb (cl:+ ,t2 ,t3) )))))


;;; see also oct/qd-methods for Ray's version of =, /=, >, etc etc
;;; Tiring to do it over again. Can we just use it?  I think I like my
;;; version with  ga::monotone.

(defun octi::two-arg-< (a b)
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

#| oct can record some numbers to very high accuracy if they can
 be encoded as the sum of two floats of very different exponent.
 Seeing this is not easy; you must either print to many places
 or look at the internal encoding.

 To 65 places, these two numbers look the same:
 (setf r1  (into (expt 2 256))) ; 2^256
 (setf r2  (add-oct (into (expt 2 256))(into (expt 2 -256)))); 2^256+2^(-256)
 (oct-decode r1)   
 (oct-decode r2)

but if you look at 160 decimal digits, 
 (oct-decode  r1 160) 
 (oct-decode  r2 160) 

you see they differ.

 Probably a better way to look at the numbers is in binary.
	     
(oct-decode  r2 200 10)

Another set of anomalies occurs at the lower limits (small numbers).
On the system I am using at the moment, the (gradually underflowing)
least-positive-double-float number is lpdf= 5.0d0-324.  Encoding that
as an OCT does not provide any room for more precision, and indeed
multiplying lpdf by 1.01d0 does not change it at all.

Results are not quite so extreme if we look at the (not underflowing)
least-positive-normalized-double-float, lpndf=2.2250738585072014d-308.

Here we can multiply lpndf by 1.01d0 and get a different number, but
even so, numbers of this magnitude are not represented to quad
precision: much of the precision is washed out in underflow of the
lower-order terms.

|#

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

;; how about a function that returns 2 answers, like sincos?



#|

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


;;(oct2string  (div-oct (into 1)(into 3))) ==> "0.33333333333333333333333333333333333333333333333333333333333333333Q0"

|#









;; implementing dsetv and with-temps for OCTI  (not oct)

;; started with dsetv version of dec 6, 2007
;; probably should feed back the changes with block, progn, etc.

;; probs?  what to do with  (macroexpand '(dsetv w (let ((x 3)(y 4))(+ x y))))
;; an error?
;;  (macroexpand '(dsetv w (let ((x (into 3))(y (into 4)))(+ x y))))
;; works better.


(in-package :octi)
(eval-when (load compile eval)
  
(mapc #'(lambda(r)
	    (let ((op (first r))
		  (progname (second r))
		  (octi-targ (third r)))
	  (setf (get op 'argnum) 2)
	  (setf (get progname 'argnum) 2)
	  (setf (get op 'octi-program) octi-targ)
	  (setf (get progname 'octi-program) octi-targ) ;; after macroexpand-all
	  ) )
	
	;; depending on whose package you are using, either +  or two-arg-+
	;; or maybe excl::+_2op
	;; might appear, after macro-expansion.
	'((+ two-arg-+ add-qd-t)
	  (excl::+_2op two-arg-+ add-qd-t)
	  (- two-arg-- sub-qd-t)
	  (excl::-_2op two-arg-- sub-qd-t)
	  (* two-arg-* mul-qd-t)
	  (excl::*_2op two-arg-* mul-qd-t)
	  (/ two-arg-/ div-qd-t)
	  (excl::/_2op two-arg-/ div-qd-t) ;;etc
	  
	  (excl::>_2op  two-arg->  >-qd-t )  ;; see below
	  (excl::>=_2op two-arg->= >=-qd-t)
	  (excl::<_2op  two-arg-<  <-qd-t )
	  (excl::<=_2op two-arg-<= <=-qd-t)
	  (excl::=_2op  two-arg-=  =-qd-t )
	  (excl::/=_2op two-arg-/= /=-qd-t)
	  
	      ))
  
  ;; it's not so neat to have  (excl::>_2op two-arg-> >-qd-t)
  ;; because the target isn't an octi, nor is the target necessary at all.
  ;
  
  ;; we need another class of programs: 2 arg functions returning true/false.
  ;; more leg work.

(defun compare-qd(a b)   ;; a<b -1.  a=b: 0.  a>b:1.  NaN : nil.
    (cond((or (excl::nan-p (aref a 0))
	      (excl::nan-p (aref b 0)))
	  nil) ; no comparison with NaN returns true
	
	 (t (dotimes (i 4 0) ; equal
	   (cond ((= (aref a i)(aref b i))) ; keep going if equal
		 ((> (aref a i)(aref b i)) (return 1))
		 (t (return -1)))))))

(defun >-qd-t (a b ignore)
  (declare (ignore ignore))
  (eq 1 (compare-qd a b)))
(defun <-qd-t (a b ignore)
  (declare (ignore ignore))
  (eq -1 (compare-qd a b)))
(defun >=-qd-t (a b ignore)
    (declare (ignore ignore))
  (let ((compare-qd a b))
    (or (eq 0 h)(eq 1 h))))
(defun <=-qd-t (a b ignore)
    (declare (ignore ignore))
  (let ((compare-qd a b))
    (or (eq 0 h)(eq -1 h))))

(defun =-qd-t(a b ignore)
      (declare (ignore ignore))
  (eq 0 (compare-qd a b)))
(defun /=-qd-t(a b ignore)
      (declare (ignore ignore))
  (not (eq 0 (compare-qd a b))))


;; lisp has =, /=, <, >, <=, >=
  
  
  ;; one-arg functions
   (mapc #'(lambda(r)
	    (let ((op (first r))
		  (progname (second r))
		  (octi-targ (third r)))
	  (setf (get op 'argnum) 1)
	  (setf (get progname 'argnum) 1)
	  (setf (get op 'octi-program) octi-targ)
	  (setf (get progname 'octi-program) octi-targ) ;; after macroexpand-all
	  ) )
	 '(
	   ;; controversy: should sqrt/sin/cos/tan
	   ;; be supported by dsetv in compiled mode, or just
	   ;; by consing up the data type?
	   ;; if supported, we must implement sin-qd-t etc in octi.
	   ;; if unsupported, all we need is sin  in quad-real
	   ;; it also means that sqrt can return a quad-complex.
	   ;;etc
	      ))
   
  ;; we might wish to distinguish expt from power [integer exponent]
  

;; this works only if we have a full oct compiler environment, which we need if the rest of this is to work.

(defmacro dsetv
  (targ ex)
  ;; try  (dsetv a (+ b c)) 
  ;; should be faster than (setf a (+ b c)). maybe 2X.
  ;; All the logic below is done during macro-expansion,
  ;; which means it is usually done at compile time. Run time
  ;; is therefore not penalized for macro-expansion, and benefits because
  ;; temporaries are allocated once, regardless of how many times the
  ;; program is executed.  If you use dsetv from an interpreted
  ;; program it will be slow, however, because it will do the macro
  ;; expansion followed by the execution, each time it is used.
  ;;(setf ex (macroexpand ex))  
  ;; rjf's qd version expanded (+ a b c) into (two-arg-+ a (two-arg-+ b c))
  ;; Not so for oct, which leaves + as a function, but defines a compiler macro.
  ;;so we do this
  (setf ex (cmexpand ex))		;check for compiler macro, too
  
   ;(format t "~% expanding ex in dsetv, ex=~s" ex)
  
  (cond 
   ((atom ex) `(into ,ex ,targ))
   ((eq (car ex) 'into) `(into ,@(cdr ex)  ,targ))
   (t ;;(setf ex (cmexpand `(with-temps ,ex)))
    ;;(setf ex (cmexpand ex))
    (let* ((op (car ex))
	   (args (cdr ex))
	   (the-op (get op 'octi-program))
	   (argnum (get op 'argnum))
	   ;;(theval (cmexpand (with-temps `(,op ,@ args))))
	 ;;  (theval (cmexpand (with-temps  ex)))
	   )
      (cond 	       
       ((not the-op);; not a previously listed op
	`(copy-octi ,(cmexpand `(with-temps ,ex)) ,targ))
       ((not (eql argnum (length args))) 
	(error "dsetv was given operator ~s which expects ~s args, but was given ~s --  ~s" 
	       op argnum (length args) args))
       (t
	(case argnum
	  (1;; one argument.
	  ;; (format t "~% one arg ~s" args)
	   `(,the-op  ,(cmexpand `(with-temps ,(car args))) , targ))

	  (2
	   

	      `(,the-op ,(cmexpand `(with-temps ,(car args)))
		       ,(cmexpand `(with-temps ,(cadr args)))   ,targ))

	  (otherwise (error "argnum is wrong for op ~s " op))
	  )))))))

(defmacro with-temps(expr)
  (let ((*names* nil)
	(*howmany* 0))
     ;; (format t "~% with-temps expanding ~s " expr)
    (labels ((genlist(n)(loop for i from 1 to n collect (into i)));make a list of fresh qd items
	     (ct1 (r);; count temporaries needed
		  (cond ((numberp r) (incf *howmany*))
			((not (consp r)) r)
			(t (incf *howmany*)
			   (mapc #'ct1 (cdr r)))))
		
	     (maketemps(r)		;change r=(+ a (* b c)) to  temp storage .
	       
	    ;;   (format t "~% expand with-temps for r=~s" r)
		       (cond ((numberp r) (into r))
			     ((atom r) r)
			     ((get (car r) 'argnum); known operator
			      `(dsetv ,(pop *names*)
				      ,(cons (car r)(mapcar #'maketemps (cdr r)))))
			     ;; just a symbol name? maybe aref? better be the right type, a qd.
			   
			     ((member (car r) '(let let*))
   
			      (cons (car r); let or let*
				    (cons (mapcar 
					   #'(lambda(x); pair: name, value
					       (list (car x); variable
						     ;;(cmexpand `(with-temps ,(cadr x)))
						     (cmexpand (cadr x))
						     
						     ))
					   (cadr r))
					  (mapcar #'(lambda(x)
						      (cmexpand `(with-temps ,x)))(cddr r))
	       
					  )))
			     ((member (car r) '(block tagbody if))
			     ;; (format t "~%block, tagbody if =~s" r)
			       
				   
					  (mapcar #'(lambda(x)
						      (cmexpand `(with-temps ,x))) r ))
	       
			      
			     (;;(member (car r) '(setq setf progn block tagbody psetq))
			      t
				      (cons (car r); setq, setf, progn. expand everything after.
					  (mapcar #'(lambda(x)
						     (cmexpand `(with-temps ,x))
						     ;; (cmexpand x)
						      )
						  (cdr r))
	       
					  ))
				     
			     ;(t  r)
			     )))
      
      (setf expr (cmexpand expr))
      
      (ct1 expr)
      ;; (ct1 expr); count the temporaries
      (setf *names* (genlist *howmany*))
      (maketemps expr))))

(defun copy-octi(inf int)
  (dotimes (i 4 nil)(declare (optimize (speed 3)(safety 0))(fixnum i))
    (setf (aref int i)(aref inf i))))

(defun cmexpand(ex)  ;; compiler  macro and regular macro expand
 (if (consp ex)  
  (macroexpand    (let ((cmf (compiler-macro-function (car ex))))
		    (if cmf (funcall cmf ex nil)  ex)))
  ex))

    
); end of eval-when











#| working examples..

octi(17): (macroexpand '(with-temps (let ((x 3)(y 4)) (+ x y))))
(let ((x #(3.0d0 0.0d0 0.0d0 0.0d0)) (y #(4.0d0 0.0d0 0.0d0 0.0d0)))
  (add-qd-t x y #(1.0d0 0.0d0 0.0d0 0.0d0)))


 octi(20): (macroexpand '(dsetv  w (let ((x 3)(y 4)) (+ x y))))
(copy-octi (let ((x #(3.0d0 0.0d0 0.0d0 0.0d0))
                 (y #(4.0d0 0.0d0 0.0d0 0.0d0)))
             (add-qd-t x y #(1.0d0 0.0d0 0.0d0 0.0d0)))
           w)

octi(37): (cmexpand '(with-temps (setf (aref w 3) 2 q 25)))
(progn (let* ((#:g255099 w) (#:g255100 #(2.0d0 0.0d0 0.0d0 0.0d0)))
         (excl::.inv-s-aref #:g255100 #:g255099 3))
       (let* ((#:g255201 #(25.0d0 0.0d0 0.0d0 0.0d0)))
         (setq q #:g255201)))

octi(73):  (pprint (cmexpand '(with-temps (dotimes (i 100) (dsetv w (+ w 1))))))

(block nil
  (let ((i 0))
    (tagbody
      #:Tag75
        (if (>= i 100) (progn (return-from nil (progn))) nil)
        (tagbody (add-qd-t w #(1.0d0 0.0d0 0.0d0 0.0d0) w))
        (let () (setq i (+ i 1)) nil)
        (go #:Tag75))))


;;but you need to do this
;;(with-temps (dotimes (i 100) (dsetv w (+ w (into i)))))
;; to incorporate i into the loop body


[6] octi(69):  (pprint (cmexpand '(with-temps (let ((i 123))(dsetv x (+ x i))))))

(let ((i 123)) (add-qd-t x i x))
[6] octi(70):  (pprint (cmexpand '(with-temps (let ((i (with-temps 123)))(dsetv x (+ x i))))))

(let ((i #(123.0d0 0.0d0 0.0d0 0.0d0))) (add-qd-t x i x))


(pprint (cmexpand '(with-temps (do ((i 1 (1+ i)))((> i 10) 'foo) (dsetv w (+ w i))))));; not good, but see..

(pprint (cmexpand '(with-temps (do ((i (into 1)(with-temps(+ i 1)) ))((with-temps (> i 10)) 'foo) (dsetv w (+ w i))))))

(block nil
  (let ((i (into 1)))
    (tagbody
      #:Tag199
        (if (>-qd-t i #(10.0d0 0.0d0 0.0d0 0.0d0)
                    #(1.0d0 0.0d0 0.0d0 0.0d0))
            (progn (return-from nil (progn 'foo)))
          nil)
        (tagbody (add-qd-t w i w))
        (let ()
          (setq i
                (add-qd-t i #(1.0d0 0.0d0 0.0d0 0.0d0)
                          #(1.0d0 0.0d0 0.0d0 0.0d0)))
          nil)
        (go #:Tag199))))
;; buggy
;;(defun qfact(x)(with-temps(if (= x 1) 1 (* x (qfact (- x 1))))))
;; (qfact #q10)
;; compiles into bad program though. compiler messes up, expansioin is wrong
;;(defun fact(x)(if (= x 1) 1 (* x (fact(- x 1)))))
;;no
;;(defun qfact(x)(if (= x 1) 1 (with-temps(* x (qfact (- x 1))))

(defun qfact(x)(with-temps (if (= x 1) 1 (* x (qfact (- x 1))))))
;; no
;;(defun qfact(x)(with-temps (let ((p #q1)) (dotimes (i x)(dsetv p (* p (into i)))))))

|#


;; input and output.  

#|   if you run  (q-reads-octi),  then 
(setf w #q3.141592653589793238462643383279502884197169399375105820974944592q0)
 returns an array, namely 
#(3.141592653589793d0 1.2246467991473532d-16 -2.9947698097183397d-33
  1.1124542208633622d-49)
and (oct2string w)  returns the string
"0.314159265358979323846264338327950288419716939937510582097494459199Q1"

|#


  

(defun q-reads-octi()
  (set-dispatch-macro-character #\# #\q #'octi::octi-reader) )

(defun into(r &optional (where (%make-oct 0d0 0d0 0d0 0d0)))
  (lisp2oct r where))
(defmethod lisp2oct((x ratio) (ans array)) ;to encode a lisp rational, divide num/den
  (div-oct-t (into (numerator x))(into (denominator x)) ans))
(defmethod lisp2oct((x fixnum) (ans array))  ;small integers are easy.
  (lisp2oct (coerce x 'double-float) ans))
(defmethod lisp2oct ((x float) (ans array)) ;single-float or double-floats fit here
  ;;(format t "~% float array to lisp2oct ~s, ~s" x ans)
  (loop for i from 1 to 3 do 
	(setf (aref ans i) 0.0d0))
  (setf (aref ans 0) (coerce x 'double-float))
  ans)
(defmethod lisp2oct ((x integer) (ans array))
  ;; bignum integers that are shorter than a double-float fraction are easy.
  (if (< (cl::abs x) #.(cl::expt 2 53)) (lisp2oct  (coerce x 'double-float) ans)

    (let* ((p (coerce x 'double-float))
	   (ans (lisp2oct p ans)))
      (dotimes (i 3 ans)
	(setf p (coerce (decf x (cl::rational p)) 'double-float))
	(add-oct-d-t ans p ans)))))
(defmethod lisp2oct((x array)(ans array)) ;; assume it is an octi
  (copy-octi-into-octi x ans)
  ans)


(defun octi-reader(stream subchar arg)
  (declare (ignore subchar arg))
  (let ((s (read stream)))

    (string2octi (format nil "~s" s))))


(defun string2octi(s);; read integerQinteger like 10Q2   = 100 = 0.1Q3  or  -3.1q-1
  ;; examples of acceptable numbers [put " " around them]
  ;; .1q0 ; 1Q0; 1.2q3;  -1.23q-4;  - .23 q -4;  2.3q+4; +4.5q6; q0; --3Q1 same as 3q1
  ;; 3 ; 3.4; 123.45q+100; -.q0 ; .q0 ;
  ;; examples of not acceptable numbers:
  ;;    q ; .q;   123.45q+100  [=NaN(qd)]
  (let ((p0 0)(sign (into 1)))
    (setf s (delete #\  s));; remove leading or other spaces from string.
    (if (char= #\- (aref s 0))
	(progn (setf p0 1) (neg-qd-t sign sign)));; set sign if negative
    (multiple-value-bind (frac pos);; read fraction to left of .
	(parse-integer s :start p0 :radix 10 :junk-allowed t)
  ;;    (format t "~% frac= ~s pos=~s" frac pos)
      (if (null frac)(setf frac 0));; empty fraction is zero
      (if (= pos (length s)) (mul-qd sign (into frac))
	(case (aref s pos);; look at next char
	  ((#\Q #\q);; 10Q2
	   (multiple-value-bind (expon pos2)
	       (parse-integer s :start (1+ pos);skip the Q
			      :radix 10)
	;;     (format t "~% sign=~s frac=~s expon=~s" sign frac expon)
	     (mul-qd sign (into frac) (into (expt 10 expon)))))
	  (#\.;; 
	   (multiple-value-bind (frac2 pos2)
	       (parse-integer s :start (1+ pos);skip the "."
			      :radix 10 :junk-allowed t )
	     (if (null frac2)(setf frac2 0))
	     ;;(format t "~% sign=~s frac2=~s p0=~s s=~s" sign frac2 p0 s)
	     (setf frac
	       (add-qd (into frac)
		      (into (* (if (< frac 0) -1 1) frac2 (cl::expt 10 (cl::1+(cl::- pos pos2)))))))
	     ;;(format t "~% sign=~s frac2=~s frac=~s p0=~s s=~s" sign frac2 frac p0 s)
	     
	     (if (cl::= pos2 (length s))(mul-qd sign (into frac))
	       (case (aref s pos2)
		 ((#\Q #\q);; 10Q2
		  (multiple-value-bind (expon pos3)
		      (parse-integer s :start (1+ pos2);skip the Q
				     :radix 10 )
		    (mul-qd  frac (mul-qd sign (into (cl::expt 10 expon))))
		    ))
		 (otherwise 
		  (format t "next char is ~a -- Not an oct spec: ~s"  
			  (aref s pos2) s)
		  (into (or frac 0)))))))
	  )))))


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
