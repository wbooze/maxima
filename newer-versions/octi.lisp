;;; A quadruple-precison floating-point arithmetic package.

;;;  Based on Raymond Toy's conversion of Yozo Hida's QD.  See
;;;  rtoy-copyright.txt for copyright notice and permission.
;;;  Rewritten in places by Richard Fateman to make OCT work fast for
;;;  lisps that do not do aggressive in-lining as needed by RToy's
;;;  original version.

;;; This is part 1 of 4 files, octi.lisp, oct-const.lisp, ga.lisp,
;;; oct.lisp.  The octi package provides the the internal
;;; representation of a quad double: an array.  Typically one would
;;; use, on top of this octi representation, another layer of
;;; abstraction, provided in a separate coordinated package.  OCT is
;;; the object abstraction which overloads standard arithmetic so that
;;; it can use quad-doubles in conjunction with ordinary lisp, and
;;; which routinely prints oct numbers in a more-easily human-readable
;;; form.

;;; Actually you can read the first 16 decimal digits of an octi
;;; number pretty easily: just look at the first word.
;;; For example, converting  10000000200000040005 to oct looks like
;;;#(1.000000020000004d+15 0.0d0 0.0d0 0.0d0)

;;; regarding the other files:
;;; oct-const contains constants required for functions like cos-oct to work.
;;; ga.lisp implements a foundation for object-oriented generic arithmetic.
;;; oct.lisp uses ga to overload +, *, ..., sin, cos, ... to operate on
;;; objects whose interior is an octi array.

#| for example, using only octi, one can do this:

octi(100): (into 1)
#(1.0d0 0.0d0 0.0d0 0.0d0)
octi(101): (atan-oct *)
#(0.7853981633974483d0 3.061616997868383d-17 -7.486924524295849d-34
  2.7811355521584127d-50)
octi(102): (oct2string *)
"0.78539816339744830961566084581987572104929234984377645524373614807Q0"

Using oct, one can do this:
oct(131): (atan (into 1))
0.78539816339744827899949086713604629039764404296875Q0

or even

oct(134): (atan #q1.0Q0)  ;; syntax for reading quad doubles
0.78539816339744827899949086713604629039764404296875Q0

That number is approximately pi/4,


|#

;;; Other parts in the works include complex functions built upon this.

;;  There's other work that could be transferred to OCT from RJF's QD.lisp
;;; including quadrature, fft.

;;; 10/27/07 RJF
(defpackage :octi
  (:use  :cl)
    (:export into lisp2oct oct2lisp
	   add-oct-t add-oct-d-t
	   sub-oct-t sub-oct-d-t
	   mul-oct-t mul-oct-d-t
	   div-oct-t div-oct-d-t
	   sqr-oct-t sqr-oct-d-t	   
	   neg-oct-t neg-oct-d-t	   
	   abs-oct-t abs-oct-d-t	   
	   pow-oct-i pow-oct-i-t
	   1--oct-t
	   1+-oct-t))

(in-package :octi) 
(declaim (special sina cosa 
		  +oct-zero+ +oct-one+ +oct-eps+ +oct-e+ +oct-pi+
		  +oct-log2+ +oct-2pi+ +oct-pi/2+ +oct-pi/1024+ +oct-pi/4+  +oct-3pi/4+
		  +oct-1/rt2+ +oct-cos-table+ +oct-sin-table+))

;;; In this file, and OCT generally, we use this for a
;;; quad-double. This is the underlying representation.  Separately we
;;; cover this over in a CLOS object wrapper as an abstraction that
;;; also provides for using the common names like +, *, .... Actually
;;; we implement programs like defmethod two-arg-+ (x oct)(y oct) ...)
;;; on top of a generic arithmetic system that implements "+" by
;;; appropriate calls to two-arg-+.

  
(deftype %quad-double ()   '(simple-array double-float (4)))


(defmacro oct-0(x) `(aref ,x 0))
(defmacro oct-1(x) `(aref ,x 1))
(defmacro oct-2(x) `(aref ,x 2))
(defmacro oct-3(x) `(aref ,x 3))

(defmacro float-infinity-p(x)`(= (abs ,x) #.excl::*infinity-double*))
  
;;qts = quick-two-sum 

(eval-when (:execute :load-toplevel :compile-toplevel :execute)
 (defmacro %make-oct(a b c d)
  `(let ((r 
	  #-allegro
	  (make-array 4 :element-type 'double-float )
	  #+allegro 
	  (make-array 4 :element-type 'double-float 
		      :allocation :lispstatic-reclaimable)))
     (setf (aref r 0) ,a)
     (setf (aref r 1) ,b)
     (setf (aref r 2) ,c)
     (setf (aref r 3) ,d)
     r)))

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

#+ignore
(defmacro copy-octi-into-octi (from to)  
  `(let()    
     (dotimes (i 4 ,to)	
       (setf (aref ,to i)(aref ,from i)))))

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

;;(defmethod make-oct-d ((a float))  ;; make an object from the array
;;  (make-instance 'oct :value (%make-oct (coerce a 'double-float)
;;					   0d0 0d0 0d0)))



(defun time-qts (n)
  (declare (fixnum n)
       (optimize (speed 3) (safety 1) (debug 0)))
  (let ((a 1.0d0)
	(b .1d0)
	(q 0.0d0)(r 0.0d0))
    (declare (double-float a b q r))
    (dotimes (k n)
      (declare (fixnum k))
      (qtsmac q r a b))
    (+  q r)))

(defun time-mul (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((th (%make-oct 3d0 0d0 0d0 0d0))
	 (one (%make-oct 1d0 0d0 0d0 0d0))
	 (h (div-oct one th)))		; 1/3
   (print h)
    (time (dotimes (k n)
      (declare (fixnum k))
	    (mul-oct-t h h th)))))


(defun time-mul-t (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((th (%make-oct 3d0 0d0 0d0 0d0))
	 (one (%make-oct 1d0 0d0 0d0 0d0))
	 (h (div-oct one th)))		; 1/3
   (print h)
    (time (dotimes (k n)
      (declare (fixnum k))
	    (mul-oct-t h h th)))))


(defun time-sin-t (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((th (%make-oct 3d0 0d0 0d0 0d0))
	 (one (%make-oct 1d0 0d0 0d0 0d0))
	 (h (div-oct one th)))		; 1/3
;   (print h)
    (time (dotimes (k n)
      (declare (fixnum k))
	    (sin-oct-t h th)))))

#+ignore
;; same program, yozo's code

(defun time-mul (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let ((h (into-octi 1/3)))
    (time (dotimes (k n)
      (declare (fixnum k))
	    (* h h)))))

#+ignore
(defun time-mul-empty (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let ((h 0d0))
    (time (dotimes (k n)
      (declare (fixnum k))
					;(* h h)
	    nil
	    ))))

#+ignore
(defun time-mul2 (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let ((h (into-octi 1/3))
	(g (into-octi 0)))
    (time (dotimes (k n)
      (declare (fixnum k))
	  (dsetv g  (* h h))))))


;; splitmac works 10/1/07 RJF

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
  (lisp2oct r where))

;; The following methods REQUIRE a place to put the answer, and so
;; are less convenient than "into" to use directly.

(defmethod lisp2oct((x ratio) (ans array)) ;to encode a lisp rational, divide num/den
 (div-oct-t (octi::into (numerator x))(octi::into (denominator x)) ans))

(defmethod lisp2oct((x fixnum) (ans array))  ;small integers are easy.
  (lisp2oct (coerce x 'double-float) ans))

(defmethod lisp2oct ((x float) (ans array)) ;single-float or double-floats fit here
  ;;(format t "~% float array to lisp2oct ~s, ~s" x ans)
  (loop for i from 1 to 3 do 
	(setf (aref ans i) 0.0d0))
  (setf (aref ans 0) (coerce x 'double-float))
  ans)


;; this works for bignums, also rationals
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
(ss "hypot-oct" 2)(ss "expt-oct" 2)
(ss "invert-oct" 1)(ss "invert-oct-d" 1)
(ss "sqr-oct" 1) (ss "abs-oct" 1) (ss "neg-oct" 1)
(ss "1+-oct" 1) (ss "1--oct" 1)

(ss "sin-oct" 1) (ss "cos-oct" 1)(ss "tan-oct" 1) 
;;(ss "sinh-oct" 1) (ss "cosh-oct" 1)(ss "tanh-oct" 1)
(ss "asin-oct" 1) (ss "acos-oct" 1) (ss "atan-oct" 1 )
(ss "log-oct" 1) 
(ss "exp-oct" 1) (ss "sqrt-oct" 1)


;; In this group of programs, only sincos-oct returns 2 values here.

(defun sincos-oct(y)(let ((tars (%make-oct 0d0 0d0 0d0 0d0)) (tarc (%make-oct 0d0 0d0 0d0 0d0)))
		      (sincos-oct-t y tars tarc)
		      (values tars tarc)))

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

(defun pow-oct-i(a n)			; n is any integer
  (if (not (and(arrayp a)(integerp n)))
      (error "bad args to pow-oct-i ~s ~s" a n))
  (let ((ans (%make-oct 0d0 0d0 0d0 0d0)))
    (cond ((< n 0)(invert-oct-t (pow-oct-i-t a (- n) ans) ans
				))
	  (t (pow-oct-i-t a n ans)))
    ans))
	
;; compute power of oct to an integer, store in ans
(defun pow-oct-i-t(a n ans)			; n is positive integer
  ;; this is generalized in expt-oct.
  (cond((cl:zerop n) (setf (aref ans 0) 1d0) ans)
       ((cl:evenp n)(sqr-oct-t (pow-oct-i-t a (cl:ash n -1) ans) ans))
       (t (mul-oct-t a (pow-oct-i a (1- n)) ans))))

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

;; maybe we should keep this??
#+ignore
(defun div-oct (a b)
  (declare (type %quad-double a b)
	   (optimize (speed 3)
		     (space 0)
		     (safety 1)))
  (let ((a0 (aref a 0))
	(b0 (aref b 0)))
    (declare (double-float a0 b0))
    (let* ((q0 (cl:/ a0 b0))
	   (r (sub-oct a (mul-oct-d b q0)))
	   (q1 (cl:/ (aref r 0) b0)))
     
      (if (float-infinity-p q0)
	(return-from div-oct (%make-oct q0 0d0 0d0 0d0)))
      
      (setf r (sub-oct r (mul-oct-d b q1)))
      (let ((q2 (cl:/ (aref r 0) b0)))
	(setf r (sub-oct r (mul-oct-d b q2)))
	(let ((q3 (cl:/ (aref r 0) b0)))
	  (renorm-4mac q0 q1 q2 q3 q0 q1 q2 q3)
	  (%make-oct q0 q1 q2 q3))))))


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


;;
;;Sine and Cosine. Used after reduction so |a|<pi/2048.
;; copied from Ray Toy's code.

(defun sincos-taylor(a)
  (let ((s (into 0))
	(c (into 0)))
    (sincos-taylor-t a s c)
    (values s c)))

(defun sincos-taylor-t (a s-ans c-ans)
  (declare (type %quad-double a))
  (let ((thresh (cl:* +oct-eps+ (abs (oct-0 a)))))
    (when (zerop-oct a)
      (copy-octi-into-octi +oct-zero+ s-ans)
      (copy-octi-into-octi +oct-one+ c-ans)
      (return-from sincos-taylor-t s-ans))
    (let* ((x (neg-oct (sqr-oct a)))
	   (s a)
	   (p a)
	   (m 1d0))
      (loop
	 (setf p (mul-oct p x))
	 (incf m 2)
	 (setf p (div-oct-d p (cl:* m (cl:1- m))))
	 (setf s (add-oct s p))
	 ;;(format t "p = ~A~%" (oct-0 p))
	 (when (<= (abs (oct-0 p)) thresh)
	   (return)))
      ;; cos(c) = sqrt(1-sin(c)^2).  This works ok, since c is small.
      (copy-octi-into-octi s s-ans)
      (sqrt-oct-t (add-oct-d (neg-oct (sqr-oct s)) 1d0) c-ans)
      s-ans)))

;;; end of octi arithmetic package

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

;; how about complex octs?

#| Here is the list of CL stuff that might make sense, defined in this package purely for octi real data: array of 4 doubles.

* / + - = /= < > <=  >=   ;; done as two-arg-*   etc

;; partly done in REAL
;; see RToy's code for most of these.

expt done for integer exponent as pow
... maybe faster ffloor etc.

;; not yet done
minusp plusp 

ftruncate fround

mod rem 
signum random numberp cis
complex complexp conjugate phase realpart imagpart realp 
numerator denominator rational rationalize 
decode-float 

See file oct-const for constants

|#



;; a version of exp function based on oct  from Ray Toy
;;  exp-qd/reduce
(defun exp-oct-t (a target)
  ;; Strategy:  Reduce the size of x by noting that
  ;;
  ;; exp(k*r+m) = exp(m) * exp(r)^k
  ;;
  ;; Thus, by choosing m to be a multiple of log(2) closest to x, we
  ;; can make |kr| < log(2)/2 = 0.3466.  Now we can set k = 256, so
  ;; that |r| <= 0.00136.
  ;;
  ;; Then
  ;;
  ;; exp(x) = exp(k*r+s*log(2)) = 2^s*(exp(r))^256
  ;;
  ;; We can use Taylor series to evaluate exp(r).

  (let* ((k 256)
	 (z (truncate (oct-0 (nint-oct (div-oct a +oct-log2+)))))
	 (r (div-oct-d (sub-oct a (mul-oct-d +oct-log2+ (float z 1d0)))
		      (float k 1d0)))
	 ;; For Taylor series.  p = r^2/2, the first term
	 (p (div-oct-d (sqr-oct r) 2d0))
	 ;; s = 1+r+p, the sum of the first 3 terms
	 (s (add-oct-d (add-oct r p) 1d0))
	 ;; Denominator of term
	 (m 2d0))
    ;; Taylor series until the term is small enough.
    ;;
    ;; Note that exp(x) = sinh(x) + sqrt(1+sinh(x)^2).  The Taylor
    ;; series for sinh has half as many terms as for exp, so it should
    ;; be less work to compute sinh.  Then a few additional operations
    ;; and a square root gives us exp.
    (loop
      (incf m)
      (mul-oct-t p r p)			;setf p
      (div-oct-d-t p m p)		;setf p
      (add-oct-t s p s)			;setf s
      (unless (> (abs (oct-0 p)) +oct-eps+)
	 (return)))
   (scale-float-oct-t (pow-oct-i-t s k r) z target) ))

;;exp(1)
;;2.718281828459045235360287471352662497757247093699959574966967627724077b0


;; EVALUATING a POLYNOMIAL

;; Octlist is an ordinary lisp list of the coefficients in a
;; polynomial. The last item in the list is the constant
;; coefficient. All coeffs are octi objects.  

;; (setf testpoly (list (into 5) (into 3) (into 1))) ; 5*x^2+3*x+1 
;; (setf r0 (polyeval-oct testpoly (into 1)); returns 9
;; 

(defun polyeval-oct (octlist x)
  (let ((sum (into 0))
	(tmp #.(%make-oct 0d0 0d0 0d0 0d0)))
	(dolist (i octlist sum)
	  (add-oct-t (mul-oct-t x sum tmp) i sum))))

;; similar but dlist is a list of DOUBLE FLOATS.
;; evaluate the polynomial to a quad-double

(defun polyeval-oct-d (dlist x)
  (let ((sum (into 0)))
	(polyeval-oct-d-t dlist x sum)))

(defun polyeval-oct-d-t (dlist x sum) ;; sum is the target
  (let ((sum (copy-octi-into-octi +oct-zero+ sum))
	(tmp #.(%make-oct 0d0 0d0 0d0 0d0)))
	(dolist (i dlist sum)
	  (add-oct-d-t  (mul-oct-t x sum tmp) i sum))))

;;; an example to show that oct numbers can have very high pseudo precision..
;;;  (setf testpoly2 (list (into 5) (into 3) (into 0))) ; 5*x^2+3*x  
;;;  (setf r1  (polyeval-oct testpoly (into (expt 2 510))))
;;; #(5.617791046444737d+307 1.0055855947456948d+154 1.0d0 0.0d0)
;;;  (setf r2  (polyeval-oct testpoly2 (into (expt 2 510))))
;;; #(5.617791046444737d+307 1.0055855947456948d+154 0.0d0 0.0d0)
;;;  (sub-oct r1 r2) --> 1

;;;  we can evaluate exp(x) for  |x|< 0.01, say, by
;;; a pade approximation, computed using Maxima.

;;[(x^10+110*x^9+5940*x^8+205920*x^7+5045040*x^6+90810720*x^5+1210809600*x^4+11762150400*x^3+79394515200*x^2+335221286400*x+670442572800)/(x^10-110*x^9+5940*x^8-205920*x^7+5045040*x^6-90810720*x^5+1210809600*x^4-11762150400*x^3+79394515200*x^2-335221286400*x+670442572800)]

;; Notice that the numerator and denominator have the same
;; coefficients with different signs on odd terms, so they can be
;; evaluated as f(x^2)+x*g(x^2) and f(x^2)-x*g(x^2) respectively using
;; about half the number of multiplies.

(defconstant f-exp-pade  '(1.0d0 5940.0d0 5045040.0d0  1.2108096d+9
			   7.93945152d+10 6.704425728d+11))

(defconstant g-exp-pade  '(110.0d0  205920.0d0  9.081072d+7 
			   1.17621504d+10  3.352212864d+11 ))

#+ignore  ;; an alternative from rjf. Not quite so accurate./
(defun exp-oct-small(x) ;; re-use temps  exp(x) for |x|<0.01, to quad-double  
  (let* ((x2 (sqr-oct x))
	 (fx2 (polyeval-oct-d f-exp-pade x2))
	 (xgx2 (mul-oct x (polyeval-oct-d g-exp-pade x2))))
    ;; 12 oct*oct, 10 oct+double  so far. 

    ;;(format t "~%f=~s g=~s"  (add-oct-t fx2 xgx2 x2)(sub-oct-t fx2 xgx2 xgx2))
    ;; next line takes 2 oct adds and a divide
		(div-oct-t (add-oct-t fx2 xgx2 x2)(sub-oct-t fx2 xgx2 xgx2) fx2)
		))

#+ignore
(defun exp-oct-t(a target)
  (if (< (oct-0 a) 0d0)(invert-oct-t (exp-oct-t (neg-oct-t a target) target) target)
    
    (multiple-value-bind (m n)
	(divrempos-oct a +oct-log2+)

      ;; now a= m*log(2)+n.  compute 2^m* exp(n/256)^256.
 ;;     (format t "~%doing exp ~s, m=~s, n=~s" a m n)
      (mul-oct-t (pow-oct-i (into 2) (truncate (oct-0 m)));2^m
		 (pow-oct-i  (exp-oct-small 
				   (div-oct-d n  256.0d0))
			    256)
		 target))));;  (e^(n/256))^256

#|
;; more coefficients --> not more accurate??

;; pade approx
;; x^12+156*x^11+12012*x^10+600600*x^9+21621600*x^8+588107520*x^7+12350257920*x^6+201132771840*x^5+2514159648000*x^4+23465490048000*x^3+154872234316800*x^2+647647525324800*x+1295295050649600



(defconstant f-exp-pade2		; coefs of x^12, x^10, ..., x^0
    '(1d0 12012d0 21621600d0 12350257920d0 2514159648000d0 154872234316800d0 1295295050649600d0))
(defconstant g-exp-pade2
    '(156d0 600600d0 588107520d0 201132771840d0 23465490048000d0  647647525324800d0))

;; no better than exp-oct-small, I think.
(defun eos(x) ;; re-use temps  exp(x) for |x|<0.01, to quad-double
  (let* ((x2 (sqr-oct x))
	 (fx2 (polyeval-oct-d f-exp-pade2 x2))
	 (xgx2 (mul-oct x (polyeval-oct-d g-exp-pade2 x2))))
    ;; 12 oct*oct, 10 oct+double  so far. 
    ;; next line takes 2 oct adds and a divide
    (div-oct-t (add-oct-t fx2 xgx2 x2)(sub-oct-t fx2 xgx2 xgx2) x2)))

(defun exp-oct2(a)
  (multiple-value-bind (m n)
      (ftruncate-oct a +oct-log2+)
    ;; now a= m*log(2)+n.  compute 2^m* exp(n/256)^256.
    (mul-oct (pow-oct-i (into 2) (truncate (oct-0 m)));2^m
	     (pow-oct-i (into (eos
			       (div-oct-d n  256.0d0)))
			256))))  

|#

;; my attempts to understand sin and cos.

#| To compute sin(x), we want to reduce the range of x, which could be any
representable number.

For example we find some y, 0<= y < 2*pi such that x=y+2*pi*N for some
integer N.  This process of range reduction may require that we know
pi to very many digits, at least if we are to be accurate.
After finding y, the method used in the qd library is to
decomposing y = a*pi/2 +b*pi/1024 + s, with a, b, non-negative integers,
and s some number less than pi/1024, or about 0.0030679616
Actually, a can be 0,1,2,3.  b can be 0 to 1023

let ss[b]:= sin(b*pi/1024), an entry in a table. Similarly, cc[b].
sin(y) is then
.. for a=0,  cc[b]*sin(s)+ss[b]*cos(s)
.. for a=1, -ss[b]*sin(s)+cc[b]*cos(s)
.. for a=2, -cc[b]*sin(s)-ss[b]*cos(s)
.. for a=3,  ss[b]*sin(s)-cc[b]*cos(s)
.. for a=4, same as a=0 etc

so all we need to do is pre-compute tables of sines and cosines, and
compute the approximation (by taylor series or other means) for sin(s)
and cos(s) where s is guaranteed to be less than about 0.003068. The
largest error would be at s=0.003068, and if we run the Taylor series
to 17 terms, the first neglected term x^19/19! would be at most
1.463e-65 . This is a little close, since sin(pi/1024) is about
0.0030, so an error of 1.4e-65 would show up. So we can arrange some
room for slosh, by keeping x^19 and leaving off the x^21 term, at
3.277e-73. At least I think this will work.

We have 2 arrays, sina cosa, corresponding to ss and cc in the formulas above.

;; Regarding Range Reductions
;; Kind of arbitrary cutoff to do exact range reduction.
;; we replace y by y-2*n*pi where pi is known to huge precision.
;; This is important if y=, say, 6283185307179586476925286766559006, which
;; is very close 10^33 times 2pi. If we compute y-10^33*pi, we have about
;; 31 decimal digits in the result, and 33 unknown because we used only 64
;; decimal digits (actually, 212 bits) of pi.  This way we use an extravagant
;; number of digits of pi to make sure that doesn't happen.
;; We don't do it often, so we hope efficiency is not an issue.
;; It's nice we can do exact integer stuff relatively simply in lisp.


|#
    
(defvar two-pi-many-bits ;; at least 1236 bits, as a rational number.
     22884743329868046105125534265462446198541589729467624782075250820510692843229684905276031268214145918457914724288873462827859618300605284995751611318802551321814951688142116429613752561980/3642220022337780247665573591773167826362669080317417579448364704693110029206203569295741890258185811272321756922288545062428726354857482731853161900688354006071607571978699476172264403277)
      
(defun careful-range-reduction(y)
    (multiple-value-bind (n m)		; y=n*2pi+m
	(truncate (oct2lisp y) two-pi-many-bits)
      (declare (ignore n))
    (into m)))

;;(defun sincos(a b c) ;; stub for sin/cos of small arg
;;		(copy-octi-into-octi (into (sin (oct-0 a))) b)
;;		(copy-octi-into-octi (into (cos (oct-0 a))) c))


;;(defun test-s (x)(list (oct-0(sin-oct (into x))) (sin x)))
;;(defun test-c (x)(list (oct-0(cos-oct (into x))) (cos x)))




#| 38999999999999384 * pi is very nearly the integer
k=122522113490000000, so range reduction tends to make the computation
of cos (122522113490000000) rather touchy.  Using Mathematica to
compute N[Cos[k]] as a double-float, and N[Cos[k],16] gives two
numbers
0.47237690434970653   (use InputForm[] to see the digits)
0.4722378698148295

The cosines agree to only 3 decimal digits.
This first value is not unique to Mathematica, but happens in 2 other systems
using ordinary floating point arithmetic. We tried (Maxima in GCL, Allegro CL, both
on Intel Pentium). 

The right answer? As computed by oct (rjf) and oct (rtoy) and then Mathematica (t0 70 places) and
then in Maxima (to 70 places), and then in Yozo's QD...
  0.472237869814829536825748604378294945886073849481652855352802025677Q0  oct 
#q0.47223786981482953682574860437829494588607384948165285535280202569q0   oct from rtoy; good
 "0.472237869814829536825748604378294945886073849481652855352802025777Q0" oct rjf; 10/26/07
  0.4722378698148295368257486043782949458860738494816528553528020257046972 mathematica 6.0
   4.722378698148295368257486043782949458860738494816528553528020257046972b-1 maxima
  0.472237869814829536825748604378294945886073849482247612284170520164Q0   qd yozo

Note that QD includes about 19 digits of noise. The last few digits printed of 
OCT and QD are something of a stretch. 63.8 decimal digits are representable in 212 bits;
The last few digits are not correct as decimals: OCT is really filling 
in the closest decimal digits to get closest to the last few bits.
 
|#

;; more code from Ray Toy
(defun expm1-oct (a)  
  "exp(a) - 1, done accurately"
  (declare (type %quad-double a))
  (when (float-infinity-p (oct-0 a))
    (return-from expm1-oct
      (if (minusp (float-sign (oct-0 a)))
	  +oct-zero+
	  a)))
  (expm1-oct/duplication a))


(defun expm1-oct/duplication (a)
  (declare (type %quad-double a))
  ;; Brent gives expm1(2*x) = expm1(x)*(2+expm1(x))
  ;;
  ;; Hence
  ;;
  ;; expm1(x) = expm1(x/2)*(2+expm1(x/2))
  ;;
  ;; Keep applying this formula until x is small enough.  Then use
  ;; Taylor series to compute expm1(x).
  (cond ((< (abs (oct-0 a)) .0001d0)
	 ;; What is the right threshold?
	 ;;
	 ;; Taylor series for exp(x)-1
	 ;; = x+x^2/2!+x^3/3!+x^4/4!+...
	 ;; = x*(1+x/2!+x^2/3!+x^3/4!+...)
	 (let ((sum +oct-one+)
	       (term +oct-one+))
	   (dotimes (k 28)
	     (setf term (div-oct-d (mul-oct term a) (float (cl:+ k 2) 1d0)))
	     (setf sum (add-oct sum term)))
	   (mul-oct a sum)))
	(t
	 (let ((d (expm1-oct/duplication (scale-float-oct a -1))))
	   (mul-oct d (add-oct-d d 2d0))))))

(defun sinh-oct (a)
  "Sinh(a)"
  (declare (type %quad-double a))
  ;; Hart et al. suggests sinh(x) = 1/2*(D(x) + D(x)/(D(x)+1))
  ;; where D(x) = exp(x) - 1.  This helps for x near 0.
  (cond ((zerop-oct a)
	 a)
	((float-infinity-p (oct-0 a))
	 a)
	(t
	 (let ((d (expm1-oct a)))
	   (when (float-infinity-p (oct-0 d))
	     (return-from sinh-oct d))
	   (scale-float-oct (add-oct d
				   (div-oct d (add-oct-d d 1d0)))
			   -1)))))


(defun cosh-oct (a)
  "Cosh(a)"
  (declare (type %quad-double a))
  ;; cosh(x) = 1/2*(exp(x)+exp(-x))
  (let ((e (exp-oct a)))
    (when (float-infinity-p (oct-0 e))
      (return-from cosh-oct e))
    (scale-float-oct (add-oct e (div-oct +oct-one+ e))
		    -1)))


(defun tanh-oct (a)
  "Tanh(a)"
  (declare (type %quad-double a))
  ;; Hart et al. suggests tanh(x) = D(2*x)/(2+D(2*x))
  (cond ((zerop-oct a)
	 a)
	((> (abs (oct-0 a)) (/ (+ (log most-positive-double-float)
				 (log 2d0))
			      4d0))
	 ;; For this range of A, we know the answer is +/- 1.
	 ;;
	 ;; However, we could do better if we wanted.  Assume x > 0
	 ;; and very large.
	 ;;
	 ;; tanh(x) = sinh(x)/cosh(x)
	 ;;         = (1-exp(-2*x))/(1+exp(-2*x))
	 ;;         = 1 - 2*exp(-2*x)/(1+exp(-2*x))
	 ;;
	 ;; So tanh(x) is 1 if the other term is small enough, say,
	 ;; eps.  So for x large enough we can compute tanh(x) very
	 ;; accurately, thanks to how quad-double addition works.
	 ;; (The first component is, basically 1d0, and the second is
	 ;; some very small double-float.)
	 #+ignore
	 (let* ((e (exp (* -2 a)))
		(res (- 1 (/ (* 2 e) (1+ e)))))
	   (if (minusp (float-sign (oct-0 a)))
	       (neg-oct res)
	       res))
	 (into (float-sign (oct-0 a))))
	(t
	 (let* ((a2 (mul-oct-d a 2d0))
		(d (expm1-oct a2)))
	   (div-oct d (add-oct-d d 2d0))))))

(defun asinh-oct (a)
  "Asinh(a)"
  (declare (type %quad-double a))
  ;; asinh(x) = log(x + sqrt(1+x^2))
  ;;
  ;; But this doesn't work well when x is small.
  ;;
  ;; log(x + sqrt(1+x^2)) = log(sqrt(1+x^2)*(1+x/sqrt(1+x^2)))
  ;;   = log(sqrt(1+x^2)) + log(1+x/sqrt(1+x^2))
  ;;   = 1/2*log(1+x^2) + log(1+x/sqrt(1+x^2))
  ;;
  ;; However that doesn't work well when x is large because x^2
  ;; overflows.
  ;;
  ;; log(x + sqrt(1+x^2)) = log(x + x*sqrt(1+1/x^2))
  ;;   = log(x) + log(1+sqrt(1+1/x^2))
  ;;   = log(x) + log1p(sqrt(1+1/x^2))
  #+ignore
  (log-oct (add-oct a
		  (sqrt-oct (add-oct-d (sqr-oct a)
				     1d0))))
  (cond ((< (abs (oct-0 a)) (sqrt most-positive-double-float))
	 (let ((a^2 (sqr-oct a)))
	   (add-oct (scale-float-oct (log1p-oct a^2) -1)
		   (log1p-oct (div-oct a
				     (sqrt-oct (add-oct-d a^2 1d0)))))))
	((float-infinity-p (oct-0 a))
	 a)
	(t
	 (if (minusp (oct-0 a))
	     (neg-oct (asinh-oct (neg-oct a)))
	   (let ((1/a (div-oct +oct-one+
			       a)))
	       (+ (log-oct a)
		  (log1p-oct (sqrt-oct (add-oct-d (sqr-oct 1/a) 1d0)))))))))


(defun acosh-oct (a)
  "Acosh(a)"
  (declare (type %quad-double a))
  ;; acosh(x) = log(x + sqrt(x^2-1))

  ;; log(x+sqrt(x^2-1)) = log(x+sqrt((x-1)*(x+1)))
  ;;  = log(x+sqrt(x-1)*sqrt(x+1))

  ;; Let x = 1 + y
  ;; log(1 + y + sqrt(y)*sqrt(y + 2))
  ;;   = log1p(y + sqrt(y)*sqrt(y + 2))
  ;;
  ;; However, that doesn't work well if x is large.
  ;;
  ;; log(x+sqrt(x^2-1)) = log(x+x*sqrt(1-1/x^2))
  ;;   = log(x) + log(1+sqrt(1-1/x^2))
  ;;   = log(x) + log1p(sqrt(1-1/x)*sqrt(1+1/x))
  ;;
  (cond ((< (abs (oct-0 a)) (sqrt most-positive-double-float))
	 (let ((y (sub-oct-d a 1d0)))
	   (log1p-oct (add-oct y (sqrt-oct (mul-oct y (add-oct-d y 2d0)))))))
	((float-infinity-p (oct-0 a))
	 a)
	(t
	 (let ((1/a (div-oct +oct-one+ a)))
	   (+ (log-oct a)
	      (log1p-oct (mul-oct (sqrt-oct (sub-oct +oct-one+ 1/a ))
				(sqrt-oct (add-oct-d  1/a 1d0)))))))))

(defun atanh-oct (a)
  "Atanh(a)"
  (declare (type %quad-double a))
  ;; atanh(x) = 1/2*log((1+x)/(1-x))
  ;;          = 1/2*log(1+(2*x)/(1-x))
  ;; This latter expression works better for small x
  #+nil
  (scale-float-oct (log-oct (div-oct (add-oct-d 1d0 a 1d0)
				  (sub-oct +oct-one+ a)))
		  -1)
  ;; atanh(+/-1) = +/- infinity.  Signal a division by zero or return
  ;; infinity if the division-by-zero trap is disabled.
  (if (oct-= (abs-oct a) +oct-one+)
      (div-oct (into (float-sign (oct-0 a)))
	      +oct-zero+)
      (scale-float-oct (log1p-oct (div-oct (scale-float-oct a 1)
					(sub-oct +oct-one+ a)))
		       -1)))


(defun log1p-oct/duplication (x)
  (declare (type %quad-double x)
	   (optimize (speed 3)))
  ;; Brent gives the following duplication formula for log1p(x) =
  ;; log(1+x):
  ;;
  ;; log1p(x) = 2*log1p(x/(1+sqrt(1+x)))
  ;;
  ;; So we apply the duplication formula until x is small enough, and
  ;; then use the series
  ;;
  ;; log(1+x) = 2*sum((x/(2+x))^(2*k+1)/(2*k+1),k,0,inf)
  ;;
  ;; Currently "small enough" means x < 0.005.  What is the right
  ;; cutoff?
  (cond ((> (abs (oct-0 x)) .005d0)
	 ;; log1p(x) = 2*log1p(x/(1+sqrt(1+x)))
	 (mul-oct-d (log1p-oct/duplication
		    (div-oct x
			    (add-oct-d
				      (sqrt-oct (add-oct-d  x 1d0)) 1d0)))
		   2d0))
	(t
	 ;; Use the series
	 (let* ((term (div-oct x (add-oct-d x 2d0)))
		(mult (sqr-oct term))
		(sum term))
	   (loop for k of-type double-float from 3d0 by 2d0
	      while (> (abs (oct-0 term)) +oct-eps+)
	      do
		(setf term (mul-oct term mult))
		(setf sum (add-oct sum (div-oct-d term k))))
	   (mul-oct-d sum 2d0)))))


(defun log1p-oct (x)
  "log1p(x) = log(1+x), done more accurately than just evaluating
  log(1+x)"
  (declare (type %quad-double x))
  #+cmu
  (when (float-infinity-p (oct-0 x))
    x)
  (log1p-oct/duplication x))

;; either (log array base array)  or (log array array). base is a cl real.

;; how to deal with log(arg base target)

;; vs               log(arg base)
;; vs               log(arg target)


#+ignore
(defun log-oct-t (x arg1 &optional (arg2  0 arg2p))
  ;; if arg2p, there are 3 args, log(x,base,target)
  ;; otherwise there are 2 args, log(x, target)
  (cond (arg2p   (log-oct-t-aux arg1 arg2)
		 arg2)
	(t (setf (oct-0 arg1)(cl:log (oct-0 x)))
	   arg1)))

(defun log-oct-t(a ans) ;; log base e.
  "Log(a)"
  (declare (type %quad-double a))
  (cond ((oct-=  a +oct-one+)
	 (copy-octi-into-octi +oct-zero+ ans))
	((zerop-oct a) (error "log-oct of zero"))
	((minusp (float-sign (oct-0 a)))
	 (error "log of negative"))
	(t
	 ;; Default is Halley's method
	 (copy-octi-into-octi (log-oct/halley a) ans))))

(defun log-oct/halley (a)
  (declare (type %quad-double a))
  ;; Halley iteration:
  ;;
  ;; x' = x - 2*(exp(x)-a)/(exp(x)+a)
  ;;
  ;; But the above has problems if a is near
  ;; most-positive-double-float.  Rearrange the computation:
  ;;
  ;; x' = x - 2*(exp(x)/a-1)/(exp(x)/a+1)
  ;;
  ;; I think this works better, but it's also probably a little bit
  ;; more expensive because each iteration has two divisions.
  (let ((x (into (log (oct-0 a)))))
    (flet ((iter (est)
	     (let ((exp (div-oct (exp-oct est)
				a)))
	       (sub-oct est
		       (scale-float-oct
			(div-oct (sub-oct-d exp 1d0)
				(add-oct-d exp 1d0))
			1)))))
      ;; Two iterations should be enough
      (setf x (iter x))
      (setf x (iter x))
     ; (setf x (iter x));; a third..
      x)))

;; encoding floats as integers, to make sure we get exactly the same
;; constants for quad-doubles
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

  ;; To compute sin(x), choose integers a, b so that
  ;;
  ;; x = s + a * (pi/2) + b*(pi/1024)
  ;;
  ;; with |x| <= pi/2048.  Using a precomputed table of sin(k*pi/1024)
  ;; and cos(k*pi/1024), we can compute sin(x) from sin(s) and cos(s).
  ;;
  ;; sin(x) = sin(s+k*(pi/1024) + j*pi/2)
  ;;        = sin(s+k*(pi/1024))*cos(j*pi/2)
  ;;            + cos(s+k*(pi/1024))*sin(j*pi/2)
  ;;
  ;; sin(s+k*pi/1024) = sin(s)*cos(k*pi/1024)
  ;;                     + cos(s)*sin(k*pi/1024)
  ;;
  ;; cos(s+k*pi/1024) = cos(s)*cos(k*pi/1024)
  ;;                     - sin(s)*sin(k*pi/1024)

;;sin(s+b*%pi/1024+a*%pi/4) ;;; not /2


;; test for cos: k = 
;;122522113490000000

(defun sin-oct-t(y tar &aux amod bmod) 
  (when (< (oct-0 y) 0)			;sin(-x)=-sin(x)
    (return-from sin-oct-t (neg-oct-t(sin-oct-t(neg-oct y) tar)tar)))
  (when (> (oct-0 y) 1000d0)
    (setf y (careful-range-reduction y)))
  (multiple-value-bind (a rest)
      ;;(ftruncate-oct y +oct-pi/4+)
      (divrempos-oct y +oct-pi/4+)
      
    (setf amod (mod (truncate (oct-0 a)) 8))
    (multiple-value-bind(b s)
	;;(ftruncate-oct rest +oct-pi/1024+)
	(divrempos-oct rest +oct-pi/1024+)
      (setf bmod (truncate (oct-0 b)))
      ;;    	       (format t "~% a=~s b=~s s=~s  " a b (oct2string s))
      (if (> amod 3)(neg-oct-t (sin-oct-aux-t (- amod 4) bmod s a b tar) tar)
	(sin-oct-aux-t amod bmod s a b tar)))))

(defun sin-oct-aux-t (a b s sins coss tar)
  (let ((ss (aref sina b))
	(cc (aref cosa b)))
    (sincos-taylor-t s  sins coss)  
    (case a 
      (0 (add-oct-t (mul-oct-t cc sins sins)(mul-oct-t ss coss coss) tar))
      (1 (mul-oct-t +oct-1/rt2+
		    (add-oct-t
		     (mul-oct-t coss (add-oct-t cc ss tar) coss)
		     (mul-oct-t sins (sub-oct-t cc ss tar) sins) tar) tar))
      (2 (sub-oct-t (mul-oct-t cc coss coss)(mul-oct-t ss sins sins) tar))
      (3 (mul-oct-t +oct-1/rt2+
		    (sub-oct-t
		     (mul-oct-t coss (sub-oct-t cc ss tar) coss)
		     (mul-oct-t sins (add-oct-t cc ss tar) sins)
		     tar)tar)))))

(defun cos-oct-t(y tar &aux amod bmod) ; target gets cos
  (when(< (oct-0 y) 0)  (neg-oct-t y y));;cos(-x)=cos(x)
  ;; now y is positive
  (when (> (oct-0 y) 1000d0)
    ;; See comments  re range reduction.
    (setf y (careful-range-reduction y)))
  (multiple-value-bind (a rest)
      ;; (ftruncate-oct y +oct-pi/4+)
       (divrempos-oct y +oct-pi/4+)
    (setf amod (mod (truncate (oct-0 a)) 8))
    (multiple-value-bind(b s)
	;;(ftruncate-oct rest +oct-pi/1024+)
	(divrempos-oct rest +oct-pi/1024+)
            (setf bmod (truncate (oct-0 b)))
	;; these are the cos cases
	(if (> amod 3)(neg-oct-t (cos-oct-aux-t (- amod 4) bmod s a b tar) tar)
	  (cos-oct-aux-t amod bmod s  a b tar)))))

(defun cos-oct-aux-t (a b s sins coss tar) ; use a,b as temps
  (let ((ss (aref sina b))
	(cc (aref cosa b)))
    (sincos-taylor-t s sins coss) ; put results in temps
    (case a 
      (0 (sub-oct-t (mul-oct-t cc coss coss)(mul-oct-t ss sins sins) tar));
      (1 (mul-oct-t +oct-1/rt2+
		  (sub-oct-t 
		   (mul-oct-t coss
			    (sub-oct-t cc ss tar) coss)
		   (mul-oct-t sins
			    (add-oct-t cc ss tar) sins) tar) tar));
      (2 (neg-oct-t(add-oct-t (mul-oct ss coss)(mul-oct cc sins) tar) tar));
      (3 (mul-oct-t +oct-1/rt2+
		  (sub-oct-t (mul-oct-t sins
				    (sub-oct-t ss cc tar) sins) 
			   (mul-oct-t coss
				      (add-oct-t cc ss tar) coss)
			   tar  ) tar)))))


(defun sincos-oct-t(y tars tarc &aux amod bmod) ; tars gets sin, tarc gets cos
  
  (let ((signs 1)) ;; sign of sine
    
  (when(< (oct-0 y) 0)  (neg-oct-t y y) (setf signs -1));;cos(-x)=cos(x); sin(-x)=-sin(x)
  ;; now y is positive
  (when (> (oct-0 y) 1000d0)
    ;; See comments  re range reduction.
    (setf y (careful-range-reduction y)))
  (multiple-value-bind (a rest)
      ;; (ftruncate-oct y +oct-pi/4+)
       (divrempos-oct y +oct-pi/4+)
    (setf amod (mod (truncate (oct-0 a)) 8))
    (multiple-value-bind(b s)
	;;(ftruncate-oct rest +oct-pi/1024+)
	(divrempos-oct rest +oct-pi/1024+)
            (setf bmod (truncate (oct-0 b)))
	;; these are the  cases
	    (cond ((> amod 3)
		   (neg-oct-t (cos-oct-aux-t (- amod 4) bmod s a b tarc) tarc)
		   (if (= signs -1 )
		        (sin-oct-aux-t (- amod 4) bmod s a b tars)
		     (neg-oct-t (sin-oct-aux-t (- amod 4) bmod s a b tars) tars)))
		  (t 
			 (cos-oct-aux-t amod bmod s  a b tarc)
			 (if (= signs -1)
			     (neg-oct-t (sin-oct-aux-t amod bmod s a b tars) tars)
			   (sin-oct-aux-t amod bmod s a b tars)
			   )))))))

(defun tan-oct-t(y targ)(let ((tars (into 0)) (tarc (into 0)))
		      (sincos-oct-t y tars tarc)
		      (div-oct-t tars tarc targ)))

(defun atan-oct-t (y arg1 &optional (arg2 0 arg2p)) 
  ;; if arg2p,` there are 3 args, atan(y,x,target) -- atan2
  ;; otherwise there are 2 args, atan(y, target)
  (cond (arg2p   ;; there are 3 args so arg2 is target
	 (atan2-oct/newton-t y arg1 arg2))
	(t (atan2-oct/newton-t y +oct-one+ arg1))))

(defun hypot-oct-t(x y tar &aux (zz #.(%make-oct 0d0 0d0 0d0 0d0)))
  (cond ((>= (abs (oct-0 x))(abs (oct-0 y)))
	 (div-oct-t y x tar)
	 (abs-oct-t (mul-oct-t 
		     (sqrt-oct-t (add-oct-d-t (sqr-oct-t tar tar) 1d0 tar)zz) x tar)
		    tar))
	(t (hypot-oct-t y x tar))))

(defun atan2-oct/newton-t (y x tar)
  (declare (type %quad-double y x)
		   (optimize (speed 3) (safety 1)(space 0)))
  ;; Instead of using Taylor series to compute atan, we instead use
  ;; Newton's iteration to solve the equation
  ;;
  ;;   sin(z) = y/r or cos(z) = x/r
  ;;
  ;; where r = sqrt(x^2+y^2)
  ;;
  ;; The iteration is
  ;;
  ;;   z' = z + (y - sin(z))/cos(z)       (for sin)
  ;;   z' = z + (x - cos(z))/sin(z)       (for cos)
  ;;
  ;; Here, x and y are normalized so that x^2 + y^2 = 1.
  ;;
  ;; If |x| > |y|, then the first iteration is used since the
  ;; denominator is larger.  Otherwise the second is used.
  (cond ((zerop-oct x)
	 ;; x = 0
	 (cond ((zerop-oct y)
		;; Both x and y are zero.  Use the signs of x and y to
		;; determine the result
		(error "atan2(0,0)"))
	       (t
		;; x = 0, but y is not.  Use the sign of y.
		(return-from atan2-oct/newton-t
		  (cond ((plusp (float-sign (oct-0 y)))
			 (copy-octi-into-octi +oct-pi/2+ tar))
			(t
			 (neg-oct-t +oct-pi/2+ tar)))))))
	((zerop-oct y)
	 ;; y = 0.
	 (return-from atan2-oct/newton-t
	   ;; Use the sign of x and y to figure out the result.
	   (cond ((plusp (float-sign (oct-0 x)))
		  (copy-octi-into-octi  +oct-zero+ tar))
		 ((plusp (float-sign (oct-0 y)))
		  (copy-octi-into-octi  +oct-pi+ tar))
		 (t
		  (neg-oct-t +oct-pi+ tar))))))

  (when (oct-= x y)
    (return-from atan2-oct/newton-t
      (copy-octi-into-octi
       (if (plusp-oct y)
	  +oct-pi/4+
	  +oct-3pi/4+) tar)))

  (when (oct-= x (neg-oct y))
    (return-from atan2-oct/newton-t
      (if (plusp-oct y)
	       (copy-octi-into-octi +oct-3pi/4+ tar)
	  (neg-oct-t +oct-pi/4+ tar))))

  (let* ((r (hypot-oct x y))
	 (xx (div-oct x r))
	 (yy (div-oct y r)))
     ;; Compute double-precision approximation to atan
    (let ((z (%make-oct-d (atan (oct-0 y) (oct-0 x))))
	  (sinz (%make-oct-d 0d0))
	  (cosz (%make-oct-d 0d0)))
      (cond ((> (abs (oct-0 xx))
		(abs (oct-0 yy)))
	     ;; Newton iteration  z' = z + (y - sin(z))/cos(z)
	     (dotimes (k 3)
	       (sincos-oct-t z sinz cosz)
	       (add-oct-t z (div-oct-t (sub-oct-t yy sinz sinz) cosz cosz) z)))
	    (t
	     ;; Newton iteration z' = z - (x - cos(z))/sin(z)
	     (dotimes (k 3)
	         (sincos-oct-t z sinz cosz)
	       (sub-oct-t z (div-oct-t (sub-oct-t xx cosz cosz)
				       sinz sinz) z))
	     
	     ))
      (copy-octi-into-octi z tar)
    ;  (format t "~% tar= ~s" tar)
      z)))


(defun asin-oct-t (a tar)
  "Asin(a)"
  (declare (type %quad-double a))
  (atan2-oct/newton-t a (sqrt-oct-t (sub-oct-t +oct-one+
					(sqr-oct-t a tar) tar) tar) tar))

(defun acos-oct-t (a tar)
  "Acos(a)"
  (declare (type %quad-double a))
  (atan2-oct/newton-t (sqrt-oct-t (sub-oct-t +oct-one+ (sqr-oct-t a tar) tar)tar)
	    a tar))

(eval-when (:execute :load-toplevel :execute)
  (load "oct-const"))












