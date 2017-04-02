;;; A quadruple-precison floating-point arithmetic package.
;;; hacked version of RJF's octi to be loaded in to Ray Toy's OCT
;;; for improvements to performance for Allegro and maybe other non-CMU
;;; lisp implementations.

;;;  Based on Raymond Toy's conversion of Yozo Hida's QD.  See
;;;  rtoy-copyright.txt for copyright notice and permission.
;;;  Why hacked?
;;;  Some lisps do not do aggressive in-lining as needed by RToy's
;;;  original version for CMU-CL.  Also CMU-CL has built-in double-double (dd) arithmetic.

;;; for more comments see octi.lisp

;;; this file should be revised for #+cmu-cl conditionalization if 
;;; rtoy really finds it advantageous to encode a quad-double as a complex dd.

;;; 11/29/07 RJF
(in-package :octi) 
(declaim (special ;;sina cosa 
		  +oct-zero+ +oct-one+ +oct-eps+ +oct-e+ +oct-pi+
		  +oct-log2+ +oct-2pi+ +oct-pi/2+ +oct-pi/1024+ +oct-pi/4+  +oct-3pi/4+
		  +oct-1/rt2+ +oct-cos-table+ +oct-sin-table+))
  
(deftype %quad-double ()   '(simple-array double-float (4)))

(defmacro oct-0(x) `(aref ,x 0))
(defmacro oct-1(x) `(aref ,x 1))
(defmacro oct-2(x) `(aref ,x 2))
(defmacro oct-3(x) `(aref ,x 3))

#+allegro(defmacro float-infinity-p(x)`(= (abs ,x) #.excl::*infinity-double*))
  
;;qts = quick-two-sum 

(eval-when (:execute :load-toplevel :compile-toplevel :execute)
 (defmacro %make-oct(a b c d)
  `(let ((r 
	  #-allegro ;; maybe #-(or allegro cmu-cl) ??
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
      (setf (oct-0 target)(cl:scale-float a0 k))
      (setf (oct-1 target)(cl:scale-float a1 k))
      (setf (oct-2 target)(cl:scale-float a2 k))
      (setf (oct-3 target)(cl:scale-float a3 k))
      target))


(defmacro qtsmac (s e x y);; s, e should be symbols.
  (let ((a (gensym))
	(b (gensym)))
    `(let* ((,a ,x)
	    (,b ,y))
       (declare (double-float ,s ,e ,a ,b) (optimize (speed 3)(safety 1)))
       (setf ,s (cl:+ ,a ,b))
       (setf ,e (cl:- ,b (cl:- ,s ,a))))))



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
       (cond ((cl:/= ,s1 0.0d0)
	      (qtsmac ,s1 ,s2 ,s1 ,c2)
	      (if (cl:/= ,s2 0.0d0)
		  (qtsmac ,s2 ,s3 ,s2 ,c3)
		(qtsmac ,s1 ,s2 ,s1 ,c3)))
	     (t   
	      (qtsmac ,s0 ,s1 ,s0 ,c2)
	      (if (cl:/= ,s1 0d0)
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
	(cond ((cl:/= ,s2 0d0)
	       (qtsmac ,s2 ,s3 ,s2 ,c3)
	       (if (cl:/= ,s3 0d0) (incf ,s3 ,c4)	 (incf ,s2 ,c4)))
	      (t (qtsmac ,s1 ,s2 ,s1 ,c3)
		 (if (cl:/= ,s2 0d0) (qtsmac ,s2 ,s3 ,s2 ,c4)
		   (qtsmac ,s1 ,s2 ,s1 ,c4)))))
       (t
	(qtsmac ,s0 ,s1 ,s0 ,c2)
	(cond ((cl:/= ,s1 0d0)
	       (qtsmac ,s1 ,s2 ,s1 ,c3)
	       (if (cl:/= ,s2 0d0)(qtsmac ,s2 ,s3 ,s2 ,c4)
		 (qtsmac ,s1 ,s2 ,s1 ,c4)))
	      (t
	       (qtsmac ,s0 ,s1 ,s0 ,c3)
	       (if (cl:/= ,s1 0d0) (qtsmac ,s1 ,s2 ,s1 ,c4)
		 (qtsmac ,s0 ,s1 ,s0 ,c4)))))))))

(defmacro copy-octi-into-octi (from to)  
  ;; not sanitized
  (let ((ans (list 'progn)))
    ;; expand the do loop in place
  (dotimes (i 4)
    (push `(setf (aref ,to ,i)(aref ,from ,i)) ans))
  (push to ans)
  (reverse ans)))



(defmacro copy-floats-into-octi (a b c d to)  
  (let ((ans (list 'progn)))
    (push `(setf (aref ,to 0),a) ans)
    (push `(setf (aref ,to 1),b) ans)
    (push `(setf (aref ,to 2),c) ans)
    (push `(setf (aref ,to 3),d) ans)
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
		(setf ,a-hi (cl:- ,tmp (cl:- ,tmp ,a)))
		(setf ,a-lo (cl:- ,a ,a-hi))))))))
(defun splitmacr1 (a)
  (let* ((tmp (* a #.(+ 1.0d0 (scale-float 1d0 -27))))
	 (as (* a #.(scale-float 1d0 -27)))
	 (a-hi (* (cl:- tmp (cl:- tmp as)) #.(expt 2 27)))
	 (a-lo (cl:- a a-hi)))
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
       (setf ,p-hi (cl:* ,a ,b))
       (splitmac ,a-hi ,a-lo ,a)
       (splitmac ,b-hi ,b-lo ,b)
       (setf ,p-lo (cl:+ (cl:+ (cl:- (cl:* ,a-hi ,b-hi) ,p-hi)
		   (cl:* ,a-hi ,b-lo)
		   (cl:* ,a-lo ,b-hi))
		      (cl:* ,a-lo ,b-lo))))))

(defmacro two-summac (s e %a %b)
  "Computes fl(a+b) and err(a+b)"
      (let ((a (gensym))
	    (b (gensym))
	    (v (gensym)))
    `(let ((,a ,%a)
	   (,b ,%b)
	   (,v 0.0d0))

  (declare (double-float ,a ,b ,v ,s ,e) (optimize (speed 3)(safety 1)(debug 0))) ;rjf 9/16/07
    (setf ,s (cl:+ ,a ,b))
    (setf ,v (cl:- ,s ,a))
    (setf ,e (cl:+ (cl:- ,a (cl:- ,s ,v))
		(cl:- ,b ,v))))))




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
;; rtoy uses other stuff for this, more complicated but maybe rounded better..
#|
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

|#  ;; now put this into rtoy's code.


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

;; I used to call this add-oct, add-oct-d  etc

(ss "add-qd" 2)(ss "add-qd-d" 2)
(ss "mul-qd" 2)(ss "mul-qd-d" 2)
(ss "sub-qd" 2)(ss "sub-qd-d" 2)
(ss "div-qd" 2)(ss "div-qd-d" 2)
(ss "expt-qd" 2)
(ss "invert-qd" 1)(ss "invert-qd-d" 1)
(ss "sqr-qd" 1) (ss "abs-qd" 1) (ss "neg-qd" 1)
(ss "1+-qd" 1) (ss "1--qd" 1)

#| ;; use rtoy's  sin-qd etc for this
(ss "sin-oct" 1) (ss "cos-oct" 1)(ss "tan-oct" 1) 
;;(ss "sinh-oct" 1) (ss "cosh-oct" 1)(ss "tanh-oct" 1)
(ss "asin-oct" 1) (ss "acos-oct" 1) (ss "atan-oct" 1 )
(ss "log-oct" 1) 
(ss "exp-oct" 1) (ss "sqrt-oct" 1)
(ss "hypot-qd" 2)

|#


;;; here are the arithmetic routines (with targets)

(defun add-qd-t (a b target) ;; compare to add-qd
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


(defun mul-qd-t (a b target)  ;;; compare to mul-qd!
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
	  (return-from mul-qd-t target)))
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
(defun sqrt-qd (x &optional (target (make-qd-d 0d0)))
  (sqrt-qd-t x target))

(defun sqrt-qd-t(a ans)
  (let ((sqr0 (sqrt (oct-0 a)))		; appx squareroot of z to 53 bits
	(sqr1 #.(%make-oct-d 0d0))	; temp storage allocated by compiler
	 )

    (cond ((cl::zerop sqr0)(copy-octi-into-octi +oct-zero+ ans))
	  (t (copy-floats-into-octi sqr0 0d0 0d0 0d0 sqr1)
					;3 Newton iterations.

	   (mul-qd-d-t (add-qd-t sqr1 (div-qd-t a sqr1 ans) ans) 0.5d0 sqr1)
	   (mul-qd-d-t (add-qd-t sqr1 (div-qd-t a sqr1 ans) ans) 0.5d0 sqr1)
	   (mul-qd-d-t (add-qd-t sqr1 (div-qd-t a sqr1 ans) ans) 0.5d0 ans)
	   ans))))

;; inversion is 1/x

(defun invert-qd-t(v h2);;h2 is the target.
  ;; a quartic newton iteration for 1/v
  ;; to invert v, start with a good guess, x.
  ;; let h= 1-v*x  ;; h is small
  ;; return x+ x*(h+h^2+h^3) . compute h^3 in double-float
  ;; enough accuracy.

  (let* 
      (
       (x #.(%make-oct 0d0 0d0 0d0 0d0));; allocate space for temp
       (h #.(%make-oct 0d0 0d0 0d0 0d0));;allocate space for temp
       (h3 0d0)
       )
    (declare  (type %quad-double v h h2)(double-float h3))    
    (setf (oct-0 x) (cl:/ (oct-0 v)))
    (add-qd-d-t  (neg-qd-t(mul-qd-t v x h) h) 1.0d0 h);set h
    (mul-qd-t h h h2)			;also use h2 for target
    (setf h3 (* (oct-0 h)(oct-0 h2)))
    (add-qd-t x 
	      (mul-qd-t x
			(add-qd-t h (add-qd-d-t h2 
						h3 
						h2) h2) h2) h2) ))

(defun invert-qd-d-t(v h2) ;; v is a double-float h2 is the target.
  ;; a quartic newton iteration for 1/v
  ;; to invert v, start with a good guess, x.
  ;; let h= 1-v*x  ;; h is small
  ;; return x+ x*(h+h^2+h^3) . compute h^3 in double-float
  ;; enough accuracy.

  (let* 
      ((x #.(%make-oct 0d0 0d0 0d0 0d0))
       (h #.(%make-oct 0d0 0d0 0d0 0d0)) ;;allocate space for temp
       (h3 h))
    (declare  (type %quad-double h h2)(double-float v h3))    
    (setf (oct-0 x) (cl:/ v))
    (add-qd-d-t  (neg-qd-t
		  ;;(mul-qd-d-t x v h)
		  (mul-qd-d x v)
		  h)1.0d0 h)		;set h
    (setf h2 (mul-qd h h))		;also use h2 for target
    (setf h3 (* (aref h 0)(aref h2 0)))
    (add-qd-t x 
	      (mul-qd-t x
			(add-qd-t h (add-qd-d-t h2 
					      h3 
					      h2) h2) h2) h2)))
				 
;; maybe check for division by zero or infinity?
;; should we use rtoy's programs here??
(defun div-qd-t(u v ans);;
  (mul-qd-t u (invert-qd-t v #.(%make-oct 0d0 0d0 0d0 0d0)) ans))

(defun div-qd-d-t(u v ans);;
    (mul-qd-t u (invert-qd-d-t v ans) ans))

(defun sqr-qd-t(a ans)
    ;; cheap hack for a wasteful program
  (mul-qd-t a a ans)
  ans)

(defun pow-qd-i(a n)			; n is any integer
  (if (not (and(arrayp a)(integerp n)))
      (error "bad args to pow-qd-i ~s ~s" a n))
  (let ((ans (%make-oct 0d0 0d0 0d0 0d0)))
    (cond ((< n 0)(invert-qd-t (pow-qd-i-t a (- n) ans) ans
				))
	  (t (pow-qd-i-t a n ans)))
    ans))
	
;; compute power of oct to an integer, store in ans
(defun pow-qd-i-t(a n ans)			; n is positive integer
  ;; this is generalized in expt-qd.
  (cond((cl:zerop n) (setf (aref ans 0) 1d0) ans)
       ((cl:evenp n)(sqr-qd-t (pow-qd-i-t a (cl:ash n -1) ans) ans))
       (t (mul-qd-t a (pow-qd-i a (1- n)) ans))))

(defun expt-qd-t(a b ans) ;; a^b, where a, b are both octs.
  ;;a^b = exp(b*log(a))
  ;; first see if b is really an integer.
  (let* ((r (oct-0 b))
	 (s (round r)))
       
    (if (and (cl:= r s)(cl:= 0d0 (oct-1 b) (oct-2 b) (oct-3 b)))
	(pow-qd-i-t a s ans)
      (exp-qd-t (mul-qd b (log-qd a)) ans))))

(defun mul-qd-d-t (a b ans)
  "Multiply quad-double A with B, result put in ans"
  (declare (type %quad-double a ans)
	   (double-float b)
	   (optimize (speed 3)(space 0)(safety 1)))
  (let ((p0 0d0)(q0 0d0)(p1 0d0)(q1 0d0) (p2 0d0)(q2 0d0))
    (declare (double-float p0 q0 p1 q1 p2 q2))
    (two-prodmac p0 q0 (oct-0 a) b)
    (when (float-infinity-p p0)
      (setf (oct-0 0)p0)
      (return-from mul-qd-d-t ans))
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

(defun neg-qd-t (a target)
  (let((a0 0d0)(a1 0d0)(a2 0d0)(a3 0d0))
    (declare (type %quad-double a target)
	     (double-float a0 a1 a2 a3)(optimize (speed 3)))
    (oct-partsmac a0 a1 a2 a3 a)
    (setf (aref target 0) (cl:- a0))
    (setf (aref target 1) (cl:- a1))
    (setf (aref target 2) (cl:- a2))
    (setf (aref target 3) (cl:- a3))
    target))

(defun abs-qd-t(a target)
  (if (two-arg-< a +oct-zero+) (neg-qd-t a target) 
    (copy-octi-into-octi a target)))

(defun sub-qd-t (a b target)
  (add-qd-t a (neg-qd b) target)) ; could expand out like add..

(defun sub-qd-d-t (a b target)
  (add-qd-d-t a (cl:- b) target))

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


(defun add-qd-d-t (a b ans)
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
      (return-from add-qd-d-t  ans))
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

(defun 1+-qd-t (a ans)(add-qd-d-t a 1d0 ans))
(defun 1--qd-t (a ans)(add-qd-d-t a -1d0 ans))


;; EVALUATING a POLYNOMIAL

;; Octlist is an ordinary lisp list of the coefficients in a
;; polynomial. The last item in the list is the constant
;; coefficient. All coeffs are octi objects.  

;; (setf testpoly (list (into 5) (into 3) (into 1))) ; 5*x^2+3*x+1 
;; (setf r0 (polyeval-oct testpoly (into 1)); returns 9
;; 

(defun polyeval-qd (octlist x)
  (let ((sum (into 0))
	(tmp #.(%make-oct 0d0 0d0 0d0 0d0)))
	(dolist (i octlist sum)
	  (add-qd-t (mul-qd-t x sum tmp) i sum))))

;; similar but dlist is a list of DOUBLE FLOATS.
;; evaluate the polynomial to a quad-double

(defun polyeval-qd-d (dlist x)
  (let ((sum (into 0)))
	(polyeval-qd-d-t dlist x sum)))

(defun polyeval-qd-d-t (dlist x sum) ;; sum is the target
  (let ((sum (copy-octi-into-octi +oct-zero+ sum))
	(tmp #.(%make-oct 0d0 0d0 0d0 0d0)))
	(dolist (i dlist sum)
	  (add-qd-d-t  (mul-qd-t x sum tmp) i sum))))

;;; an example to show that oct numbers can have very high pseudo precision..
;;;  (setf testpoly (list (into 5) (into 3) (into 1))) ; 5*x^2+3*x  
;;;  (setf testpoly2 (list (into 5) (into 3) (into 0))) ; 5*x^2+3*x  
;;;  (setf r1  (polyeval-qd testpoly (into (expt 2 510))))
;;; #(5.617791046444737d+307 1.0055855947456948d+154 1.0d0 0.0d0)
;;;  (setf r2  (polyeval-qd testpoly2 (into (expt 2 510))))
;;; #(5.617791046444737d+307 1.0055855947456948d+154 0.0d0 0.0d0)
;;;  (sub-qd r1 r2) --> 1

;;;  we can evaluate exp(x) for  |x|< 0.01, say, by
;;; a pade approximation, computed using Maxima.

;;; rtoy's program is more accurate, however.

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
(defun exp-qd-small(x) ;; re-use temps  exp(x) for |x|<0.01, to quad-double  
  (let* ((x2 (sqr-qd x))
	 (fx2 (polyeval-qd-d f-exp-pade x2))
	 (xgx2 (mul-qd x (polyeval-qd-d g-exp-pade x2))))
    ;; 12 oct*oct, 10 oct+double  so far. 

    ;;(format t "~%f=~s g=~s"  (add-qd-t fx2 xgx2 x2)(sub-qd-t fx2 xgx2 xgx2))
    ;; next line takes 2 oct adds and a divide
		(div-qd-t (add-qd-t fx2 xgx2 x2)(sub-qd-t fx2 xgx2 xgx2) fx2)
		))

#+ignore
(defun exp-qd-t(a target)
  (if (< (oct-0 a) 0d0)(invert-qd-t (exp-qd-t (neg-qd-t a target) target) target)
    
    (multiple-value-bind (m n)
	(divrempos-qd a +oct-log2+)

      ;; now a= m*log(2)+n.  compute 2^m* exp(n/256)^256.
 ;;     (format t "~%doing exp ~s, m=~s, n=~s" a m n)
      (mul-qd-t (pow-qd-i (into 2) (truncate (oct-0 m)));2^m
		 (pow-qd-i  (exp-qd-small 
				   (div-qd-d n  256.0d0))
			    256)
		 target))));;  (e^(n/256))^256




;;; patching up qd-fun...

(defun tan-qd (x &optional (target (make-qd-d 0d0)))
  (tan-qd-t x target))

(defun tan-qd-t (r targ)
  (let ((c #.(make-qd-d 0d0))) ;compiler temp for cos
  (declare (type %quad-double r targ c))
  (sincos-t r targ c);; we need to define an octi version of this. part is
    ;; What to do, what do?  If C is zero, we get divide by zero
    ;; error.  We could return infinity, but quad-double stuff doesn't
    ;; handle infinities very well.
  (div-qd-t s c targ)
  targ))


;; |a| must be small for this to work

(defun sincos-taylor-t (a s c)		;sin target, cos target
  (declare (type %quad-double a s c))
  (let ((thresh (cl:* +qd-eps+ (abs (qd-0 a)))))
    (when (zerop-qd a)
      (copy-octi-into-octi +qd-zero+ s)
      (copy-octi-into-octi +qd-one+ c)
      (return-from sincos-taylor-t s))
    (let* ((x #.(make-qd-d 0d0))
	   (p #.(make-qd-d 0d0))
	   (m 1d0))
      (declare (double-float m)(optimize (speed 3)))
      (neg-qd-t (sqr-qd-t a x) x)	;set x to -a
      (copy-octi-into-octi a s)
      (copy-octi-into-octi a p)
      (loop
       (mul-qd-t p x p)
       (incf m 2d0)
       (div-qd-d-t p (cl:* m (cl:1- m)) p)
       (add-qd-t s p s)
	;;(format t "p = ~A~%" (qd-0 p))
       (when (<= (abs (qd-0 p)) thresh)
	 (return)))
      ;; cos(c) = sqrt(1-sin(c)^2).  This seems to work ok, even
      ;; though I would have expected some round-off errors in
      ;; computing this.  sqrt(1-x^2) is normally better computed as
      ;; sqrt(1-x)*sqrt(1+x) for small x.
      (sqrt-qd-t (add-qd-d-t (neg-qd-t (sqr-qd-t s c) x ) 1d0 x) c) ; set c. s is already set
      s)))


(defun sin-qd-t(y tar &aux amod bmod) 
  (when (< (qd-0 y) 0)			;sin(-x)=-sin(x)
    (return-from sin-qd-t (neg-qd-t(sin-qd-t(neg-qd y) tar)tar)))
  (when (> (qd-0 y) 1000d0)
    (setf y (careful-range-reduction y)))
  (multiple-value-bind (a rest)
      ;;(ftruncate-qd y +qd-pi/4+)
      (divrempos-qd y +qd-pi/4+)
      
    (setf amod (mod (truncate (qd-0 a)) 8))
    (multiple-value-bind(b s)
	;;(ftruncate-qd rest +qd-pi/1024+)
	(divrempos-qd rest +qd-pi/1024+)
      (setf bmod (truncate (qd-0 b)))
      ;;(format t "~% a=~s b=~s s=~s "
      ;;    a b (qd2string s))
      (if (> amod 3)(neg-qd-t (sin-qd-aux-t (- amod 4) bmod s a b tar) tar)
	(sin-qd-aux-t amod bmod s a b tar)))))

(defun sin-qd-aux-t (a b s sins coss tar)
  (let ((ss (aref sina b))
	(cc (aref cosa b)))
    (sincos-taylor-t s  sins coss)  
    (case a 
      (0 (add-qd-t (mul-qd-t cc sins sins)(mul-qd-t ss coss coss) tar))
      (1 (mul-qd-t +qd-1/rt2+
		    (add-qd-t
		     (mul-qd-t coss (add-qd-t cc ss tar) coss)
		     (mul-qd-t sins (sub-qd-t cc ss tar) sins) tar) tar))
      (2 (sub-qd-t (mul-qd-t cc coss coss)(mul-qd-t ss sins sins) tar))
      (3 (mul-qd-t +qd-1/rt2+
		    (sub-qd-t
		     (mul-qd-t coss (sub-qd-t cc ss tar) coss)
		     (mul-qd-t sins (add-qd-t cc ss tar) sins)
		     tar)tar)))))

(defvar two-pi-many-bits ;; at least 1236 bits, as a rational number.
     22884743329868046105125534265462446198541589729467624782075250820510692843229684905276031268214145918457914724288873462827859618300605284995751611318802551321814951688142116429613752561980/3642220022337780247665573591773167826362669080317417579448364704693110029206203569295741890258185811272321756922288545062428726354857482731853161900688354006071607571978699476172264403277)

(defun careful-range-reduction(y)
    (multiple-value-bind (n m)		; y=n*2pi+m
	(truncate (qd2lisp y) two-pi-many-bits)
      (declare (ignore n))
      (into m)))

(defun qd2lisp (x)
  (with-qd-parts (x0 x1 x2 x3) x
    (+ (cl:rational x0)
       (cl:rational x1)
       (cl:rational x2)
       (cl:rational x3))))

;; see explanation, also more code..
;; similar for cos,  in c:/lisp/generic/octi.lisp

