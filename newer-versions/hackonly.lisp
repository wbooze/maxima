;;; attempts by RJF to make OCT work fast in Allegro.
;;; translating RTOY's code to macros, mostly.
;;;  OCT is a lisp-only version of quad-double arithmetic (e.g. Yozo Hida)
(in-package :octi) 

;;; In this file, and OCT generally, we use this for a
;;; quad-double. This is the underlying representation.  Separately we
;;; cover this over in a CLOS object wrapper as an abstraction that
;;; also provides for using the common names like +, *, .... Actually
;;; we implement programs like defmethod two-arg-+ (x oct)(y oct) ...)
;;; on top of a generic arithmetic system that implements "+" by
;;; appropriate calls to two-arg-+.

(deftype %quad-double ()   '(simple-array double-float (4)))

#+ignore
(defmacro float-infinity-p(x)`(and (excl::exceptional-floating-point-number-p ,x)
				   (not (excl::nan-p ,x))))

(defmacro float-infinity-p(x)`(= (abs ,x) #.excl::*infinity-double*))

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
     r))

;;(defmethod make-oct-d ((a array))  ;; make an object from the array
;;  (make-instance 'oct-real :value a))

;;(defmethod make-oct-d ((a float))  ;; make an object from the array
;;  (make-instance 'oct-real :value (%make-oct (coerce a 'double-float)
;;					   0d0 0d0 0d0)))

(defmacro oct-0(x) `(aref ,x 0))
(defmacro oct-1(x) `(aref ,x 1))
(defmacro oct-2(x) `(aref ,x 2))
(defmacro oct-3(x) `(aref ,x 3))

;;(defmacro %make-oct-d (a b c d) not used

;;qts = quick-two-sum 

(defmacro qtsmac (s e x y);; s, e should be symbols.
  (let ((a (gensym))
	(b (gensym)))
    `(let* ((,a ,x)
	    (,b ,y))
       (declare (double-float ,s ,e ,a ,b) (optimize (speed 3)(safety 1)))
       (setf ,s (+ ,a ,b))
       (setf ,e (- ,b (- ,s ,a))))))

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
	    (mul-oct h h)))))


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

#+ignore
;; same program, yozo's code

(defun time-mul (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let ((h (into 1/3)))
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
  (let ((h (into 1/3))
	(g (into 0)))
    (time (dotimes (k n)
      (declare (fixnum k))
	  (dsetv g  (* h h))))))


;; splitmac works 10/1/07 RJF

(defmacro splitmac (a-hi a-lo %a) ;; set a-hi and a-lo
  "Split the double-float number a into a-hi and a-lo such that a =
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


;;oct-to-decimal decoder. 
;;; this is OCT's oct, which is an array

(defun oct-decode(q &optional (digits 65) (base 10)) 
  ;; returns 3 integers: sign; fraction to right of dec.point; base-10 expon.
  (decimalize  (apply #'cl::+ (map 'list  #'rational (coerce q
							     'list)))
	       digits ;; could be more than 65 if you have 1.0+1.0q-80 !
	       base))

;; convert rational r to a decimal [or other base] number with n digits
;; showing in the fraction. If r=0, then just 0.
;; return sign=1,-1, or 0;  fraction is an integer, exponent is an integer.

(defun decimalize(r n &optional (base 10)); r rational number >=0
  (let* ((expon  (largest-power-less-than r base))
	 (frac (truncate;; was, round. causes tricky edge cases.
		(cl::*(cl::abs r) (cl::expt base (cl::- n expon 1))))))
    (values   (signum r)
	      frac
	      (cl::1+ expon))))

;; return z such that base^z<r,  
;; not completely general. Works for
;; rational r, where r can be converted without overflow to double-float
;; Should be OK for oct numbers.

(defun largest-power-less-than(r base)
  (if (cl::= r 0) -1
  (let ((guess (cl::floor(cl::log (cl::* 1.0d0 (cl::abs r)) base))))
    (if (>= (expt base guess) r) (1- guess) guess))))


;;(loop for i from 10 while (< i 20) do (print (list i (multiple-value-list (oct-decode (- 1 (expt #q10 (- i)))))) ))

;; make a formatted output . Something like this does the job

(defparameter *oct-digits-to-show* 66) ; MAX number of digits to show. Trailing zeros are omitted.

;; oct's oct ... just an array
(defmethod oct2string((x array))
  ;; check if x is a NaN
  (if (excl::nan-p (aref  x 0)) "NaN(oct)"
    (multiple-value-bind (s r e h)  ;h is extra variable
	(decimalize (oct2lisp x) *oct-digits-to-show* 10)
      (format nil "~a0.~aQ~s" (if (cl::< s 0)"-" "") 
	      (string-right-trim 
	       "0"  
	       (subseq (setf h(format nil "~a" r)) 0  
		       (cl::min (length h) *oct-digits-to-show*)) )
	      e))))

(defmethod oct2lisp((x array))
  (if (excl::nan-p (aref  x 0)) (aref x 0)
					; if oct contains a NaN, use it.
    (apply #'cl::+  (map 'list #'rational  x ))))



(defun into(r &optional (where (%make-oct-d 0d0)))
  (lisp2oct r where))

;; The following methods REQUIRE a place to put the answer, and so
;; are less convenient to use directly.
;;;************FIX THIS


(defmethod lisp2oct((x rational) (ans array)) ;to encode a lisp rational, divide num/den
 (div-oct-t (into (numerator x))(into (denominator x)) ans))


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
	(setf p (coerce (decf x (rational p)) 'double-float))
	(add-oct-d-t ans p ans)))))


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

(defmacro oct-partsmac (s0 s1 s2 s3 rr) ;; pick apart a OCT
  `(let() 
     (setf ,s0 (aref ,rr 0))
     (setf ,s1 (aref ,rr 1))
     (setf ,s2 (aref ,rr 2))
     (setf ,s3 (aref ,rr 3))))

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

(defun add-oct(a b)
  "Add two quad-doubles A and  B"
  (let ((ans(%make-oct 0d0 0d0 0d0 0d0)))
    (add-oct-t a b ans)))

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
      (let* ((s0 (cl:+ a0 b0))
	     (s1 (cl:+ a1 b1))
	     (s2 (cl:+ a2 b2))
	     (s3 (cl:+ a3 b3))
	     (v0 (cl:- s0 a0))
	     (v1 (cl:- s1 a1))
	     (v2 (cl:- s2 a2))
	     (v3 (cl:- s3 a3))
	     (u0 (cl:- s0 v0))
	     (u1 (cl:- s1 v1))
	     (u2 (cl:- s2 v2))
	     (u3 (cl:- s3 v3))
	     (w0 (cl:- a0 u0))
	     (w1 (cl:- a1 u1))
	     (w2 (cl:- a2 u2))
	     (w3 (cl:- a3 u3)))
	(let* ((u0 (cl:- b0 v0))
	      (u1 (cl:- b1 v1))
	      (u2 (cl:- b2 v2))
	      (u3 (cl:- b3 v3))
	      (t0 (cl:+ w0 u0))
	      (t1 (cl:+ w1 u1))
	      (t2 (cl:+ w2 u2))
	      (t3 (cl:+ w3 u3)))

	  (declare (double-float t0 t1 t2 t3 u0 u1 u2 u3))
	  (two-summac s1 t0 s1 t0)
	  (three-summac  s2 t0 t1 s2 t0 t1)
	  (three-sum2mac s3 t0 s3 t0 t2)
	  (setf t0 (cl:+ t0 t1 t3))
	  ;; Renormalize
	 ;; (format t "~% renorm ~s ~s ~s ~s ~s" s0 s1 s2 s3 t0)
	  (renorm-5mac s0 s1 s2 s3   s0 s1 s2 s3 t0))
	(cond ((and (zerop a0) (zerop b0))
	       (setf (aref target 0) 0d0)
	       (setf (aref target 1) 0d0)
	       (setf (aref target 2) 0d0)
	       (setf (aref target 3) 0d0))
	      (t
	       (setf (aref target 0) s0)
	       (setf (aref target 1) s1)
	       (setf (aref target 2) s2)
	       (setf (aref target 3) s3)))
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
      #+nil
      (format t "p0,p1,s0,s1,s2 = ~a ~a ~a ~a ~a~%"
	      p0 p1 s0 s1 s2)
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
      
(defun mul-oct(a b)
    "Multipl  quad-doubles A and B"
 (let ((ans(%make-oct 0d0 0d0 0d0 0d0)))
   (mul-oct-t a b ans)))

(defun sqr-oct(a)
    ;; cheap hack for a wasteful program
  (mul-oct a a))

(defun sqr-oct-t(a ans)
    ;; cheap hack for a wasteful program
  (mul-oct-t a a ans)
  ans)

(defun pow-oct-i(a n)			; n is any integer
  (if (not (and(arrayp a)(integerp n)))
      (error "bad args to pow-oct-i ~s ~s" a n))
  (let ((ans (%make-oct 0d0 0d0 0d0 0d0)))
    (cond ((< n 0)(div-oct  (%make-oct 1.0d0 0d0 0d0 0d0)
			   (pow-oct-i-aux a (- n) ans)))
	  (t (pow-oct-i-aux a n ans)))
    ans))
	
;; compute power of oct to an integer, store in ans
(defun pow-oct-i-aux(a n ans)			; n is positive integer
  ;; should be generalized, just doing this for testing
  (cond((zerop n) (setf (aref ans 0) 1d0) ans)
       ((evenp n)(sqr-oct-t (pow-oct-i-aux a (ash n -1) ans) ans))
       (t (mul-oct-t  a (pow-oct-i-aux a (1- n) ans) ans) ans)))

  
(defun mul-oct-d(a b)
 (let ((ans(%make-oct 0d0 0d0 0d0 0d0)))
   (mul-oct-d-t a b ans)))

;; real version modified from from OCT

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

(defun neg-oct (a)
  (let((a0 0d0)(a1 0d0)(a2 0d0)(a3 0d0))
    (declare (double-float a0 a1 a2 a3)(optimize (speed 3)))
    (oct-partsmac a0 a1 a2 a3 a)
    (%make-oct (cl:- a0) (cl:- a1) (cl:- a2) (cl:- a3))))

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

(defun sub-oct (a b)
  (add-oct a (neg-oct b)))

(defun sub-oct-t (a b target)
  (add-oct-t a (neg-oct-t b target) target))

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

(defmacro quick-renormmac (c0 c1 c2 c3;; output
			   %c0 %c1 %c2 %c3 %c4;; input
			   )
  ;; macro is not sanitized
  
  `(let ((t0 0.0d0)(t1 0.0d0)(t2 0.0d0)(t3 0.0d0) (s 0.0d0))
     (declare (double-float ,c0 ,c1 ,c2 ,c3 t0 t1 t2 t3 )
	      (optimize (speed 3)(safety 1)))

     (qtsmac s t3 ,%c3 ,%c4)  ; quick-two-sum  macro
     (qtsmac s t2 ,%c2 s)
     (qtsmac s t1 ,%c1 s)
     (qtsmac ,c0 t0 ,%c0 s)
     (qtsmac s t2 t2 t3)
     (qtsmac s t1 t1 s)
     (qtsmac ,c1 t0 t0 s)
     (qtsmac s t1 t1 t2)
     (qtsmac ,c2 t0 t0 s)
     (setf ,c3 (cl:+ t0 t1))))

(defun add-oct-d(a b)
    "Add a quad-double A and a double-float B"
 (let ((ans(%make-oct 0d0 0d0 0d0 0d0)))
   (add-oct-d-t a b ans)))

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

;; how about inversion, that is, 1/x?

(defun invert-oct(v) 
  (let ((h2 (%make-oct 0d0 0d0 0d0 0d0)))
    (invert-oct-t v h2)))

(defun invert-oct-t(v h2) ;;h2 is the target.
  ;; a quartic newton iteration for 1/v
  ;; to invert v, start with a good guess, x.
  ;; let h= 1-v*x  ;; h is small
  ;; return x+ x*(h+h^2+h^3) . compute h^3 in double-float
  ;; enough accuracy.

  (let* 
      ((x (%make-oct (cl:/ (aref v 0)) 0d0 0d0 0d0))
       (h(%make-oct 0d0 0d0 0d0 0d0)) ;;allocate space for temp
       (h3 h))
    (declare  (type %quad-double v h h2)(double-float h3))    
    (add-oct-d-t  (neg-oct-t(mul-oct-t v x h) h)1.0d0 h) ;set h
    (setf h2 (mul-oct h h))		;also use h2 for target
    (setf h3 (* (aref h 0)(aref h2 0)))
    (add-oct-t x 
	      (mul-oct-t x
			(add-oct-t h (add-oct-d-t h2 
					      h3 
					      h2) h2) h2) h2)))

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

(defun div-oct(u v);;
  (let ((ans (%make-oct 0d0 0d0 0d0 0d0)))
    (mul-oct-t u (invert-oct-t v ans) ans)))

(defun div-oct-t(u v ans);;
  (mul-oct-t u (invert-oct-t v ans) ans))

(defun div-oct-d(u v);;
  (let ((ans (%make-oct 0d0 0d0 0d0 0d0)))
    (div-oct-d-t u v ans)))

(defun div-oct-d-t(u v ans);;
    (mul-oct-t u (invert-oct-d-t v ans) ans))


(defun zerop-oct(x)(= (oct-0 x) 0d0))

(defun scale-float-oct (oct k)
  (declare (type %quad-double oct)
	   (type fixnum k)
	   (optimize (speed 3) (space 0)))
  ;; (space 0) to get scale-double-float inlined??
  (let((a0 0d0)(a1 0d0)(a2 0d0)(a3 0d0))
    (declare (double-float a0 a1 a2 a3))
      (oct-partsmac a0 a1 a2 a3  oct)
    (%make-oct-d (scale-float a0 k)
	       (scale-float a1 k)
	       (scale-float a2 k)
	       (scale-float a3 k))))

(defmacro copy-oct (from to)
  `(let()
       (dotimes (i 4 ,to)
	   (setf (aref ,to i)(aref ,from i)))))


(defun sqrt-oct(a)(let((ans (%make-oct-d 0d0)))
		    (sqrt-dq-t a ans)))

;; this is an example of a hand-coded program using destructive
;; operations.

(defun sqrt-oct-t(a ans)
  (let* ((as (oct-0 a))			; approx to z, the first 53 bits.
	 (sqr0 (sqrt as))		; appx squareroot of z
	 (sqr1 (%make-oct-d sqr0))	; convert approx to oct
	 )
    (cond ((zerop sqr0)(copy-oct +oct-zero+ ans))
	  (t				;3 Newton iterations.
	   (mul-oct-d-t (add-oct-t sqr1 (div-oct-t a sqr1 ans) ans) 0.5d0 sqr1)
	   (mul-oct-d-t (add-oct-t sqr1 (div-oct-t a sqr1 ans) ans) 0.5d0 sqr1)
	   (mul-oct-d-t (add-oct-t sqr1 (div-oct-t a sqr1 ans) ans) 0.5d0 ans)
	   ans))))


;; This is another example of a hand-coded program using destructive
;; operations. Used after reduction so |a|<pi/2048.
  
(defun sincos-taylor-t (a s-ans c-ans)
  (declare (type %quad-double a))
  (let ((thresh (cl:* +oct-eps+ (abs (oct-0 a)))))
    (when (zerop-oct a)
      (return-from sincos-taylor-t
	(values +oct-zero+
		+oct-one+)))
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

      ;; cos(c) = sqrt(1-sin(c)^2).  This seems to work ok, since c is small.

      (copy-oct s s-ans)
      (sqrt-oct-t (add-oct-d (neg-oct (sqr-oct s)) 1d0) c-ans))))

(defstruct oct (real  #+allegro
		      (make-array 4 :element-type 'double-float 
			       :initial-element 0.0d0 
			       ;; next line is Allegro-specific
			       :allocation :lispstatic-reclaimable
			       )
		      #-allegro
		      (make-array 4 :element-type 'double-float 
			       :initial-element 0.0d0)

		   :type (simple-array double-float (4))))

(defmacro defarithmetic (op pgm)
    (let ((two-arg
	   (intern (concatenate 'string "two-arg-" (symbol-name op))
		   :ga))
	  (oo-entry ;; two oct args
	   (intern (concatenate 'string (symbol-name pgm) "-oct-t")
		   :ga))
	  (od-entry ;; oct and double args
	   (intern (concatenate 'string (symbol-name pgm) "-oct-d-t") :ga)))
	
 ;;           (format t "~% defining ~s" two-arg)
      `(progn
	 
	 ;; new defmethods for oct. ..
	 (defmethod ,two-arg ((arg1 oct-real) (arg2 oct-real))
	   (let* ((r (make-oct-d 0d0)) 
		  (in (oct-real r))
		  (a1 (oct-real arg1))
		  (a2 (oct-real arg2)))
	     (declare (optimize speed)
		      (type(simple-array double-float (4)) in a1 a2))
	     (,oo-entry a1 a2 in) r))
       
	 (defmethod ,two-arg((arg1 real) (arg2 oct-real))
	   (let* ((r (into arg1))
		  (in (oct-real r))
		  (a2 (oct-real arg2)))
	     (declare (optimize speed)
		      (type(simple-array double-float (4)) in a1 a2))
	     (,od-entry in a2 in) r))
       
	 (defmethod ,two-arg ((arg1 oct-real) (arg2 real))
	   (let* ((r (into arg2))
		  (in (oct-real r))
		  (a1 (oct-real arg1) ))
	     (declare (optimize speed)
		      (type(simple-array double-float (4)) in a1 a2))
	   
	     (,od-entry a1 in in) r))
	 (setf (get ',op 'argnum) 2) ;used by with-temps, dsetv
	 (setf (get ',op 'oct-program) ',pgm) ;used by with-temps, dsetv
	 (setf (get ',two-arg 'oct-program) ',pgm) ;used after macroexpand-all
	 (setf (get ',two-arg 'argnum) 2)
	 
	 )))


(defarithmetic + add)
(defarithmetic * mul)
(defarithmetic - sub)
(defarithmetic * div)
;; need to do other elementary functions etc as well.
;; see lisp/generic/qd and  lisp/oct/qd-fun..

