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
	   expt-oct-t expt-oct
	   pow-oct-i pow-oct-i-t
	   1--oct-t
	   1+-oct-t))

(in-package :octi)

(eval-when (:execute :load-toplevel :compile-toplevel :execute)
  (declaim (special  +oct-zero+ +oct-one+ ))
(in-package :octi)  
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


;;; ARITHMETIC


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
(ss "expt-oct" 2)
;;(ss "hypot-oct" 2)
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
	(b0 0d0)(b1 0d0)(b2 0d0)(b3 0d0) 
	;;(target #.(make-array 4 :element-type 'double-float   :allocation :lispstatic-reclaimable))
	)
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
	;;(copy-octi-into-octi target tar);***
	target
	)))

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
  (if (<-oct-t a +oct-zero+ target) (neg-oct-t a target) 
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


;;; see also oct/qd-methods for Ray's version of =, /=, >, etc etc
;;; Tiring to do it over again. Can we just use it?  I think I like my
;;; version with  ga::monotone, or see compare.


;; all compares with NaNs should be  false (nil).

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

(defmethod oct2lisp((x array))
  (if (excl::nan-p (aref  x 0)) (aref x 0)
					; if oct contains a NaN, use it.
    (apply #'cl::+  (map 'list #'cl::rational  x ))))

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
	'((+ two-arg-+ add-oct-t)
	  (excl::+_2op two-arg-+ add-oct-t)
	  (- two-arg-- sub-oct-t)
	  (excl::-_2op two-arg-- sub-oct-t)
	  (* two-arg-* mul-oct-t)
	  (excl::*_2op two-arg-* mul-oct-t)
	  (/ two-arg-/ div-oct-t)
	  (excl::/_2op two-arg-/ div-oct-t)
	  (expt two-arg-expt expt-oct-t)
	  ;;etc
	  
	  (excl::>_2op  two-arg->  >-oct-t )  ;; see below
	  (excl::>=_2op two-arg->= >=-oct-t)
	  (excl::<_2op  two-arg-<  <-oct-t )
	  (excl::<=_2op two-arg-<= <=-oct-t)
	  (excl::=_2op  two-arg-=  =-oct-t )
	  (excl::/=_2op two-arg-/= /=-oct-t)
	      ))
  
  ;; it's not so neat to have  (excl::>_2op two-arg-> >-oct-t)
  ;; because the target isn't an octi, nor is the target necessary at all.
  ;
  
  ;; we need another class of programs: 2 arg functions returning true/false.
  ;; more leg work.

(defun compare-oct(a b)   ;; a<b -1.  a=b: 0.  a>b:1.  NaN : nil.
    (cond((or (excl::nan-p (aref a 0))
	      (excl::nan-p (aref b 0)))
	  nil) ; no comparison with NaN returns true
	
	 (t (dotimes (i 4 0) ; equal
	   (cond ((= (aref a i)(aref b i))) ; keep going if equal
		 ((> (aref a i)(aref b i)) (return 1))
		 (t (return -1)))))))

(defun >-oct-t (a b ignore)
  (declare (ignore ignore))
  (eq 1 (compare-oct a b)))

(defun <-oct-t (a b ignore)
  (declare (ignore ignore))
  (eq -1 (compare-oct a b)))

(defun >=-oct-t (a b ignore)
    (declare (ignore ignore))
  (let ((h(compare-oct a b)))
    (or (eq 0 h)(eq 1 h))))

(defun <=-oct-t (a b ignore)
    (declare (ignore ignore))
  (let ((h(compare-oct a b)))
    (or (eq 0 h)(eq -1 h))))

(defun =-oct-t(a b ignore)
      (declare (ignore ignore))
      (eq 0 (compare-oct a b)))

(defun /=-oct-t(a b ignore)
      (declare (ignore ignore))
  (not (eq 0 (compare-oct a b))))


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
	   ;; controversy: should sqrt/sin/cos/tan/exp/log
	   ;; be supported by dsetv in compiled mode, or just
	   ;; by consing up the data type?
	   ;; if supported, we must implement sin-oct-t etc in octi.
	   ;; if unsupported, all we need is sin  in quad-real
	   ;; it also means that sqrt can return a quad-complex.
	   ;;etc
	   ;; RJF and RTOY now think the overhead for sqrt...log
	   ;; is sufficient that savings for re-using targets is
	   ;; not important.
	      ))
   
  ;; we might wish to distinguish expt from power [integer exponent]
  

;; make a new copy of an oct, or just return the value:
(defun copy-if-oct(p)(if (arrayp p)(make-array 4  :element-type 'double-float :initial-contents p) p))
       
(defun copy-octi(from to); copy from -> to
  (dotimes (i 4 nil)(declare (optimize (speed 3)(safety 0))(fixnum i))
    (setf (aref to i)(aref from i))))

(defun cmexpand(ex)  ;; compiler macro and regular macro expand
 (if (consp ex)  
     (macroexpand 
      (let ((cmf (compiler-macro-function (car ex))))
	(if cmf (funcall cmf ex nil)  ex)))
  ex))

    
); end of eval-when




;; input, like oct..
(defun q-reads-octi()
  (set-dispatch-macro-character #\# #\q #'octi::octi-reader) )
;; (q-reads-octi)

(defun octi-reader(stream subchar arg)
  (declare (ignore subchar arg))
  (let ((s (read stream)))

    (string2octi (format nil "~s" s))))


(defun string2octi(s);; read integerQinteger like 10Q2   = 100 = 0.1Q3  or  -3.1q-1
  ;; examples of acceptable numbers [put " " around them]
  ;; .1q0 ; 1Q0; 1.2q3;  -1.23q-4;  - .23 q -4;  2.3q+4; +4.5q6; q0; --3Q1 same as 3q1
  ;; 3 ; 3.4; 123.45q+100; -.q0 ; .q0 ;
  ;; examples of not acceptable numbers:
  ;;    q ; .q;   123.45q+1000  [=NaN(qd)]
  (let ((p0 0)(sign (into 1)))
    (setf s (delete #\  s));; remove leading or other spaces from string.
    (if (char= #\- (aref s 0))(setf p0 1  sign  (into -1)));; set sign if negative
    (multiple-value-bind (frac pos);; read fraction to left of .
	(parse-integer s :start p0 :radix 10 :junk-allowed t)
  ;;    (format t "~% frac= ~s pos=~s" frac pos)
      (if (null frac)(setf frac 0));; empty fraction is zero
      (if (= pos (length s)) (mul-oct sign (into frac)) ;#q1234
	(case (aref s pos);; look at next char
	  ((#\Q #\q #\d #\D #\e #\E);; 10Q2
	   (if (cl::= (1+ pos) (length s))(mul-oct sign  (into frac))
	   (multiple-value-bind (expon pos2)
	       (parse-integer s :start (1+ pos);skip the Q
			      :radix 10)
	    ;; (format t "~% sign=~s frac=~s expon=~s" sign frac expon)
	     (mul-oct  (mul-oct sign (into  frac)) (into (expt 10 expon))))))
	  (#\.;; 
	   (multiple-value-bind (frac2 pos2)
	       (parse-integer s :start (1+ pos);skip the "."
			      :radix 10 :junk-allowed t )
	     (if (null frac2)(setf frac2 0))
	     (setf frac
		   (add-oct (into frac)
			    (into(* (if (< frac 0) -1 1) frac2 (cl::expt 10 (cl::1+(cl::- pos pos2)))))))
	     
	   ;;  (format t "~% string pos2 ~s frac= ~s" pos2 frac)

	     (if (cl::= (1+ pos2) (length s))(mul-oct sign  frac)
	       (case (aref s pos2)
		 ((#\Q #\q);; 10Q2
		  (if (=  pos2(length s))   (mul-oct  frac  sign)
		      (multiple-value-bind (expon pos3)
		      (parse-integer s :start (1+ pos2);skip the Q
				     :radix 10 )
		    (mul-oct  frac (* sign (cl::expt 10 expon))))))

		 (otherwise 
		  (format t "next char is ~a -- Not an oct spec: ~s"  
			  (aref s pos2) s)
		  (into (or frac 0))))))))))))

;; tests?

#| some working examples..

octi(17): (macroexpand '(with-temps (let ((x 3)(y 4)) (+ x y))))
(let ((x #(3.0d0 0.0d0 0.0d0 0.0d0)) (y #(4.0d0 0.0d0 0.0d0 0.0d0)))
  (add-oct-t x y #(1.0d0 0.0d0 0.0d0 0.0d0)))

octi(20): (macroexpand '(dsetv  w (let ((x 3)(y 4)) (+ x y))))
(copy-octi (let ((x #(3.0d0 0.0d0 0.0d0 0.0d0))
                 (y #(4.0d0 0.0d0 0.0d0 0.0d0)))
             (add-oct-t x y #(1.0d0 0.0d0 0.0d0 0.0d0)))
           w)

octi(37): (cmexpand '(with-temps (setf (aref w 3) 2 q 25)))
(progn (excl::.inv-s-aref (copy-if-oct #(2.0d0 0.0d0 0.0d0 0.0d0)) w 3)
       (let* ((#:g271076 #(25.0d0 0.0d0 0.0d0 0.0d0)))
         (setq q (copy-if-oct #:g271076))))

octi(73): (pprint (cmexpand '(with-temps (dotimes (i 100) (dsetv w (+ w 1))))))

(block nil
  (let ((i #(0.0d0 0.0d0 0.0d0 0.0d0)))
    (tagbody
      #:Tag114
        (if (>=-oct-t i #(100.0d0 0.0d0 0.0d0 0.0d0)
                      #(1.0d0 0.0d0 0.0d0 0.0d0))
            (progn (return-from nil (progn)))
          nil)
        (tagbody (add-oct-t w #(1.0d0 0.0d0 0.0d0 0.0d0) w))
        (let ()
          (setq i
                (copy-if-oct (add-oct-t i #(1.0d0 0.0d0 0.0d0 0.0d0)
                                        #(1.0d0 0.0d0 0.0d0 0.0d0))))
          nil)
        (go #:Tag114))))


 octi(69):  (pprint (cmexpand '(with-temps (let ((i 123))(dsetv x (+ x i))))))
(let ((i #(123.0d0 0.0d0 0.0d0 0.0d0))) (add-oct-t x i x))


;; recursive factorial..
(defun fact(x)(if (<= x 1) 1 (* x (fact (- x 1)))))
;; try (fact 4)

(defun qfact(x)(with-temps (if (<= x 1) 1 (* x (qfact (1- x))))))
;; try (qfact (into 4))  
;; or (q-reads-octi) (qfact #q4)

;;(pprint (macroexpand '(with-temps (if (<= x 1) 1 (* x (qfact (1- x)))))))

;; iterative factorial
(defun fact4(x) (do ((i 1 (1+ i)) (f 1 (* f i)))((> i x) f)))
(defun qfact4(x)(with-temps (do ((i 1  (1+ i)) (f 1  (* f i)))((> i x) f))))
;; try (qfact4 (into 4))  
;; or (q-reads-octi) (qfact4 #q4)

(pprint (cmexpand '(with-temps (do ((i 1  (1+ i)) (f 1  (* f i)))((> i x) f)))))

(defun tryit(n)(oct2string(qfact4 (into n))))

|#

  ;; how it is done..  (dsetv a (+ b c)) 
  ;; should be faster than (setf a (+ b c)). maybe 2X. AFTER being compiled.
  ;; note that (macroexpand '(dsetv a (+ b c))) --> (add-oct-t b c a)

  ;; All the logic below is done during macro-expansion,
  ;; which means it is usually done at compile time. 
  
  ;; new version, 12/12/07.
  ;;First do a code walk expanding other macros, using Dick Water's code in mexp
  ;; wt is helper for  with-temps.

(defun wt(expr)
  (let ((*names* nil)
	(*howmany* 0))
    ;; (format t "~% with-temps expanding ~s " expr)
    (labels 
	((genlist(n) (loop for i from 1 to n collect (into i)))
	 (ct1 (r);; count temporaries needed
	      (cond ((numberp r) (incf *howmany*))
		    ((not (consp r)) r)
		    (t (incf *howmany*)
		       (mapc #'ct1 (cdr r)))))
		
	 (maketemps(r)			;change r=(+ a (* b c)) to  temp storage .
	   
	   ;;   (format t "~% expand with-temps for r=~s" r)
	   (cond ((numberp r) (into r))
		 ((atom r) r)
		 ((consp (car r));; not normal function style, cons in the car of form.
		  ;;    (format t "~% odd form ~s"r)
		  (mapcar #'wt r ))
			     
		 ((get (car r) 'argnum)	; known operator
		  ;;(format t "~%argnum known, r=~s" r)
		  ;; m, figure this out...
		  ;; dsetv expansion
		  (ds (pop *names*) (cons (car r) (mapcar #'maketemps (cdr r)))))
		 ((arrayp r) r)
		 (t
		  (case (car r)
		   ((let let*)
		     (cons (car r)		; let or let*
			(cons (mapcar 
			       #'(lambda(x); pair: name, value
				   (list (car x); variable
					 (wt (cadr x))	 ))
			       (cadr r))
			      (mapcar #'wt (cddr r)))))
		   
		 ((go make-array copy-if-oct return-from aref
				   add-oct-t mul-oct-t sub-oct-t div-oct-t);; etc
		   r) 
		 
		 (( setq setf psetq)
		  (cons (car r)		;  expand val in (set* name val ..); protect via copy
			(cons (cadr r)
			      (mapcar #'(lambda(x)
					  `(copy-if-oct ,(wt x)))
				      (cddr r)))))
		
		 ((if when block tagbody progn)
		  (cons (car r)		;  expand everything after progn
			(mapcar #'wt (cdr r)   )))
		 (excl::.inv-s-aref  ;; setf method for aref in Allegro
		  `(,(car r) (copy-if-oct ,(wt (cadr r))) ,@(cddr r)))
		 (otherwise  (cons (car r)	;  expand everything after unknown function, (foo ...)

			    (mapcar #'(lambda(x);; expand (f a b) to 
					;;(f (with-temps a)(with-temps b))
					;; with copy-if-oct around args
					(let((m (wt x)))
					  (if (and (consp m)(eq (car m) 'copy-if-oct)) m 
					    `(copy-if-oct ,m))))
				    (cdr r) )))
		 )))))

      (setf expr(mexp:macroexpand-all expr)) ;; use code-walker for compiler expansions too
      (ct1 expr) ; count temporaries needed
      (setf *names* (genlist *howmany*))
      (maketemps expr))))

(defun ds (targ ex)
  (setf ex (mexp:macroexpand-all ex))	;check for compiler macro, too
  
  ;;(format t "~% expanding ex in dsetv, ex=~s" ex)
  
  (cond 
   ((atom ex) `(into ,ex ,targ))
   ((eq (car ex) 'into) `(into ,@(cdr ex)  ,targ))
   (t
    (let* ((op (car ex))
	   (args (cdr ex))
	   (the-op (get op 'octi-program))
	   (argnum (get op 'argnum))	   )
      (cond 	       
       ((not the-op);; not a previously listed op
	`(copy-octi ,(wt ex) ,targ))
       ((not (eql argnum (length args))) 
	(error "ds was given operator ~s which expects ~s args, but was given ~s --  ~s" 
	       op argnum (length args) args))
       (t
	(case argnum
	  (1;; one argument.
	   `(,the-op  ,(wt (car args)) , targ))
	  (2
	   `(,the-op ,(wt  (car args))
		     ,(wt  (cadr args))  ,targ))

	  (otherwise (error "argnum is wrong for op ~s " op))
	  )))))))

(defmacro with-temps(r) (wt `,r))
(defmacro dsetv(r s) (ds `,r `,s))
