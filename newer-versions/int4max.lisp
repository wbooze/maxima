;;  A basis for interval arithmetic
;;  constructed by overloading generic arithmetic. 
;;  Richard Fateman, November, 2005
;;  revised and extended, 12/18/07 

;; stuff added for MAXIMA/Macsyma
;; many of the nice parts ripped out.
;; in GCL, damaged packaging and CLOS makes this original not work...
;; among the features now removed, the possibility of having as 
;; interval endpoints any of the various numeric real types in
;; an associated generic arithmetic (ga) package. Though this will
;; work for Common Lisp types like rationals, integers, floats.

;; this is a fairly  primitive  version, not like ninterval.  Two instances
;; of an interval, say [-1,1] are the same only if their index (an invisible 3rd arg)
;; is the same.  thus x*y and x*x  can be distinguish if x=[-1,1,index123] and y=[-1,1,index124].

;; unfortunately, x^2 and x^2 will not be correlated using this simple idea.


(in-package :maxima) ;; I guess
#|
(defpackage :ga) ;; needed for gcl to read this file


(defpackage :ri				;uses generic arithmetic
  (:use :ga :cl)
  

  (:shadowing-import-from 
   :ga
   "+" "-" "/" "*" "expt"		;binary arith
   "=" "/=" ">" "<" "<=" ">="		;binary comparisons
   "sin" "cos" "tan"			;... more trig
   "atan" "asin" "acos"			;... more inverse trig
   "sinh" "cosh" "atanh"		;... more hyperbolic
   "expt" "log" "exp" "sqrt"		;... more exponential, powers
   "1-" "1+" "abs"
   "tocl" "re-intern"
 ;;  "true" "false"			;used by simpsimp
   )
    (:shadowing-import-from 
     :cl
     "union" "intersection")
    (:export "ri" "union" "intersection"))

(defpackage :ri (:use :cl)) ;; for GCL


(in-package :ri)
|#

(defparameter *intcount* 0)
;structure for real interval includes index to make each unique
(defstruct (ri (:constructor ri (lo hi)))lo hi (index (incf *intcount*) ))
(defmethod print-object ((a ri) stream)
  (format stream "[~a,~a]"  (pbg(ri-lo a))(pbg(ri-hi a))))

#+allegro
(defun pbg(x) ;; print bad guy. This uses the mistake that infinities can be compared equal
  (if
      (badguy x)
      (case x
	((#.excl::*infinity-double*   #.excl::*infinity-single*)
	 "oo")
	((#.excl::*negative-infinity-double* #.excl::*negative-infinity-single*) 
	 "-oo")
	((#.excl::*nan-double*  #.excl::*nan-single*) 
	 "NaN")
	(otherwise x)) ;; not really a bad guy
    x))
#-allegro (defun pbg(x) x) ; go with whatever is printed.


;; must figure out ri version of sin, cos, tan, etc.
;; must figure out =, >, <, union, intersection.

;; from Graham, On Lisp, macro hackery
(defun mkstr (&rest args)
  (with-output-to-string (s)(dolist (a args) (princ a s))))

(defun symb (&rest args) (values (intern (apply #'mkstr args))))

(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
      (let ,(mapcar #'(lambda (f)
			`(,f (,(symb name f) ,gs)))
		    fields)
	,@body))));;; e.g. (with-struct (ri- lo hi) r (f lo hi))
;;; based on...
;;;from Figure 18.3: Destructuring on structures. from On Lisp, P. Graham

;; take 2 real intervals and grab their insides. Then
;; do something with them. sample usage...
;;(with-ri2 ri1 ri2 (lo1 hi1)(lo2 hi2) (ri (+ lo1 lo2)(+ hi1 h2)))
(defmacro with-ri2 (struct1 struct2 names1 names2 &body body)
  (let ((gs1 (gensym))
	(gs2 (gensym)))
    `(let ((,gs1 ,struct1)
	   (,gs2 ,struct2))
       (let ,(append 
	      (mapcar #'(lambda (f field)
			  `(,f (,(symb "RI-" field) ,gs1)))
		      names1
		      '(lo hi))
	      (mapcar #'(lambda (f field)
			  `(,f (,(symb "RI-" field) ,gs2)))
		      names2
		      '(lo hi)))
	 ,@body))))

(defmacro with-ri (struct1 names1  &body body)
  (let ((gs1 (gensym)))
    `(let ((,gs1 ,struct1))
       (let  ,(mapcar #'(lambda (f field)
			  `(,f (,(symb "RI-" field) ,gs1)))
		      names1
		      '(lo hi))
	 ,@body))))

(defmethod two-arg-+ ((r ri)(s ri))
  ;; adding 2 intervals, just add their parts.
  ;; to be more precise we should round down for lo, round up for hi.
  (with-ri2 r s (lo1 hi1)(lo2 hi2) (ri (+ lo1 lo2)(+ hi1 hi2))))

(defmethod two-arg-+ (r (s ri)) ;adding num+interval
  (with-ri s (lo1 hi1) (ri (+ lo1 r)(+ hi1 r))))

(defmethod two-arg-+ ((s ri) r)
  (with-ri s (lo1 hi1) (ri (+ lo1 r)(+ hi1 r))))

(defmethod two-arg-* ((r ri)(s ri))
  ;; multiplying intervals, try all 4, taking min and max.
  ;; to be more precise we should round down for lo, round up for hi.
  ;; could be done faster, e.g. if intervals are 0<lo<hi.
  (with-ri2 r s (lo1 hi1)(lo2 hi2)
	    (let ((prods (sort (list (* lo1 lo2)(* lo1 hi2)(* hi1 lo2)(* hi1 hi2)) #'<)))
	      (ri (car prods)(fourth prods)))))

(defmethod two-arg-* (r (s ri))
  ;; multiplying num by interval
  (with-ri s (lo1 hi1)
	   (let ((prods (sort (list (* r lo1)(* r hi1)) #'<)))
	      (ri (car prods)(cadr prods)))))


(defmethod two-arg-* ((s ri) r) (two-arg-* r s))

;; need to do more stuff for sin cos etc.
;; need to think about rounding up/down
;; can be much more careful with some of these.

#+ignore ;; see later
(defmethod cos ((s ri))
  (with-ri  s (lo hi)
	    (if (>(abs (- hi lo)) pi) (ri -1 1) ; full period
	      (error "please fix program for interval cos ~s" s))))

(defmethod two-arg-/ ((r ri)(s ri))
  ;; dividing intervals, try all 4, taking min and max.
  ;; for floats we should round down for lo, round up for hi.
  ;; could be done faster, e.g. if intervals are 0<lo<hi.
  (with-ri2 r s (lo1 hi1)(lo2 hi2)
	    (if (<= lo2 0 hi2)		; divisor contains zero 
		(error "division by interval containing zero ~s" s)
	    (let ((quos (sort (list (/ lo1 lo2)(/ lo1 hi2)(/ hi1 lo2)(/ hi1 hi2)) #'<)))
	      (ri (car quos)(fourth quos))))))

(defmethod two-arg-- ((r ri)(s ri))
  ;; subtracting intervals, try all 4, taking min and max.
  ;; for floats we should round down for lo, round up for hi.
  ;; could be done faster, e.g. if intervals are 0<lo<hi.
  (with-ri2 r s (lo1 hi1)(lo2 hi2)
	    (let ((diffs (sort (list (- lo1 lo2)(- lo1 hi2)(- hi1 lo2)(- hi1 hi2)) #'<)))
	      (ri (car diffs)(fourth diffs)))))

;; how to refine an interval..
;; find the minimum and maximum of f on the interval. Or approximate them.

(defmethod mini(f (r ri) (c integer)) ;; was fixnum?
  (with-ri r (lo hi)
	   (mini2 f lo hi (funcall f r) c)))

(defun mini2(f a b val count)
  (if (<= count 0) (ri-lo val)
  (let*
      ((m (/(+ a b) 2))
       (f1 (funcall f (ri a m)))
       (f2 (funcall f (ri m b)))
       (mf1 (ri-lo f1))
       (mf2 (ri-lo f2))
       (ans 0))
    (cond ((< mf1 mf2)
	   (setf ans (mini2 f a m f1 (1- count)))
	   (if (< ans mf2) ans (min ans (mini2 f m b f2 (1- count)))))
	  (t
	   (setf ans (mini2 f  m b f2 (1- count)))
	   (if (< ans mf1) ans (min ans (mini2 f a m f1 (1- count)))))))))

	   
;;; short version of maxi.
#+ignore
(defmethod maxi(f (r ri) (c fixnum))
  (with-ri r (lo hi)
	  (- (mini2 #'(lambda(r)(- (funcall f r)))
		  lo hi (funcall f r) c))))
       

;;; longer version, saves some function calls and negations.
(defmethod intmaxi(f (r ri) (c integer)) ;was fixnum
  (with-ri r (lo hi)
	   (maxi2 f lo hi (funcall f r) c)))

(defun maxi2(f a b val count)
  (if (<= count 0) (ri-hi val)
  (let*
      ((m (/(+ a b) 2))
       (f1 (funcall f (ri a m)))
       (f2 (funcall f (ri m b)))
       (mf1 (ri-hi f1))
       (mf2 (ri-hi f2))
       (ans 0))
    
    (cond ((> mf1 mf2)
	   (setf ans (maxi2 f a m f1 (1- count)))
	   (if (> ans mf2) ans (max ans (maxi2 f m b f2 (1- count)))))
	  (t
	   (setf ans (maxi2 f  m b f2 (1- count)))
	   (if (> ans mf1) ans (max ans (mini2 f a m f1 (1- count)))))))))

;; uses memoizing, in case f is hard to evaluate.
(defmethod refine-mem(f (r ri) (c integer))
  (labels
      ((lo-hi(r)(setf r (car r))	;(ma::ucons (ri-lo r)(ri-hi r))
	     (cons (ri-lo r)(ri-hi r))
	     )
       (memoize(fn-name &key (key #'lo-hi) (test #'eq))
	 ;;(amlparser::clear-memoize fn-name)
	 (clear-memoize fn-name)
	 (setf (symbol-function fn-name)
	       #+ignore  (amlparser::memo (symbol-function fn-name)
				  :name fn-name :key key :test test)
		    (memo (symbol-function fn-name)
              :name fn-name :key key :test test))))
    (memoize f :key #'lo-hi :test #'eq)
    (ri (mini f r c)(intmaxi f r c))))

(defmethod intrefine(f (r ri) (c integer)) ;; don't memoize.  May be faster, often.
  
  ;; should check that r is a proper interval,
  ;; here's one way.
   (let* ((lo (ri-lo r))(hi (ri-hi r))(m (/ (+ lo hi) 2)))
     (unless (< lo m hi) (format t "~%cannot use intrefine on ~s"ri) (funcall r ri))

    (ri (mini f r c)(intmaxi f r c))))


;;; a better interval sin routine...
#+allegro 
(defun badguy(x) ;; this returns non-nil for single or double nans and infinities
  (excl::exceptional-floating-point-number-p x))

#-allegro
(defun badguy(x) ;; this returns non-nil for single or double nans and infinities
 nil) ;; this requires research for GCL etc.

(defmethod left ((r ri))(ri-lo r))
(defmethod left ((r t)) r)
(defmethod right((r ri)) (ri-hi r))
(defmethod right((r t)) r)

(defconstant m1to1 (ri -1 1)) ;; interval from minus 1 to 1
#+allegro(defconstant empty-ri (ri #.excl::*nan-double* #.excl::*nan-double*));; empty ri
(defconstant piby2 (/ pi 2))
(defun intsin(z)
"real interval sin of an interval of machine floats."
;; result endpoints are same precision as inputs, generally, except if
;; we note that extrema -1, 1 are reached, in which case they may be integers
 (let ((low (left z))
       (hi  (right z)))
   (cond 
    
    ;;here: insert more code to do some checking to make sure l and
    ;; r are proper floats, not too large and if they are OK, also check
    ;; to see if it is an external interval. low<=high

    ((or (badguy low)(badguy hi)(> low hi)) m1to1)
    ((eql low hi)(sin low)) 
    ;; return a non-interval?? or maybe should widen interval a little?
    (t(let (u v min max
	    (l (ceiling low piby2))
	    (h (floor hi piby2)))
	(cond ((>= (- h l) 4) (return-from intsin m1to1)))
	(setf u (sin low))
	(setf v (sin hi))
	(setf minval (max -1 (bd(min u v)))) ;lower value. should round down, not below -1
	(setf maxval (min 1 (bu (max u v)))) ;upper value. should round up, not over 1
	(do 
	 ((k  l (1+ k)))
	 ((> k h)(ri minval maxval))
	 (case (mod k 4)
	       (1 (setf maxval 1))
	       (3 (setf minval -1)))))))))

;; here are tests: (intsin (ri 0 pi))
;;                 (intsin (ri pi (* 2 pi))
;;                 (intsin (ri (- pi) pi))
;;                 (intsin (ri -1.0 1.0))
;;                 (intsin (ri -1.0d0 1.0d0))


;;(defmethod sin((z ri)) (intsin z))

(defun bu (k) (if (rationalp k) k
		(if (> k 0)(* k #.(+ 1 long-float-epsilon))
		  (* k #.(- 1 long-float-epsilon)))))


(defun bd (k) (if (rationalp k) k (if (< k 0)(* k #.(+ 1 long-float-epsilon))
				   (* k #.(- 1 long-float-epsilon)))))
#|

GCL can't handle defmethod with built-in types, I guess.  12/18/07 RJF
(defmethod bu ((k long-float)) (if (> k 0)(* k #.(+ 1 long-float-epsilon))

(defmethod bu ((k single-float)) (if (> k 0)(* k #.(+ 1 single-float-epsilon))
				   (* k #.(- 1 single-float-epsilon))))
;(defmethod bu ((k (eql 0))) 0) ;exactly 0 stays zero same as rationals
(defmethod bu ((k rational)) k)
(defmethod bu ((k (eql 0.0))) least-positive-normalized-single-float)
(defmethod bu ((k (eql 0.0d0)))least-positive-normalized-long-float)
(defmethod bd ((k (eql 0.0))) least-negative-normalized-single-float)
(defmethod bd ((k (eql 0.0d0))) least-negative-normalized-long-float)

(defmethod bd ((k long-float)) (if (< k 0)(* k #.(+ 1 long-float-epsilon))
				 (* k #.(- 1 long-float-epsilon))))
(defmethod bd ((k single-float)) (if (< k 0)(* k #.(+ 1 single-float-epsilon))
				   (* k #.(- 1 single-float-epsilon))))
(defmethod bd ((k rational)) k)

|#

(defun widen-ri (a b) (ri (bd a)(bu b))) 


;;; these methods all need to be examined for nan and inf  and empty etc args.
(defmethod includes-r ((r ri)(s ri)) ;; r is inside s
  (with-ri2 r s (lo1 hi1)(lo2 hi2)
	    (<= lo1 lo2 hi2 hi1)))

(defmethod intersect-ri ((r ri)(s ri)) ;; 
  ;; if the intersection is empty, what interval should we return??
  (with-ri2 r s (lo1 hi1)(lo2 hi2)  ;; what to do
	    ))


(defmethod two-arg-< ((r ri)(s ri))
  (< (ri-hi r) (ri-lo s)))


;;; acl bug?
;;(< excl::*infinity-double* excl::*nan-double* )


#| a Maxima interpreter for intervals |#

;;(defpackage :maxima)
;;(in-package :ri)			; real interval

(defun maxima::$IEVAL(expr)(ieval (meval expr)))

(defparameter maxima::$autowiden nil)


(defun ieval(e)
  
  (cond ((realp e) (widen-ri e e))	;3.0 -> [3-eps,3+eps]
	((ri-p e) e)			; a real interval already
	((atom e) ;(eval e)... or...
		  (error "IEVAL cannot handle ~s" e)
		  )
	(t (case 
	       (caar e) 
	     (mplus
	      (cond ((null (cdr e)) 0)
		    ((null (cddr e))(ieval (cadr e)))
		    ;; normally we would use +, via defmethod. GCL makes this hard
		    (t  (two-arg-+ (ieval (cadr e));this + is a real-interval +
			   (ieval (cons (car e) (cddr e)))))))
	     (mtimes
	      (cond ((null (cdr e)) 0)
		    ((null (cddr e))(ieval (cadr e)))
		    (t  (two-arg-* (ieval (cadr e)); this * is a real-interval *
				   (ieval (cons (car e) (cddr e)))))))
	     (rat
	        (setf e (/ (cadr e)(caddr e)))(ri e e)) ; e.g maxima ((rat simp) 1 2)

	   
	     (otherwise
	      ;; look on property list of (car e) 
	      ;; to see if it has an ieval property. eg.
	      (if (setf fn (get (caar e) 
				'ieval))
		  (apply fn (mapcar #'ieval (cdr e)))
		(error "IEVAL cannot handle ~s" e))))
	   )))

;; for maxima, we would have $min, $max, etc.
;(setf (get 'includes 'ieval) #'include-ri)
(setf (get 'maxima::$intersects 'ieval) #'intersect-ri)
(setf (get 'maxima::$intmin 'ieval) #'(lambda(x)(ri-lo x))) ;; not going to work if called min
(setf (get 'maxima::$intmax 'ieval) #'(lambda(x)(ri-hi x))) ;; not going to work if called max
(setf (get 'maxima::mlist 'ieval) 'maxima::$interval)
(setf (get 'maxima::mexpt 'ieval) 'two-arg-expt)
(setf (get 'maxima::$interval 'ieval)  'maxima::$interval)
(setf (get 'maxima::%sin 'ieval) #'intsin) ;; etc etc for log, exp, power, 

;; hm, I'm using CL rationals, not ((rat simp) 1 2) internal to intervals.
;; this could be trouble, if there is user access to lo and hi, need to put back rat etc.
;(defun maxima::$RealInterval(a b)(ri a b)) ;; constructor

;; etc

(defun maxima::$interval(a b)
       (cond ((and (realp a)
		 (realp b))  ;; optional? (<= a b)
	      (ri a b))
	     ((and (ri-p a)(ri-p b))
	      (ri (ri-lo a) (ri-hi b)))
	     (t	  (error "can't convert interval(~s,~s) to interval" a b ))))

    
(defmethod two-arg-expt((r ri)(s ri))

  (with-ri2 r s (lo1 hi1)(lo2 hi2)  ;; what to do

	    (if (= lo2 hi2) ;; case of non-interval power
		(if (< 0 lo1 hi1)
		    (ri (expt lo1 lo2) (expt hi1 lo2))
		  (let ((vals (sort (list 0 (expt lo1 lo2)(expt hi1 lo2)) #'<)))
		    (ri (car vals)(caddr vals))))

	      (format nil "~s^~s" r s)) ;; later
	    ))


(defun form-ri(form) ;; to display intervals as [low,high]
  (format nil "[~a,~a]" (ri-lo form)(ri-hi form)))

(setf (get 'ri 'format-prog) #'form-ri)


;; add certainly_less, includes, etc
;; overlap, 
;; add more trig stuff.
;; 


;(load "c:/lisp/generic/nnformat.lisp") ;; must also be loaded in







