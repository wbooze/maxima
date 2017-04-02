;;; -*- Mode:Common-Lisp; Package:ri; Base:10 -*-

;;  A basis for interval arithmetic
;;  constructed by overloading generic arithmetic. 
;;  Richard Fateman, November, 2005
;;  REVISED. June 2006 RJF

;; NEW PART:  a third slot that provides info on dependence.

;; 6/23/06 RJF
;; we can already make (*  h h)  just as good as (expt h 2) by saving dependences,
;; using polynomial arithmetic in the fourth slot.

;; We can make  x=[0,1],  then x*(1-x) come out [0, 1/4].
;; [evaluating quadratics by completing the square]
;;  Challenge: Can we make
;; [1,2]*([-3,4] + [3,4]) = [0,8],  ie

;;; w*(g+h)

;;  and  the distributed version,
;; (+ (* w g) (* w h)) = [-3, 16 ]
;; come out the same?

;; need to compute and store w*g+w*h as (g+h)*w. 
;; i.e. canonical form ( 1*g^1+(1*h^1 + 0*h^0)*g^0)*w^1 + 0*w^0...

;; easily done in polynomial
;; arithmetic. Though canonical polynomial form might equally end up
;; as (1*w^1 + 0*w^0)*g^1  ((1*w^1 + 0*w^0)*h^1 +0*h^0)*g^0
;; which is no better. Need to find a form that has the fewest
;; repeated occurrences of a variable. Calculation of an Single Use
;; expression, perhaps by heuristics.

;;; Another version of interval arithmetic that seems to work especially
;;; well in geometric / graphical applications is called Affine Arithmetic;
;;; in this model each number x is expressed as x0 +x1*e1 + ...+xn*en where
;;; the xi are all scalars and the e1 are "dummy variables" each of which can
;;; assume any value in [-1,1].  The advantage for graphics is that if the
;;; X and Y coordinates of an object are specified with overlapping dummy vars,
;;; then the hull of points in the 2-D interval is not a rectangle but a
;;; zonotope or lozenge-shaped object, smaller than the rectangular hull.

;;; This model could also be used, with a modest amount of programming.

;;; ri creates a real interval;  wri creates a proper "widened" interval.
;;; if either endpoint is rational, it is assumed exact and not in need of
;;; widening.  otherwise it is bumped up/down as appropriate.  I don't
;;; use wri here, but maybe I should.

(defpackage :ri				;uses generic arithmetic for endpoints
  (:use :ga :cl)
(:shadowing-import-from  
   :ga    
   + - / * 
   = /= > < <= >=	
   sin cos tan
   atan asin acos atan2
   sinh cosh tanh
   asinh acosh atanh
   expt log exp sqrt	   
   min max
   1- 1+ abs incf decf
   numerator denominator	
   tocl re-intern 
 ;;  true false			;used by simpsimp
   )
    (:shadowing-import-from 
     :cl
     union intersection)
  (:export ri union intersection))

(eval-when (compile eval load)(require "ga") (provide "ri") (require "poly")(in-package :ri))

(in-package :ri)
(eval-when (compile eval load)	   
(defstruct ri lo hi exp )		;structure for real interval

;; index refers to place of origin of this number.
(defvar intindex)
(defvar intervalhash (make-hash-table :weak-keys t))

(defun genindex (lo hi)(let ((n (gensym)))
			 (setf (gethash n intervalhash)(cons lo hi))
			 n))

(defun ri(lo hi &key  exp)
 ;; (format t "~% lo=~s, hi=~s, index=~s exp=~s" lo hi index exp)
  (cond ((< hi lo)(error "attempt to create invalid real interval [~s, ~s]" lo hi))
	((null exp)
	 (setf exp (vector (genindex lo hi) 0 1)))) ;; exp is  0*x^0 + 1*x^1
  (make-ri :lo lo :hi hi :exp exp)))


(defun wri(lo hi &key  exp )  ;; widen on creation, if endpoints are floats.
 ;; (format t "~% lo=~s, hi=~s, index=~s exp=~s" lo hi index exp)
  (cond ((< hi lo)(error "attempt to create invalid real interval [~s, ~s]" lo hi))
	((or (null index)(null exp))
	 (setf lo (bd lo))		;bump down
	 (setf hi (bu hi))		;bump up
	 (setf exp (vector (genindex lo hi) 0 1)))) ;; exp is  0*x^0 + 1*x^1
  (make-ri :lo lo :hi hi :exp exp))

(defun into(lo hi)(ri lo hi))  ;; standard name would be ri::into

(defmethod print-object ((a ri) stream)
  (format stream "[~a,~a]"  (pbg(ri-lo a))(pbg(ri-hi a))))

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

;; The code for sin is given
;; must figure out ri version of  cos, tan, etc.
;; must figure out =, >, <, union, intersection.


;; from Graham, On Lisp, macro hackery

(defun mkstr (&rest args)
  (with-output-to-string (s)(dolist (a args) (princ a s))))

(defun symb (&rest args) (values (intern (apply #'mkstr args))))

#+ignore
(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
      (let ,(mapcar #'(lambda (f)
			`(,f (,(symb name f) ,gs)))
		    fields)
	,@body))))
 ;;; e.g. (with-struct (ri- lo hi) r (f lo hi))
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
			  `(,f (,(symb "ri-" field) ,gs1)))
		      names1
		      '(lo hi  exp))
	      (mapcar #'(lambda (f field)
			  `(,f (,(symb "ri-" field) ,gs2)))
		      names2
		      '(lo hi exp)))
	 ,@body))))

(defmacro with-ri (struct1 names1  &body body)
  (let ((gs1 (gensym)))
    `(let ((,gs1 ,struct1))
       (let  ,(mapcar #'(lambda (f field)
			  `(,f (,(symb "ri-" field) ,gs1)))
		      names1
		      '(lo hi  exp))
	 ,@body))))

(defmethod ga::two-arg-+ (r (s ri::ri)) ;adding num+interval
  (with-ri s (lo1 hi1  exp) 
	   (ri (+ lo1 r)(+ hi1 r) :exp (ma::p+cv r exp))))

(defmethod ga::two-arg-+ ((s ri::ri) r) ;adding interval + num
  (with-ri s (lo1 hi1 exp) 
	   (ri (+ lo1 r)(+ hi1 r) :exp (ma::p+cv r exp))))

(defmethod ga::two-arg-* ((s ri::ri) r) ;multiply interval * num
  (if (>= r 0)
      (with-ri s (lo1 hi1 exp) 
	       (ri (* lo1 r)(* hi1 r) :exp (ma::p*cv r exp)))
    (with-ri s (lo1 hi1  exp) 
	     (ri (* hi1 r) (* lo1 r) :exp (ma::p*cv r exp)))))

(defmethod ga::two-arg-* ( r(s ri::ri)) (ga::two-arg-* s r)) ;reverse the args

(defvar intindex 0)

;; get the index variable out of an expression in an interval

(defun get-int-index(r &aux temp)(and (vectorp (setf temp (ri-exp r)))(aref temp 0)))

;;new
(defmethod ga::two-arg-+ ((ri1 ri)(ri2 ri))
  (with-ri2 ri1 ri2 (lo1 hi1 exp1)(lo2 hi2 exp2)
	    (if (cl::eq (get-int-index ri1)
			(get-int-index ri2))
		(let			; add as polynomials
		    ((orig (gethash (get-int-index r) intervalhash))) ; get the orig value
		  (dumbevalint (ma::p+ exp1 exp2) (car orig)(cdr orig))) ;; how we combine matching index exps
		      (let ((lo (+ lo1 lo2))
			    (hi (+ hi1 hi2)))
			(ri lo hi))))) ;make a new index

;;; new
(defmethod ga::two-arg-* ((r ri)(s ri))
  ;; to be more precise we should round down for lo, round up for hi.
  (with-ri2 r s (lo1 hi1 exp1)(lo2 hi2 exp2)
	    (if (cl::eq (get-int-index r)
			(get-int-index s))
		(let  ;  multiply as polynomials
		       ((orig (gethash (get-int-index r) intervalhash))) ; get the orig value
		  (dumbevalint (ma::p* exp1 exp2) (car orig)(cdr orig)))
	      
	      ;; un-matched index
	      (let ((prods (list (* lo1 lo2)(* lo1 hi2)(* hi1 lo2)(* hi1 hi2))))
		(ri (reduce #'ga::two-arg-min prods)
		    (reduce #'ga::two-arg-max prods)
					;generate new index
		    )))))

(defmethod ga::two-arg-expt ((s ri) (r fixnum)) 
  
  (with-ri s (lo hi exp)
	   (let ((pows (list (expt lo r)(expt hi r))))
		(ri (reduce #'ga::two-arg-min pows)
		    (reduce #'ga::two-arg-max pows)
		    :exp (ma::p^ exp r)))))

;; need to do more stuff for sin cos etc.
;; need to think about rounding up/down
;; can be much more careful with some of these.

(defmethod cos ((s ri))
  (with-ri  s (lo hi)
	    (if (>(abs (- hi lo)) pi) (ri -1 1) ; full period
	      (error "please fix program for interval cos ~s" s))))

;; old...
#+ignore 
(defmethod ga::two-arg-/ ((r ri)(s ri))
  ;; dividing intervals, try all 4, taking min and max.
  ;; for floats we should round down for lo, round up for hi.
  ;; could be done faster, e.g. if intervals are 0<lo<hi.
  (with-ri2 r s (lo1 hi1)(lo2 hi2)
	    (if (<= lo2 0 hi2)		; divisor contains zero 
		(error "division by interval containing zero ~s" s)
	    (let ((quos (list (/ lo1 lo2)(/ lo1 hi2)(/ hi1 lo2)(/ hi1 hi2))))
	      (ri (reduce #'min quos)
		  (reduce #'max quos))))))

;; new
(defmethod ga::two-arg-/ ((r ri)(s ri))
  ;; dividing intervals, try all 4, taking min and max.
  ;; for floats we should round down for lo, round up for hi.
  ;; could be done faster, e.g. if intervals are 0<lo<hi.
  ;; generate a new index
  (with-ri2 r s (lo1 hi1)(lo2 hi2)
	    (if (<= lo2 0 hi2)		; divisor contains zero 
		(error "division by interval containing zero ~s" s)
	    (let ((quos (list (/ lo1 lo2)(/ lo1 hi2)(/ hi1 lo2)(/ hi1 hi2))))
	      (ri (reduce #'two-arg-min quos)
		  (reduce #'two-arg-max quos))))))

(defmethod ga::two-arg-/ ((r ri) s)
  (* (/ 1 s) r))

(defmethod ga::two-arg-/ (s (r ri)) (* s (invert r)))

(defmethod invert ((r ri))
     (with-ri r (lo2 hi2)
	    (if (<= lo2 0 hi2)		; divisor contains zero 
		(error "division by interval containing zero ~s" r)
	      (let ((e1 (/ 1 lo2))
		    (e2 (/ 1 hi2)))
	      (if (< e1 e2)(ri e1 e2)(ri e2 e1))))))

(defmethod negate((r ri))
  (ri (- (ri-hi r))(- (ri-lo r)) :exp (ma::p* -1 (ri-exp r))))

(defmethod ga::two-arg-- (r (s ri))   (+ r (negate s)))
(defmethod ga::two-arg-- ((s ri) r)   (+ s (- r)))

(defun two-arg-min(a b)(if (< a b) a b))
(defun two-arg-max(a b)(if (> a b) a b))


;; how to refine an interval..
;; find the minimum and maximum of f on the interval. Or approximate them.

(defmethod mini(f (r ri) (c fixnum))
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
(defmethod maxi(f (r ri) (c fixnum))
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
(defmethod refine-mem(f (r ri) (c fixnum))
  (labels
      ((lo-hi(r)(setf r (car r))(ma::ucons (ri-lo r)(ri-hi r)))
       (memoize(fn-name &key (key #'lo-hi) (test #'eq))
	 (amlparser::clear-memoize fn-name)
	 (setf (symbol-function fn-name)
	         (amlparser::memo (symbol-function fn-name)
              :name fn-name :key key :test test))))
    (memoize f :key #'lo-hi :test #'eq)
    (ri (mini f r c)(maxi f r c))))

(defmethod refine(f (r ri) (c fixnum)) ;; don't memoize.  May be faster, often.
  
  ;; should check that r is a proper interval,
  ;; here's one way.
   (let* ((lo (ri-lo r))(hi (ri-hi r))(m (/ (+ lo hi) 2)))
     (unless (< lo m hi) (format t "~%cannot refine on ~s" r) (funcall f r))
    (ri (mini f r c)(maxi f r c))))


(defun badguy(x) ;; this returns non-nil for single or double nans and infinities
  (excl::exceptional-floating-point-number-p x))

;; before doing any comparisons, maybe we have to check that none of the
;; comparands are badguys.

(defmethod left ((r ri))(ri-lo r))
(defmethod left ((r t)) r)
(defmethod right((r ri)) (ri-hi r))
(defmethod right((r t)) r)

(defparameter empty-ri (ri #.excl::*nan-double* #.excl::*nan-double*));; empty ri
(defparameter piby2 (/ pi 2))

(defun intsin(z)
  "real interval sin of a real interval of machine floats."
  ;; result endpoints are same precision as inputs, generally, except if
  ;; we note that extrema -1, 1 are reached, in which case they may be integers
  (let ((low (left z))
	(hi  (right z)))
    (cond 
    
     ;;here: insert more code to do some checking to make sure low and
     ;; hi are proper floats, not too large and if they are OK, also check
     ;; to see if it is an external interval. low<=high

     ((or (badguy low)(badguy hi)(> low hi))(ri -1 1))
     ((eql low hi)(sin low)) 
     ;; return a non-interval?? or maybe should widen interval a little?
     (t(let (u v minval maxval
	     (l (ceiling low piby2))
	     (h (floor hi piby2)))
	 (cond ((>= (- h l) 4) (return-from intsin (ri -1 1))))
	 (setf u (sin low))
	 (setf v (sin hi))
	 (setf minval (min u v))	;lower value. should round down
	 (setf maxval (max u v))	;upper value. should round up
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


(defmethod ga::sin((z ri)) (intsin z))

;; bump up, bump down.
(defmethod bu ((k double-float)) (if (> k 0)(* k #.(+ 1 double-float-epsilon))
				   (* k #.(- 1 double-float-epsilon))))

(defmethod bu ((k single-float)) (if (> k 0)(* k #.(+ 1 single-float-epsilon))
				   (* k #.(- 1 single-float-epsilon))))
;(defmethod bu ((k (eql 0))) 0) ;exactly 0 stays zero same as rationals
(defmethod bu ((k rational)) k)
(defmethod bu ((k (eql 0.0))) least-positive-normalized-single-float)
(defmethod bu ((k (eql 0.0d0)))least-positive-normalized-double-float)
(defmethod bd ((k (eql 0.0))) least-negative-normalized-single-float)
(defmethod bd ((k (eql 0.0d0))) least-negative-normalized-double-float)

(defmethod bd ((k double-float)) (if (< k 0)(* k #.(+ 1 double-float-epsilon))
				   (* k #.(- 1 double-float-epsilon))))
(defmethod bd ((k single-float)) (if (< k 0)(* k #.(+ 1 single-float-epsilon))
				   (* k #.(- 1 single-float-epsilon))))
(defmethod bd ((k rational)) k)

(defmethod widen-ri ((r ri)) (ri (bd (ri-lo r))(bu (ri-hi r))))


;;; these methods all need to be examined for nan and inf  and empty etc args.
(defmethod includes ((r ri)(s ri)) ;; s is inside r
  (with-ri2 r s (lo1 hi1)(lo2 hi2)
	    (<= lo1 lo2 hi2 hi1)))

#+ignore
(defmethod intersect-ri ((r ri)(s ri)) ;; 
  ;; if the intersection is empty, what interval should we return??
  (with-ri2 r s (lo1 hi1)(lo2 hi2)  ;; what to do
	    ))


(defmethod ga::two-arg-< ((r ri)(s ri))
  (< (ri-hi r) (ri-lo s)))


;;; acl bug?
;;(< excl::*infinity-double* excl::*nan-double* )

;; not used in this file
(defun horner(poly pt);; poly looks like #(x 3 0 1)  for x^2+3
  (let ((L (1- (length poly)))
	(sum 0))
    (do ((i L (1- i)))
	((cl::= i 0) sum)
      (setf sum(+ (aref poly i)(* pt sum))))))

;; eval dumb way, except if pt is an interval.

(defun dumbeval(poly pt);; poly looks like #(x 3 0 1)  for x^2+3
  (let ((L (1- (length poly)))
	(sum 0))
    (do ((i 1 (1+ i)))
	((cl::> i L) sum)
      (setf sum (+ sum (* (expt pt (1- i)) (aref poly i)))))))


(defun quadeval(poly lo hi)  ;; poly must be QUADRATIC, e.g. #(x c b a)
  ;; we use completing the square to make the evaluation use x a SINGLE time.
  ;; a*x^2+b*x+c =   a* (x^2+(b/a)*x* +c/a)
  ;; (x^2+r*x +s) =  (x + r/2)^2 -(r/2)^2 +s.
  (assert (= (length poly) 4))
  (let* ((a (aref poly 3))
	 (b (aref poly 2))
	 (rby2 (/ b (* 2 a)))
	 (s (/ (aref poly 1) a))
	 (rest (+ s (* -1 (* rby2 rby2))))
	 (h1 (two-arg-max 0 (+ lo rby2)))
	 (e1  (* a (+ rest (* h1 h1))))
	 (h2  (two-arg-max 0(+ hi  rby2)))
  	 (e2  (* a  (+ rest (* h2 h2)))))
    
    ;;    (format t "~% h1=~s, h2=~s" h1 h2)

    (if (< e1 e2)(ri e1 e2)(ri e2 e1))
    ))




;; inefficient version, and not tight either. works perfectly
;; for pure powers though.

(defun dumbevalint(poly lo hi);; poly looks like #(x 3 0 1)  for x^2+3
  (cond (;;(numberp poly) (ri poly poly)) ; constants look like 34
	 (numberp poly) poly) ; constants look like 34
	((= (length poly) 4)(quadeval poly lo hi))
	(t
	 (setf poly (subseq poly 1))	; chop off the variable
	 (let ((L (length poly))
	       (sum (list 0 0))
	       (x2lo 0)(x2hi 0))	; x^2 low and high
	   (if ( < (* lo hi) 0) (setf x2lo 0 x2hi (max (* lo lo)(* hi hi)))
	     (setf x2lo (sort(list (* lo lo)(* hi hi)) #'<)
		   x2hi (cadr x2lo) x2lo (car x2lo)))
	   ;;  (format t "~%x2 = ~s ~s " x2lo x2hi)
	   (do ((i 0 (1+ i)))
	       ((cl::= i L) sum)
	     (setf sum (mapcar #'cl::+ 
			       sum 
			       (mapcar #'(lambda(r)
					   (cl::*  (aref poly i) r))
				       (if (evenp i)
					   (list  (cl::expt x2lo (ash i -1))
						  (cl::expt x2hi (ash i -1)))
		

			 (sort (list (cl::expt lo i)(cl::expt hi i)) #'<))))))))))

;;smartevalint would take a univariate polynomial p(x) and an interval, [lo,hi]
;; eturn the min and max of that polynomial on that interval, [min,max]
;; algorithm:  compute derivative, d=p'(x).  Find bounding intervals for
;; each of the real zeros of d,  {r1,r2,...,rn}.   For each of these
;; (small) intervals, compute p(r1), p(r2), ... p(rn)  as rigorous intervals.
;; Find the minimum of the lower bounds of these intervals and the lower bound, rounded down
;; of p([lo,lo]), p([hi,hi]). That is the min to return.

;; Find the maximum the upper bounds of these intervals and the upper bound, rounded up
;; of p([lo,lo]), p([hi,hi]). That is the max to return.


#| try this
:ld mysys-int
:ld poly
:ld ninterval

:pa :ri

(setf h (ri -1 1) g (ri -1 1) j (ri -2 2))

(* h h)
(describe *)
(* g h)
(describe *)
(* (* j j)(* j j))
(describe *)
|#

