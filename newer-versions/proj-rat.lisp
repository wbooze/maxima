;;  A basis for rational arithmetic with infinities
;;  constructed by overloading generic arithmetic. 
;;  Richard Fateman, November, 2005
;;
;; this is the PROJECTIVE model that has only ONE INFINITY.
;; Think of the numbers on a big wheel with 0 at the top, infinity
;; at the bottom.  Or since 0 has no sign, 1/0 has no sign.

;; It is kind of unfair to equate these rational numbers to their
;; corresponding names/concepts in floating point.  The float infinity
;; is used for numbers that are "too large" or overflow.  The rational
;; infinity is a limit.  The NaN (not a number) floating point name is
;; a misfit in the rational model, because in a symbolic system like
;; Lisp, MOST objects are not numbers. Calling the 0/0 undefined is
;; not fair either, because it is defined to some extent: (at least we
;; know it is not the same as some particular number, say 43). It is
;; indeterminate, perhaps.  But as a consideration for analogical
;; reasoning, we will use the name NaN, but let's let it stand for
;; something else. iNdetermiNAte.   I know that's NNA, but to
;; a dyslexic, ohw serac?

(defpackage :proj				;uses generic arithmetic
  (:use  :common-lisp :ga )
  (:shadowing-import-from 
   :ga
   "+" "-" "/" "*" "expt"		;binary arith
   "=" "/=" ">" "<" "<=" ">="		;binary comparisons
   "sin" "cos" "tan"			;... more trig
   "atan" "asin" "acos"			;... more inverse trig
   "sinh" "cosh" "atanh"		;... more hyperbolic
   "expt" "log" "exp" "sqrt"		;... more exponential, powers
   "1-" "1+" "abs"
 ;;  "tocl" "re-intern"
 ;;  "numerator" "denominator"
   )
  (:export    "proj" )
)
(eval-when (load eval) (require "ga"))				;generic arithmetic
(in-package :proj)			;package is projective rationals

(defstruct proj n)			;a projective rat. n is 0 or 1
;; the constructor for proj is just (/ a b)

(defvar NaN (make-proj :n 0))		; 0/0
(defvar inf (make-proj :n 1))		; 1/0

(defmethod print-object ((a proj) stream)  (format stream "~a/0"  (proj-n a)))

;; Non-rational operations on extended rationals are not very useful.
;; Here are some, anyway.
(defmethod ga::sin ((s proj)) NaN)
(defmethod ga::cos ((s proj)) NaN)
(defmethod ga::tan ((s proj)) NaN)
(defmethod ga::exp ((s proj)) NaN)
(defmethod ga::log ((s proj)) NaN)
(defmethod ga::two-arg-expt((s proj)(n t)) (if (zerop n) 1 s)) ;NaN^0=1
(defmethod ga::two-arg-expt((s t)(n proj)) (cond ((= s 1) 1) ;1^inf, 1^nan
						 ((= s 0) 0) ;0^inf, 0^nan
						 ;; maybe |x|<1 x^inf?
						 (t n))) ;x^inf, x^nan
(defmethod ga::atan ((s proj)) NaN) 

(defmethod ga::two-arg-+ ((r proj) (s proj)) NaN) ;checked 1/2/06
(defmethod ga::two-arg-+ ((r proj) (s t))  r) ; +-inf*norm, und*norm; 1/2/06
(defmethod ga::two-arg-+ ((s t) (r proj))  r)

(defmethod ga::two-arg-* ((r proj) (s proj))  
  (make-proj :n (* (proj-n r)(proj-n s)))) ;checked 1/2/06
(defmethod ga::two-arg-* ((r proj) (s t))  r) ; +-inf*norm, und*norm ;checked 1/2/06
(defmethod ga::two-arg-* ((s t) (r proj))  r)

(defmethod ga::two-arg-/ ((r rational) (s rational))  
  ;;norm/norm. check for norm/0 . This is how we construct INF and UND
  ;; for affine, look for sign of r
  (if(zerop s) (make-proj :n (if (zerop r) 0 1))(cl::/ r s)))

;;More could be done but IEEE float is affine not projective.
;;To do IEEE floats justice we need +inf -inf and signed zeros.
;;These are in the file affine-rat. The names for nan
;;are not standardized. These are the names for allegro cl.
;; At this time (1/5/06) the affine file has more details . RJF

#+allegro
(defmethod ga::two-arg-/ ((r number) (s (eql 0)))   ;; e.g. floats.
   (cond ((zerop r) #.excl::*nan-double*)  ;0/0
	 ((excl::nan-p r)  #.excl::*nan-double*) ; nan/0
	 (t #.excl::*infinity-double*)))

(defmethod ga::two-arg-/ ((r t) (s proj)) ; norm/inf=0 or norm/und ;checked 1/2/06
  (if (= 0 (proj-n s)) NaN 0))

(defmethod ga::two-arg-/ ((r proj) (s t)) ; inf/ norm, und/norm
  (if (= 0 s) NaN r))

;(defmethod numerator((s proj))(proj-n s))
;(defmethod numerator((s t))(cl::numerator s))
;(defmethod denominator((s proj))0)
;(defmethod denominator((s t))(cl::denominator s))

(defmacro defcomparison (op)
  (let ((two-arg (intern (concatenate 'string "two-arg-" 
				      (symbol-name op))    :ga )))
    `(progn ;; very few compares work. Just notequal. See below
       (defmethod ,two-arg ((arg1 proj) (arg2 number)) nil)
       (defmethod ,two-arg ((arg1 number) (arg2 proj)) nil)
       (defmethod ,two-arg ((arg1 proj) (arg2 proj)) nil)
      (compile ',two-arg)
      ',op)))
		      
(defcomparison >)
(defcomparison =)
(defcomparison <)
(defcomparison <=)
(defcomparison >=)

;; No regular OR extended number is equal to  1/0 or 0/0.
(defmethod ga::two-arg-/= ((arg1 proj) (arg2 number)) t)
(defmethod ga::two-arg-/= ((arg1 number) (arg2 proj)) t)
(defmethod ga::two-arg-/= ((arg1 proj) (arg2 proj)) t)

