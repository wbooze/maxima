;;  A basis for rational arithmetic with infinities
;;  constructed by overloading generic arithmetic. 
;;  Richard Fateman, November, 2005
;;
;; this is the AFFINE model

;; There are 2 infinities 1/0  and -1/0  and 2 zeros:  0,  0/-1, as well as 0/0.

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

(defpackage :affine				;uses generic arithmetic
  (:use  :cl )
  (:shadowing-import-from 
   :ga
   "+" "-" "/" "*" "expt"		;binary arith
   "=" "/=" ">" "<" "<=" ">="		;binary comparisons
   "sin" "cos" "tan"			;... more trig
   "atan" "asin" "acos"			;... more inverse trig
   "sinh" "cosh" "atanh"		;... more hyperbolic
   "expt" "log" "exp" "sqrt"		;... more exponential, powers
  ;; "1-" "1+" "abs"  ;; maybe need these too
  ;; "numerator" "denominator"
   "two-arg-/"
  ;; "two-arg-/="
   )
  ;(:shadow "numerator" "denominator")
  (:export    "aff" )
)
(eval-when (load eval)(require "ga"))				;generic arithmetic
(in-package :affine)			;package is projective rationals

(defstruct aff n d)			;an affine rat. n is 0 or 1
;; the constructor for aff is just (/ a b)

(defconstant NaN (make-aff :n 0 :d 0))	; 0/0
(defconstant inf (make-aff :n 1 :d 0))	; 1/0
(defconstant minf (make-aff :n -1 :d 0))	; -1/0
(defconstant minz (make-aff :n 0 :d -1))	; 0/-1

(defmethod print-object ((a aff) stream)  (format stream "~a/~a"  (aff-n a) (aff-d a)))

;; Non-rational operations on extended rationals are not very useful, though
;; they do provide some help with locating branch cuts.
;; Algebraic and transcendental functions are of course
;;  not so great for regular rationals either --- unless you
;; have an approximation in mind.
;; Here are some, anyway.
(defmethod ga::sin ((s aff)) NaN)
(defmethod ga::cos ((s aff)) NaN)
(defmethod ga::tan ((s aff)) NaN)
(defmethod ga::exp ((s aff)) NaN)
(defmethod ga::log ((s aff)) NaN)
(defmethod ga::two-arg-expt((s aff)(n t)) (if (zerop n) 1 s)) ;NaN^0=1
(defmethod ga::two-arg-expt((s t)(n aff)) (cond ((= s 1) 1) ;1^inf, 1^nan
						 ((= s 0) 0) ;0^inf, 0^nan
						 ;; maybe |x|<1 x^inf?
						 (t n))) ;x^inf, x^nan
(defmethod ga::atan ((s aff)) NaN) 



;;; these all were copied from projective, which is simpler.
;;; but they are not accurate for affine model.

;;; Here is the table of operations for +
#|    1    2      3      4      5   6
 -------------------------------------
+     0    -0     1/0    -1/0   0/0   x
---------------------------------------
0     0     0     1/0    -1/0    ||   x

-0    0    -0     1/0    -1/0    ||    x

1/0  1/0  1/0    1/0      0/0    ||   1/0

-1/0 -1/0  -1/0  0/0     -1/0    ||   -1/0

0/0  - - - - -  - - - - - - - -  - -  0/0   all the same..

y     y     y     1/0    -1/0  0/0   x+y

|#
(defun minzp(x)(and (= (aff-n x) 0)(= (aff-d x) -1))) ;minus zero predicate
(defun pinfp(x)(and (= (aff-n x) 1)(= (aff-d x) 0))) ;pos inf predicate
(defun minfp(x)(and (= (aff-n x) -1)(= (aff-d x) 0))) ;minus inf predicate
(defun NaNp(x)(and (= (aff-n x) 0)(= (aff-d x) 0))) ;minus inf predicate

(defmethod ga::two-arg-+ ((r aff) (s aff))
  (cond ((or (NaNp r)(NaNp s)) r) ;col 5, row 5
	((minzp r) s) ;col 2
	((minzp s) r) ;row 2
	((pinfp r)(if (minfp s) NaN r)) ;col 3
	((pinfp s)(if (minfp r) NaN s))	;row 3
	((minfp r)(if (pinfp s) NaN r))	; row 4
	((minfp s)(if (pinfp r) NaN s))	; col 4
	(t (error "add ~s and ~s?" r s))))

(defmethod ga::two-arg-+ ((r aff) (s t)) 
  (if (minzp r)(if (= s 0) 0 s) r))	;
(defmethod ga::two-arg-+ ((s t) (r aff))
  (if (minzp r)(if (= s 0) 0 s) r))   ;; checked 1/2/06

#| Here is the table of operations for *
      1    2      3      4      5   6
------------------------------
*     0    -0     1/0    -1/0   0/0   x
---------------------------------------
0     0    -0    0/0     0/0    ||   0*s  ; s=sign(x)

-0   -0     0     0/0    0/0    ||   -0*s

1/0  0/0  0/0     1/0    -1/0   ||   s/0

-1/0  0/0  0/0   -1/0    1/0    ||   -s/0

0/0  - - - - -  - - - - - - - -  - -  0/0   all the same..

y     0*t   -0*t    t/0    -t/0  0/0  x*y  t=sign y


|#

(defmethod ga::two-arg-* ((r aff) (s aff))  
  (let ((n(* (aff-n r)(aff-n s)))
	(d (* (aff-d r)(aff-d s))))
    (if (= d 1) n  ;; from (-1) *( -1)
      (make-aff :n n
		:d d))))


(defmethod ga::two-arg-* ((r aff) (s t))  
  (make-aff :n (* (aff-n r)(signum s)) :d (aff-d r)))

(defmethod ga::two-arg-* ((s t)(r aff)) (ga::two-arg-* r s)) ;same as above

(defmethod ga::two-arg-/ ((r rational) (s (eql 0)))
  ;;norm/norm. check for norm/0 . This is how we construct INF and UND
  (make-aff :n (signum r) :d 0))

;; this does not do division by 0.0

;;More could be done; check with IEEE float rules affine 
;; The names for nan are not standardized. These are the names for allegro cl.

#+allegro
(defmethod ga::two-arg-/ ((r number)  (s (eql 0.0d0)))   ;; e.g. floats.
		      ;;norm/norm. check for norm/0
		      (cond ((zerop r) #.excl::*nan-double*)  ;0/0
			    ((excl::nan-p r)  #.excl::*nan-double*) ; nan/0
			    (t #.excl::*infinity-double*)))

(defmethod ga::two-arg-/ ((r t) (s aff))
  (* r (invert-aff s)))

(defmethod invert-aff((x aff))
  (let 	((n (aff-n x)))
    (if (= (aff-d x) -1)		
	minf				;  1/(0/-1)  is minus inf
      (cond ((= n 1)   0)		; 1/  (1/0) =  0
	    ((= n -1) minz)		; 1/  (-1/0) = -0
	    (t  NaN))			;  1/(0/0)  is 0/0
      )))

(defmethod invert-aff((x (eql 0)))inf)
(defmethod invert-aff((x t))(/ 1 x))

(defmethod negative-aff((x t))(- x))
(defmethod negative-aff((x aff))(if (= -1 (aff-d x)) 0 ; -0 -> 0
				  (make-aff :n (- (aff-n x)) ; 0/0 -> 0/0, -1/0 -> 1/0, 1/0 -> -1/0
					    :d (aff-d x))))

(defmethod ga::two-arg-- ((r t) (s aff))
  (+ r (negative-aff s)))

;; These are new: we can't shadow these in :ga.

;;(defmethod aff-numerator((s aff))(aff-n s))
;;(defmethod aff-numerator((s t))(cl::numerator s))
;;(defmethod aff-denominator((s aff))(aff-d s))
;;(defmethod aff-denominator((s t))(cl::denominator s))
;;(defun numerator(x)(if (aff-p x)(aff-n x) (cl::numerator x)))
;;(defun denominator(x)(if (aff-p x)(aff-d x) (cl::denominator x)))

;; this was copied from projective. It is wrong here.

#+ignore
(defmacro defcomparison (op)
  (let ((two-arg (intern (concatenate 'string "two-arg-" 
				      (symbol-name op))    :ga )))
    `(progn ;; very few compares work. Just notequal. See below
       (format t "~%defining ~s" ',two-arg)
       (defmethod ,two-arg ((arg1 aff) (arg2 number)) nil)
       (defmethod ,two-arg ((arg1 number) (arg2 aff)) nil)
       (defmethod ,two-arg ((arg1 aff) (arg2 aff)) nil)
      (compile ',two-arg)
      ',op)))
		      

;;(defcomparison >)
;;(defcomparison <)
;;(defcomparison <=)
;;(defcomparison >=)
;;
;; No regular or extended number is equal to  1/0 or 0/0, but 0 = -minz
(defmethod ga::two-arg-/= ((arg1 aff) (arg2 number)) (not (ga::two-arg-= arg1 arg2)))
(defmethod ga::two-arg-/= ((arg1 number) (arg2 aff)) (not (ga::two-arg-= arg1 arg2)))
	   
(defmethod ga::two-arg-=  ((arg1 (eql 0)) (arg2 aff)) (=(aff-d arg2) -1)) ; 0=-0
;; what about 0.0?
(defmethod ga::two-arg-= ((arg1 number) (arg2 aff)) nil)
(defmethod ga::two-arg-= ((arg1 aff) (arg2 number))(ga::two-arg-= arg2 arg1))

(defmethod ga::two-arg-= ((arg1 aff) (arg2 aff))
  (and (= (aff-n arg1)(aff-n arg2)) ;1=1 or -1=-1
       (= (aff-d arg1)(aff-d arg2)) ;-1=-1 or 0=0
       (not (= (aff-d arg1) (aff-n arg1) 0)))) ; neither is 0/0.
  
;; we can do this:
;; a/b < c/d   if  ad<cb  except if arg1=0/-1,

;; generate these by a macro defcomparison
;; 1/5/06
  
(defmacro defcomparison (op)
  (let ((two-arg (intern (concatenate 'string "two-arg-" 
				      (symbol-name op))    :ga ))
        (cl-op (ga::tocl op)))
    `(progn
        ;; only extra methods not in ga are defined here.
       (defmethod ,two-arg ((arg1 aff) (arg2 number))
	 (,cl-op (aff-n arg1)(* (if (= (aff-d arg1) -1) arg2 0))))
       (defmethod ,two-arg ((arg1 number) (arg2 aff))
	 (,cl-op (* arg1 (aff-d arg2))(aff-n arg2)))
       (defmethod ,two-arg ((arg1 aff) (arg2 aff))
	 (,cl-op (* (aff-n arg1) (aff-d arg2))(* (aff-d arg1)(aff-n arg2))))
      (compile ',two-arg)
      ',op)))

(defcomparison >)
(defcomparison <)

(defmethod ga::two-arg-<= ((a t)(b t))
  (or (= a b)(< a b)))

(defmethod ga::two-arg->= ((a t)(b t))
  (or (= a b)(> a b)))

(defmethod ga::two-arg-/= ((arg1 t) (arg2 t)) (not (ga::two-arg-= arg1 arg2)))

;; IEEE 754 has 26 distinct comparisons, >, <, =, unordered (X settings of flags for exceptions)


;; still to do.  min, max   via two-arg-min?