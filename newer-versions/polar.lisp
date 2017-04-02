;;;generic polar complex
(defpackage :polar
  (:use  :common-lisp :ga )
  (:shadowing-import-from 
   :ga
   "+" "-" "/" "*" "expt"		;binary arith
   "=" "/=" ">" "<" "<=" ">="		;binary comparisons
   "sin" "cos" "tan"			;... more trig
   "atan" "asin" "acos"			;... more inverse trig
   "sinh" "cosh" "atanh"		;... more hyperbolic
   "expt" "log" "exp" "sqrt"		;... more exponential, powers
   "1-" "1+" "abs" "incf" "decf"
   "tocl" "re-intern"
   "numerator" "denominator"
   "realpart" "complex" "imagpart"
   )
  (:export    "polar" )
)

(provide "polar" )
(in-package :polar)

;; x= r*cos(theta)
;; y= r*sin(theta)
;; r= sqrt(x^2+y^2) ;should be careful to avoid float overflow.
;; theta= arctan(y/x) , 
;; polar numbers  r>=0,   -pi <theta <=pi 

(defstruct (polar (:constructor polar (r &optional (th 0)))) r th)

(defmethod print-object ((a polar) stream)
  (format stream "~a cis(~a)"  (polar-r a) ;replace cis(+eps) with cis(0)
	  (polar-th a)))
    
;;; needed redefine two-arg-ops  of complex objects in terms of two-arg-* of pieces.
;;; first some macros

(defmacro with-polar2 (a1 a2 bind1 bind2 body)
  `(let ,(append 
	  `((,(car bind1) (polar-r ,a1)))
	  `((,(cadr bind1)(polar-th ,a1)))
	  `((,(car bind2) (polar-r ,a2)))
	  `((,(cadr bind2)(polar-th ,a2))))
     ,body))

(defmacro with-polar1 (a1 bind1 body)
  `(let ,(append 
	  `((,(car bind1) (polar-r ,a1)))
	  `((,(cadr bind1)(polar-th ,a1))) )
     ,body))


(defmacro with-polar-rect2 ;bind the real and imag parts in rect form, 2 args
    (a1 a2 bind1 bind2 body)
  `(let ,(append 
	  `((,(car bind1) (* (polar-r ,a1) (cos (polar-th ,a1))))) ;x1 in  x1+i*y1
	  `((,(cadr bind1)(* (polar-r ,a1) (sin (polar-th ,a1))))) ;y1
	  `((,(car bind2) (* (polar-r ,a2) (cos (polar-th ,a2)))))
	  `((,(cadr bind2)(* (polar-r ,a2) (sin (polar-th ,a2))))))
     ,body))

(defmacro with-polar-rect1 ;bind the real and imag parts in rect form
    (a1 bind1 body)
  `(let ,(append 
	  `((,(car bind1) (* (polar-r ,a1) (cos (polar-th ,a1))))) ;x1 in  x1+i*y1
	  `((,(cadr bind1)(* (polar-r ,a1) (sin (polar-th ,a1)))))) ;y1
	  ,body))

(defmethod ga::two-arg-*((x polar)(y polar))
  ;; use the macro above to do most of the work
  (with-polar2 x y (r1 t1)(r2 t2)(polarnorm (* r1 r2)(+ t1 t2)) ))

(defmethod ga::two-arg-*((x polar)(y t))
  (with-polar1 x (r1 t1) (polarnorm (* r1 y) t1)))

(defmethod ga::two-arg-*((y t)(x polar))
  (with-polar1 x (r1 t1)(polarnorm (* r1 y) t1)))

(defmethod polarnorm((r number)(th number))
  ;; if r<0 add pi to theta and set r to abs(r)
  ;; if theta is not in (-pi,pi], put it there.
  ;; this is not likely to work unless r and theta are numbers
  (labels
      ((norm (x)			; put angle in the right place between -pi and <=pi
	 (if (and (< #.(- pi) x)(<= x pi)) x
	   (- (mod (- x #.(+ pi 
			      (* 2 double-float-epsilon)
			     ))
		   #.(* 2 pi)) #.pi))))
    (unless(> r 0)(incf th pi)(setf r (- r)))
    (setf th (norm th))
    (if (<(abs th)) #.(* 2 double-float-epsilon)) 
    r ; no longer a polar object, arg is 0
    (polar r th)))

;; otherwise
(defmethod polarnorm((r t)(th t)) (polar r th))

;; + is not engineered for extreme bounds or preventing overflow on x^2+y^2
(defmethod ga::two-arg-+((p1 polar)(p2 polar)) 
  (with-polar-rect2 p1 p2 (a1 b1)(a2 b2) ; a1+b1*i, a2+b2*i
		    (let* ((a3 (+ a1 a2)) ; answer is a3+b3*i
			   (b3 (+ b1 b2))
			   (th3 (atan2 b3 a3))
			   (r3 (sqrt (+(* a3 a3)(* b3 b3)))))
		      (polarnorm r3 th3))))

(defmethod ga::two-arg-+((p1 polar)(p2 t)) ;p2 is presumed NOT complex
  (with-polar-rect1 p1 (a1 b1)		; p1 = a1+b1*i
		    (let* ((a3 (+ a1 p2)) 
			   (th3 (atan2 b1 a3 ))
			   ;;(th3 (/ b1 a3))	;should use atan2. theta
			   (r3 (sqrt (+(* a3 a3)(* b1 b1)))))
		      (polarnorm r3 th3))))

(defmethod ga::two-arg-+((p2 t)(p1 polar)) ;reverse the args.
  (ga::two-arg-+ p1 p2))

(defmethod rect ((p polar)) ;; convert polar to rectangular form in ma (um, a choice)
  (with-polar-rect1 p (a b)(+ a (* b (ma::ma 'i)))))

(defun polarize (a b) ;; convert  a+b*i to polar.
  (let ((r (sqrt (+ (* a a)(* b b))))
	(th (atan2 b a)))
    (polarnorm r th)))


(defmethod ga::two-arg--((p1 polar)(p2 polar)) 
  (with-polar-rect2 p1 p2 (a1 b1)(a2 b2) ; a1+b1*i, a2+b2*i
		    (let* ((a3 (- a1 a2)) ; answer is a3+b3*i
			   (b3 (- b1 b2))
			   (th3 (atan2  b3 a3))
			   (r3 (sqrt (+(* a3 a3)(* b3 b3)))))
		      (polarnorm r3 th3))))

(defmethod ga::two-arg--((p1 polar)(p2 t)) ;p2 is presumed NOT complex
  (with-polar-rect1 p1 (a1 b1)		; p1 = a1+b1*i
		    (let* ((a3 (- a1 p2)) 
			   (th3 (atan2 b1 a3))
			   (r3 (sqrt (+(* a3 a3)(* b1 b1)))))
		      (polarnorm r3 th3))))

(defmethod ga::two-arg--((p2 t)(p1 polar)) ;reverse the args.
  (ga::two-arg-- p1 p2))

(defmethod atan2( (y number) (x number))(cl::atan y x))
(defmethod atan2( (y t)(x t)) (ma::ma `(atan2 ,y ,x)))


;;; need  /  and expt.

;;; need to write the sin, cos, tan, etc.
;;; need exponential, log

;;;  orderings of complex objects all false, or error?
;;; except for = and /=

(defmethod ga::abs((x polar))(polar-r x))

(defmacro defcomparison (op)
  (let ((two-arg (intern (concatenate 'string "two-arg-" 
				      (symbol-name op))    :ga )))
    `(progn ;; very few compares work. Just notequal. See below
       (defmethod ,two-arg ((arg1 polar) (arg2 number)) nil)
       (defmethod ,two-arg ((arg1 number) (arg2 polar)) nil)
       (defmethod ,two-arg ((arg1 polar) (arg2 polar)) nil)
      (compile ',two-arg)
      ',op)))
(defcomparison >)
(defcomparison <)
(defcomparison <=)
(defcomparison >=)

(defmethod ga::two-arg-= ((arg1 polar) (arg2 polar))
  (with-polar2 arg1 arg2 (a b)(c d) (and (= a c)(= b d))))

(defmethod ga::two-arg-/= ((arg1 polar) (arg2 polar))
  (with-polar2 arg1 arg2 (a b)(c d) (or (/= a c)(/= b d))))

(defmethod ga::two-arg-= ((arg1 polar) (arg2 t))nil)
(defmethod ga::two-arg-/= ((arg1 polar) (arg2 t))t)

