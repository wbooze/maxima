;;; Automatic Differentiation code for Common Lisp (ADIL)
;;; using overloading, forward differentiation.

;; code extended by Richard Fateman, November, 2005

(defpackage :df				;derivative and function package
  (:use  :cl)
  (:shadowing-import-from 
   :ga
   "+" "-" "/" "*" "expt"		;binary arith
   "=" "/=" ">" "<" "<=" ">="		;binary comparisons
   "sin" "cos" "tan"			;... more trig
   "atan" "asin" "acos"			;... more inverse trig
   "sinh" "cosh" "atanh"		;... more hyperbolic
   "expt" "log" "exp" "sqrt"		;... more exponential, powers
   "1-" "1+" "abs" "incf" "decf"
   "numerator" "denominator"	  
   "tocl" 	  )
   (:export "df")
   )
  
(require "ga" )
(provide "df" )
(in-package :df)

;;  structure for f,d: f is function value, and d derivative, default 0
;; df is also the constructor for an object with 2 components, f and d..
(defstruct (df (:constructor df (f &optional (d 0)))) f d )
(defmethod print-object ((a df) stream)(format stream "<~a, ~a>" (df-f a)(df-d a)))

;;comparison of df objects depends only on their f parts (values)
;;extend the generic arithmetic for this purpose

(defmacro defcomparison (op)
  (let ((two-arg (intern (concatenate 'string "two-arg-" 
				      (symbol-name op))    :ga ))
        (cl-op (tocl op)))
    `(progn
        ;; only extra methods not in ga are defined here.
      (defmethod ,two-arg ((arg1 df) (arg2 df))    (,cl-op (df-f arg1)(df-f arg2)))
      (defmethod ,two-arg ((arg1 number) (arg2 df))(,cl-op arg1(df-f arg2)))
      (defmethod ,two-arg ((arg1 df) (arg2 number))(,cl-op (df-f arg1) arg2 ))
      (compile ',two-arg)
      (compile ',op)
      ',op)))

(defcomparison >)
(defcomparison =)
(defcomparison /=)
(defcomparison <)
(defcomparison <=)
(defcomparison >=)

;; extra + methods specific to df
(defmethod ga::two-arg-+ ((a df) (b df))    (df  (+ (df-f a)(df-f b))
						 (+ (df-d a)(df-d b))))
(defmethod ga::two-arg-+ ((b df)(a number))   (df  (+ a (df-f b))    (df-d b)))
(defmethod ga::two-arg-+ ((a number)(b df))   (df  (+ a (df-f b))    (df-d b)))

;;extra - methods

(defmethod ga::two-arg-- ((a df) (b df))    (df  (- (df-f a)(df-f b))
						 (- (df-d a)(df-d b))))
(defmethod ga::two-arg-- ((b df)(a number))   (df  (-  (df-f b) a)    (df-d b)))
(defmethod ga::two-arg-- ((a number)(b df))   (df  (- a (df-f b))    (df-d (- b))))

;;extra * methods
(defmethod ga::two-arg-* ((a df) (b df)) 
  (df  (* (df-f a)(df-f b))
       (+ (* (df-d a) (df-f b)) (* (df-d b) (df-f a)))))
(defmethod ga::two-arg-* 
    ( (b df)(a number)) (df  (* a (df-f b))  (* a (df-d b))))
(defmethod ga::two-arg-* 
    ((a number) (b df)) (df  (* a (df-f b))  (* a (df-d b))))

;; extra divide methods
(defmethod ga::two-arg-/  ((u df) (v df)) 
  (df  (/ (df-f u)(df-f v))
	    (/ (+ (* -1 (df-f u)(df-d v))
			  (* (df-f v)(df-d u)))
		    (* (df-f v)(df-f v)))))
(defmethod ga::two-arg-/  ((u number) (v df)) 
  (df  (/ u (df-f v))
	    (/ (* -1  (df-f u)(df-d v))
		    (* (df-f v)(df-f v)))))
(defmethod ga::two-arg-/  ((u df) (v number)) 
  (df  (/ (df-f u) v)
	    (/ (df-d u) v)))

;; extra expt methods
(defmethod ga::two-arg-expt  ((u df) (v number))
  (df  (expt (df-f u) v)
       (* v (expt (df-f u) (1- v)) (df-d u))))

(defmethod ga::two-arg-expt ((u df) (v df))
  (let* ((z (expt (df-f u) (df-f v)));;z=u^v
	 (w;;   u(x)^v(x)*(dv*LOG(u(x))+du*v(x)/u(x)) = z*(dv*LOG(u(x))+du*v(x)/u(x))
	  (* z (+
		(* (log (df-f u))	;log(u)
		   (df-d v))		;dv
		(/ (* (df-f v)(df-d u)) ;v*du/ u
		   (df-f u))))))
    (df  z  w)))

(defmethod ga::two-arg-expt ((u number) (v df))
  (let* ((z (expt u (df-f v))) ;;z=u^v
	 (w   ;;    z*(dv*LOG(u(x))
	  (* z (* (log u) ;log(u)
			 (df-d v)))))
    (df  z  w)))

;; A rule to define rules.

(defmacro r (op s)
  `(progn
     (defmethod ,op ((a df)) ;; the chain rule d(f(u(x)))=df/du*du/dx
       (df  (,op (df-f a))
	    (* (df-d a) ,(subst '(df-f a) 'x s))))
     (defmethod ,op ((a number)) (,(tocl op) a))))

;; add as many rules as you can think of here.
;; should insert them in the shadow list too.
(r sin (cos x))
(r cos (* -1 (sin x)))
(r asin (expt (+ 1 (* -1 (expt x 2))) -1/2))
(r acos (* -1 (expt (+ 1 (* -1 (expt x 2))) -1/2)))
(r atan (expt (+ 1 (expt x 2)) -1))
(r sinh (cosh x))
(r cosh (sinh x))
(r atanh (expt (1+ (* -1 (expt x 2))) -1))
(r log (expt x -1))
(r exp (exp x))
(r sqrt (* 1/2 (expt x -1/2)))
(r 1-  1)
(r 1+  1)
(r abs x);; hm.

;; (meval (fact x) x 10)   no quotes. in :ma package  
(defmacro meval(r x p &optional (dx 1))
	      `(let ((,x ,(df  p  dx)))
		 (declare (special ,x))
		 ,r))

;; (deval '(fact x) 'x 10) ; yes, quotes
(defun deval(r x p &optional (dx 1))
	    (eval  `(let ((,x ,(df  p  dx)))
		 (declare (special ,x))
		 ,r)))

(defun cl-user::deval(r x p &optional (dx 1))
  (df::deval (ga::re-intern r :df)
	     (ga::re-intern x :df) p dx))

;; factorial with "right" derivative at x=1 to match gamma function

 (defun fact(x) (if (= x 1) (df 1 0.422784335098d0) (* x (fact (1- x)))))
#+ ignore
(defun fact(x) (if (= x 1) 1  (* x (fact (1- x)))))

;; stirling approximation to factorial
(defun stir(n) (* (expt n n) (exp (- n))(sqrt (*(+ (* 2 n) 1/3) 3.141592653589793d0))))

(defun ex(x)(ex1 x 1 15))
(defun ex1(x i lim)  ;; generate Taylor summation exactly for exp()
  (if (= i lim) 0 (+ 1 (* x  (ex1 x (+ i 1) lim)(expt i -1)))))

;; in :user (setf one (ga::df  1  1))
;; in :user (deval '(dotimes (i 10)(print (fact (+ i one)))) 'x 'irrelevant)

;; in :ga   (setf one (df  1  1))
;; in :ga   (dotimes (i 10)(print (fact (+ i one))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|Some timing results suggest that we lose about a factor of 10 in
speed just using generic arithmetic, even if we are using (say)
floating point numbers, if we are doing mostly calls and returns, but
only trivial arithmetic. Under these conditions, if we actually use df
numbers, it is perhaps a factor of two addition.

The big slowdown here is in generic over the built-in (but still generic)
Lisp arithmetic that allows floats, doubles, rationals, bignums.

If we compile with declarations for fixnum types among the lisp built-ins,
we improved by another 30 percent .


(time (dotimes (i 10)(slowmul  1000000  2  0))) ;compiled 19.38s in :ga
(time (dotimes (i 10)(slowmul  1000000  2  (df 0)))) ;    24.45s in :ga
(time (dotimes (i 10)(slowmul  1000000  2  0))) ;compiled   .37s in :user
(time (dotimes (i 10)(slowmulx 10000.0d0 2.0d0 0.0d0)));    .38 in :user declared doubles
(time (dotimes (i 10)(slowmulx 1000000 2 0)))        ;      .29 in :user declared fixnums


(defun slowmul(x y ans)(if (= x 0) ans (slowmul (1- x) y (+ y ans))))

(defun slowmulx(x y ans)
  (declare (fixnum x y ans) ;; or (double-float x y ans)
	   (optimize (speed 3)(safety 0)(debug 0)))
	   (if (= x 0) ans (slowmulx (1- x) y (+ y ans))))



Second version.  Make it faster but still
compatible with the previous version, but compile everything??
Instead of running the methods, try to dispatch at macro-expansion
time to do just the right thing?? Is this really worth it?

Another possibility is to use lists  e.g. (f . d) instead of
df.


;;run slowmul through compiling with dcomp.

(time (dotimes (i 10000)(slowmul  1000.0d0  2.0d0  0.0d0))) ;     36.54 sec
;; why is this so slow??

;; this is an interesting function that numerically computes (sin x)

(defun s(x) (if (< (abs x) 1.0d-5) x 
	      (let ((z (s (* -1/3 x))))
		(-(* 4 (expt z 3))
		  (* 3 z)))))

 (s  (df 1.23d0 1.0))   computes both sin and cos of 1.23.


(time (dotimes (i 100) (s (df 100000.0d0  1))) ) :ga 40ms
(time (dotimes (i 100) (s  100000.0d0  ))) :ga 20 ms
(time (dotimes (i 100) (s  100000.0d0  ))) :user 10ms av over more runs
(time (dotimes (i 100) (ss 100000.0d0  ))) :user  8ms av over more runs


;; compile to run faster..

(defun ss(x) (declare (double-float x)
		      (optimize (speed 3)(safety 0) (debug 0)))
       (if (< (abs x) 1.0d-5) x (let ((z (ss (* #.(/ -1 3.0d0) x))))
					   (-(* 4.0d0 (expt z 3))
					     (* 3.0d0 z)))))

|#


;; newton iteration  (ni fun guess)
;; usage:  fun is a function of one arg.
;;         guess is an estimate of solution of fun(x)=0
;; output: a new guess. (Not a df structure, just a number

(defun ni (f z) ;one newton step
  (let* ((pt (if (df-p z) z (df z 1)))	; make sure init point is a df
	 (v (funcall f pt))) ;compute f, f'
    (df-f (- pt (/ (df-f v)(df-d v))))))


(defun ni2 (f z) ;one newton step, give more info
  (let* ((pt (if (df-p z) z (df z 1)))
	 (v (funcall f pt)))		;compute f, f' at pt
    (format t "~%v = ~s" v)
  (values
   (df-f (- pt (/ (df-f v)(df-d v)))) ;the next guess
    v)))

(defun run-newt1(f guess &optional (count 18)) ;; Solve f=0
  ;; here's one that might work. It looks only at the guesses.
  ;; though if the derivative goes to 0, we are stuck
  ;; in the newton step.
  (let ((guesses (list guess))
	(reltol #.(* 100 double-float-epsilon))
	(abstol #.(* 100 least-positive-double-float)))
    (dotimes (i 6)(push (ni f (car guesses))
			guesses)) ;; make 6 iterations.
    (incf count -6)
    (cond ((< (abs (car guesses)) abstol)
	   (car guesses))
	  ((< (abs(/ (-(car guesses)(cadr guesses))(car guesses))) reltol)
	   (car guesses))
	  ((<= count 0)
	   (format t "~%newt1 failed to converge; guess =~s" (car guesses))
	   (car guesses))
	  (t (run-newt1 f (car guesses) count)))))

(defun run-newt2(f guess &key (abstol 1.0d-8) (count 18)) ;; Solve f=0
  ;; here's one that might work. It looks only at the 
  ;; residual.
    (dotimes (i count 
	       (format t  "~%Newton iteration not convergent after ~s iterations: ~s" count guess))
      (multiple-value-bind
	  (newguess v)
	  (ni2 f guess)
	(if (< (abs (df-f v)) abstol) (return newguess)
	  (setf guess newguess)))))

;;try (run-newt2 'sin 3.0d0)
;; try legendre polynomial
(defun p(m x)(cond ((= m 0) 1)
		   ((= m 1) x)
		   (t (*  (/ 1 m) (+ (* (1- (* 2 m)) x (p (1- m) x))
				     (* (- 1 m) (p (- m 2) x)))))))
;; simpler syntax for dcomp.
'(defdiff p(m x)(if (= m 0) 1
		   (if (= m 1) x
		   (*  (/ 1 m) (+ (* (+ (* 2 m) -1) x (p (- m 1) x))
				     (* (- 1 m) (p (- m 2) x)))))))

			  



