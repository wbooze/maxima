;;; attempts by RJF to make OCT work fast in Allegro, based on 
;;; translating RTOY's code to macros, and converting code from RJF's QD to OCT.
;;; see comments in file octi.lisp too
;;;  certainly until I include all of RTOY's stuff for sin/cos/.
;;;  Still, my qd stuff has quadrature, fft, polynomial eval.

;;; 10/18/07 RJF

(defpackage :oct
  (:use :ga  :octi :cl)
    (:shadowing-import-from  
   :ga    
   "+" "-" "/" "*" "expt"		;... n-ary arith
   "=" "/=" ">" "<" "<=" ">="		;... n-ary comparisons
   "1-" "1+" "abs" "incf" "decf"
   "min" "max"
   "ftruncate" "ffloor" "fround" "fceiling" 
   sin cos tan 
   asin acos 
   atan log   ;; these are trickier, 1 or 2 args.
   exp
   sinh cosh tanh 
   asinh acosh atanh 
   sqrt 
   tocl 
   numerator denominator
   expt 
   rational  ;; convert to rational, exactly
   )
    (:export into)
    )

(in-package :oct)   ;;; NOT OCTI


(eval-when (:execute :load-toplevel :compile-toplevel :execute)
  
(defun into(x &optional (targ 0 targp)) ;if a target supplied, store into it
    (if targp (progn (octi::into x (oct-real targ))targ)
      (make-oct :real (octi::into x))))



(defstruct oct (real  #+allegro
		      (make-array 4 :element-type 'double-float 
			       :initial-element 0.0d0 
			       ;; next line is Allegro-specific
			       :allocation :lispstatic-reclaimable
			       )
		      #-allegro
		      (make-array 4 :element-type 'double-float 
			       :initial-element 0.0d0)

		      :type (simple-array double-float (4)))))

(defmethod make-load-form ((a oct)&optional environment)
  (make-load-form-saving-slots a :environment environment)

;;(defmethod make-oct-d ((a array))  ;; make an object from the array
;;  (make-instance 'oct :real a))
)
(defmacro copy_mac(a);;make a fresh copy of oct a.
  `(make-oct :real
   (make-array 4 :element-type 
			    'double-float 
			    :initial-contents (oct-real ,a)
			    :allocation :lispstatic-reclaimable)))

(defun copy_into(op1 op2) ;fill op2's memory with op1's value.
  (octi::copy-octi-into-octi (oct-real op1)(oct-real op2)) op2)

(defmacro defarithmetic (op pgm)
    (let ((two-arg
	   (intern (concatenate 'string "two-arg-" (symbol-name op))
		   :ga))
	  (oo-entry ;; two oct args
	   (intern (concatenate 'string (symbol-name pgm) "-oct-t")
		   :octi))
	  ;;(od-entry ;; oct and double args
	  ;; (intern (concatenate 'string (symbol-name pgm) "-oct-d-t") :octi))
	  )
	
 ;;           (format t "~% defining ~s" two-arg)
      `(progn
	 
	 ;; new defmethods for oct. ..
	 (defmethod ,two-arg ((arg1 oct) (arg2 oct))
	   (let* ((r (oct::make-oct))
		  (in (oct::oct-real r))
		  (a1 (oct::oct-real arg1))
		  (a2 (oct::oct-real arg2)))
	     (declare (optimize speed)
		      (type(simple-array double-float (4)) in a1 a2))
	     (,oo-entry a1 a2 in) r))
       
	 (defmethod ,two-arg((arg1 real) (arg2 oct))
	  #+ignore (,two-arg (into arg1) arg2)
	   (let* ((r (oct::make-oct :real (octi::into arg1)))
		  (in (oct::oct-real r))
		  (a2 (oct::oct-real arg2)))
	     (declare (optimize speed)
		      (type(simple-array double-float (4)) in a1 a2))
	     (,oo-entry in a2 in) r)
	   )
       
	 (defmethod ,two-arg ((arg1 oct) (arg2 real))
	  #+ignore    (,two-arg arg1 (into arg2))

		   (let* ((r (oct::make-oct :real (octi::into arg2)))
		  (in (oct::oct-real r))
		  (a1 (oct::oct-real arg1) ))
	     (declare (optimize speed)
		      (type(simple-array double-float (4)) in a1 a2))
	   
	     (,oo-entry a1 in in) r))
	 (setf (get ',op 'argnum) 2) ;used by with-temps, dsetv
	 (setf (get ',op 'oct-program) ',oo-entry) ;used by with-temps, dsetv
	 (setf (get ',two-arg 'oct-program) ',oo-entry) ;used after macroexpand-all
	 (setf (get ',two-arg 'argnum) 2)
	 
	 )))


(defarithmetic + add)
(defarithmetic * mul)
(defarithmetic - sub)
(defarithmetic / div)

;; need to do other elementary functions etc as well.
;; see lisp/generic/qd and  lisp/oct/qd-fun..

;; this special case doesn't fit into defarithmetic macro.
(defmethod ga::two-arg-expt ((base oct)(n integer))
  (let ((ans (make-oct)))
    (octi::pow-oct-i-t (oct-real base) n (oct-real ans))
    ans))


       
(defmethod print-object ((a oct) stream)(format stream "~a" 
						(octi::oct2string (oct::oct-real a))))

(defmethod octi::lisp2oct((s string)(ans array))(string2oct s))

(defmethod octi::lisp2oct((a oct) (ans array)) (octi::copy-octi-into-octi (oct-real a) ans ))

(defun oct-reader(stream subchar arg)
  (declare (ignore subchar arg))
  (let ((s (read stream)))

    (string2oct (format nil "~s" s))))

(defun string2oct(s);; read integerQinteger like 10Q2   = 100 = 0.1Q3  or  -3.1q-1
  ;; examples of acceptable numbers [put " " around them]
  ;; .1q0 ; 1Q0; 1.2q3;  -1.23q-4;  - .23 q -4;  2.3q+4; +4.5q6; q0; --3Q1 same as 3q1
  ;; 3 ; 3.4; 123.45q+100; -.q0 ; .q0 ;
  ;; examples of not acceptable numbers:
  ;;    q ; .q;   123.45q+100  [=NaN(qd)]
  (let ((p0 0)(sign 1))
    (setf s (delete #\  s));; remove leading or other spaces from string.
    (if (char= #\- (aref s 0))(setf p0 1 sign -1));; set sign if negative
    (multiple-value-bind (frac pos);; read fraction to left of .
	(parse-integer s :start p0 :radix 10 :junk-allowed t)
  ;;    (format t "~% frac= ~s pos=~s" frac pos)
      (if (null frac)(setf frac 0));; empty fraction is zero
      (if (= pos (length s)) (* sign (into frac))
	(case (aref s pos);; look at next char
	  ((#\Q #\q);; 10Q2
	   (multiple-value-bind (expon pos2)
	       (parse-integer s :start (1+ pos);skip the Q
			      :radix 10)
	;;     (format t "~% sign=~s frac=~s expon=~s" sign frac expon)
	     (* sign (into frac) (expt 10 expon))))
	  (#\.;; 
	   (multiple-value-bind (frac2 pos2)
	       (parse-integer s :start (1+ pos);skip the "."
			      :radix 10 :junk-allowed t )
	     (if (null frac2)(setf frac2 0))
	     (setf frac
		   (+ (into frac)
		      (* (if (< frac 0) -1 1) frac2 (cl::expt 10 (cl::1+(cl::- pos pos2))))))
	      ;; (format t "~% string pos ~s frac= ~s" pos2 frac)
	     (if (cl::= pos2 (length s))(* sign (into frac))
	       (case (aref s pos2)
		 ((#\Q #\q);; 10Q2
		  (multiple-value-bind (expon pos3)
		      (parse-integer s :start (1+ pos2);skip the Q
				     :radix 10 )
		    (* sign  frac (cl::expt 10 expon))
		    ))
		 (otherwise 
		  (format t "next char is ~a -- Not an oct spec: ~s"  
			  (aref s pos2) s)
		  (into (or frac 0)))))))
	  )))))



;; reading OCT numbers can be done this way...
(set-dispatch-macro-character #\# #\q #'oct::oct-reader) 

(defun time-mul (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((h (into 1/3)))
    (print h)
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (* h h)))))

(defun time-poly (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((h (list (into 5)(into 2)(into 1)))
	 (arg (into 100)))
    (print h)
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (polyeval h arg)))))
(defun time-polyd (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((h (list 5d0 2d0 1d0))
	 (arg 100d0))
    (print h)
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (polyevald h arg)))))



;;Compiler for  oct data type
;; RJF
(in-package :oct)

;;; We want to make better use of the state-based programs like
;;; mul-oct-t Assuming octs.. for a, b, and c: (dsetv a (+ b c))
;;; destroys the value in a.  Compare this to (setf a (+ b c)) which
;;; creates a new value and points a to it.

;; dsetv,  data driven
(defmacro dsetv (targ ex)
  ;; try  (dsetv a (+ b c)) 
  ;; should be faster than (setf a (+ b c)). maybe 2X.
  ;; All the logic below is done during macro-expansion,
  ;; which means it is usually done at compile time. Run time
  ;; is therefore not penalized.  If you use dsetv from an interpreted
  ;; program it will be slow, however, because it will do the macro
  ;; expansion followed by the execution, each time it is used.
  (setf ex (macroexpand ex))  
  (cond 
   ((atom ex) `(into ,ex ,targ))
   ((eq (car ex) 'into) `(into ,@(cdr ex)  ,targ))
   ((eq (car ex) 'setq) 
    (let ((gg (gensym))) ;; need to protect against capturing z in (setq z ..))
    `(let ((,gg  ,(with-temps (caddr ex))))
       (copy_into  (oct-real ,gg) (oct-real ,(cadr ex)))
       ,gg)))
   (t 
    (let* ((op (car ex))
	   (args (cdr ex))
	   (the-op (get op 'oct-program))
	   (argnum (get op 'argnum)))
      (cond 	       
       ((not the-op);; not a previously listed op
	`
	   (let* ((lval ,targ)
		  (a1 (oct-real  (,op ,@ args)))
		  (tt (oct-real lval)))
	     (declare (optimize speed)
		      (type (simple-array double-float (4)) a1 tt))
	     (copy_into a1 tt)
	     lval))
       ((not (eql argnum (length args))) 
	(error "dsetv was given operator ~s which expects ~s args, but was given ~s --  ~s" 
	       op argnum (length args) args))
       (t
	(case argnum
	  (1;; one argument.
	   `(let ((a1 (oct-real ,(macroexpand `(with-temps ,(car args)))))
		    (tt (oct-real ,targ)))
		(declare (optimize speed)(type (simple-array double-float (4)) a1 tt))
		;; could also check other args for being type qd
		;; could also allow for args to be si, ui, dd, etc.
		;; could also check number of args to be appropriate for operation
		(,the-op a1 tt)
	      ,targ))
	  (2
	   `(let ((a1 (oct-real ,(macroexpand `(with-temps ,(car args)))))
		  (a2 (oct-real ,(macroexpand `(with-temps ,(cadr args)))))
		    (tt (oct-real ,targ)))
		(declare (optimize speed)(type (simple-array double-float (4)) a1 a2 tt))
		(,the-op a1 a2 tt)
		,targ
		))
	  (otherwise (error "argnum is wrong for op ~s " op))
	  )))))))

;;;;;;;;;;;;;;;;;;more efficiency hackery follows.;;;;;;;;;;;;;;;;


;; We just allocate a few private "registers" say, for a
;; function, or an inner loop, and re-use them, if we are in a
;; loop. No need to tell anyone else about a few temp locations,
;; especially if they are GC'd when truly inaccessible.  That's what
;; is below.

(defmacro with-temps(expr)
  (let ((*names* nil)
	(*howmany* 0))
    (labels ((genlist(n)(loop for i from 1 to n collect (into i))) ;make a list of fresh qd items
	     (ct1 (r) ;; count temporaries needed
	       (cond ((numberp r) (incf *howmany*))
		     ((not (consp r)) r)
		     (t (incf *howmany*)
			(mapc #'ct1 (cdr r)))))
		
	   (maketemps(r) ;change r=(+ a (* b c)) to  temp storage .
		     (cond ((numberp r) (into r))
			   ((atom r) r)
			   ((get (car r) 'argnum); known operator
			    `(dsetv ,(pop *names*)
				    ,(cons (car r)(mapcar #'maketemps (cdr r)))))
			   ;; just a symbol name? maybe aref? better be the right type, aqd.
			   (t  r))))
      (setf expr (macroexpand expr))
       (ct1 expr)
     ;; (ct1 expr); count the temporaries
    (setf *names* (genlist *howmany*))
    (maketemps expr))))


;;  try (pprint (macroexpand '(with-temps (+ x (* 3 z)))))
;; or  (defun hypot(x y)(copy_mac (with-temps (sqrt (+ (* x x)(* y y))))))
;; need the call to copy to make a  copy of the result before calling hypot again. 

;; set up the environment for with-temps and dsetv here

(eval-when (compile load eval)
  (mapc #'(lambda(h) (setf (get h 'argnum) 2)) '(atan2 log2 setq))
;; for now assume they are given only one arg and if they are given 2 signal an error with dsetv.
  (mapc #'(lambda(h) (setf (get h 'argnum) 1)) '(atan log))
;;  (defun qd_setq (a b)(qd_copy_into b a) b)
 )

;; time trial

(defun time-mul-d (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((h (into 1/3))
	 (ans (into 0)))
    (print h)
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (dsetv ans (* h h))))))


;;;;;;;;;;;::::There's a lot more in generic/qd.lisp
;;;;;;;;;not yet converted to OCT.

(defmacro defcomparison (op)
    (let ((two-arg-ga (intern (concatenate 'string "two-arg-" 
					   (symbol-name op)) :ga))
	  (two-arg-octi (intern (concatenate 'string "two-arg-" 
					(symbol-name op)) :octi))
	  )
    ;;  (format t "~% defining ~s" two-arg)
      `(progn
	 ;; only extra methods not in ga are defined here.
	 ;; qd_comp returns -1 0 1   for < = >
	 (defmethod ,two-arg-ga ((arg1 oct) (arg2 oct))   
	   (,two-arg-octi (oct-real arg1)(oct-real arg2)))

	 (defmethod ,two-arg-ga ((arg1 real) (arg2 oct))
	   (let ((arg1 (into arg1)))
	     (,two-arg-octi (oct-real arg1)(oct-real arg2))))
	 
	 (defmethod ,two-arg-ga ((arg1 oct) (arg2 real))
	   (let ((arg2 (into arg2)))
	     (,two-arg-octi (oct-real arg1)(oct-real arg2))))
	 ',op)))

(defcomparison >)
(defcomparison <)
(defcomparison =)
(defcomparison /=)
(defcomparison >=)
(defcomparison <=)

;; allows (< (into 1)(into 2)(into 3)) 



;;How about the single-arg functions like sin cos log...

(defmacro r (op);;
  (let ((fun-name  (intern op :ga ))
	(octi-name (intern (concatenate 'string op "-oct-t") :octi)))

    `(progn
       (defmethod ,fun-name ((arg oct))
	 (let* ((h (make-oct)) (in (oct-real h)))
	   (declare (optimize speed)
		    (type (simple-array double-float (4)) in))
	   (,octi-name (oct-real arg) in)
	   h))
       
       (setf (get ',fun-name 'argnum) 1)
       (setf (get ',fun-name 'oct-program) ',octi-name)
       ',fun-name
       )))


(r "sqrt") (r "abs")
(r "sin")(r "cos")

;;(r "log")(r "exp") ;; etc
(r "1-")
(r "1+")
(r "exp")


(defmethod ga::one-arg-atan((arg oct))
           (let* ((h (make-oct)) (in (oct-real h)))
           (declare (optimize speed)
            (type (simple-array double-float (4)) in))
           (octi::atan-oct-t (oct-real arg) in)
           h))

(defun ftruncate ( x &optional (y 1))
  (octi::ftruncate-oct (oct-real x) y))
(defun fceiling (x &optional (y 1)) ;; fceiling for oct only
  (octi::fceiling-oct (oct-real x) y))
(defun fround (x &optional (y 1))
  (octi::fround-oct (oct-real x) y))

;; complex version
;; we do not propose to implement complex numbers at the octi level.


(defstruct octz (real (into 0) oct)
	   (imag (into 0)oct))

;; try these out.

(defmethod ga::two-arg-+ ((a octz)(b octz))
  (let ((ans (make-octz)))
    (setf (octz-real ans) (+ (octz-real a)(octz-real b))
	  (octz-imag ans) (+ (octz-imag a)(octz-imag b)))
    ans))

(defmethod ga::two-arg-* ((a octz)(b octz))
  (let ((ans (make-octz)))
    (setf (octz-real ans) (* (octz-real a)(octz-real b))
	  (octz-imag ans) (* (octz-imag a)(octz-imag b)))
    ans))

(defun lisp2octz(a)
  (let ((ans (make-octz)))
    (setf (octz-real ans) (into (realpart a))
	  (octz-imag ans) (into (imagpart a)))
    ans))

;; we could set up a separate package for complex oct..
;; but then the sqrt(neg) issue becomes complicated.

(defmethod ga::exp ((z octz))
  ;; exp(a+bi)= exp(a)*(cos b + i sin b)
  
  (let* ((ans (make-octz))
	 (a (octz-real z))		;real
	 (b (octz-imag z))		;real
	 (ea (exp a))			;real
	 )
    (setf (octz-real ans)   (* ea(cos b))
	  (octz-imag ans)   (* ea (sin b)))
    ans))

;; re-implement ga::sqrt((z oct))  as well as ga::sqrt((z octz)).


	  
    

  
;;;;;; some fun.
;; need abs, < >
#|

;; A BUNCH OF PROGRAMS PLAYING OCT GAMES and ROOTFINDING.

;; Refining a polynomial root via Newton's method.
;; First, deriv computes the derivative of a polynomial, in a list.
;; recall that we can create a polynomial as a list:
;; (setf testpoly (list 5 3 1)) ;; 5*x^2+3*x+1
;; or as an OCT polynomial
;; (setf testpoly (list (into 5) (into 3) (into 1))) ;; 5*x^2+3*x+1
|#

(defun deriv(coefs)
  ;;given coefs of a polynomial. return coefs of derivative.
  ;; constant coeff is last.  Any kind of numbers are OK.
  (let ((ans nil) (i 0))
    (dolist (c (cdr (reverse coefs)) ans) 
      (push (* (incf i) c) ans))  ans))

(defun polyeval (coeflist x) ;coeflist of octs, x is an oct
      (let ((sum (into 0)))
      (dolist (i coeflist sum)
	(setf sum (with-temps (+ i (* x sum)))))))

;;  a fairly general Newton iteration requiring three parameters, and some optional ones.
;;  f  is a function to evaluate f(x),
;;  df is a function to evaluate f'(x).
;;  x, a starting point,
;; optional: threshold for absolute value of f at which to stop,
;; optional: iters, a maximum number of iterations.

;;  oneroot uses whatever arithmetic is appropriate, based on what f and df use.

(defun oneroot (f df x threshold iters &aux fval)
  (dotimes  (i iters (error "rootfinder failed to converge. Residual is ~s after ~s iterations." 
			    fval i))
    (setf fval (funcall f x))
    (if (< (abs fval) threshold) 
	(return (values x fval i))		;return x, residual and iteration count
      (decf x (/ fval (funcall df x))))))


;; if we replace (decf x (/ fval (funcall df x)))
;; by            (dsetv x (with-temps (- x (/ fval (funcall df x))))))))
;; this would save 2 allocated temporaries per iteration.


;; A carefully declared polynomial eval using
;; Lisp double-floats. I don't see how to make this more optimized.
(defun polyevald (dlist x) 
  (let ((sum 0.0d0))
    (declare (double-float sum x) (optimize (speed 3)(debug 0)))
    (dolist (i dlist sum)
      (declare (double-float i))
      (setf sum (the double-float 
		  (cl::+ i
			 (the double-float 
			   (cl::* x sum)))))))) ;; this is about 130 X faster than the oct version.

;; Next, set up a call to oneroot given a list of coefficients representing
;; a polynomial.  polyrootd makes everything computing in (real) double-float.
;; defaults are provided, so all you need is the list and a starting point.
;; the inputs coefs and x may be given as exact integers, floats, rationals.
;; example  (polyrootd '(1 0 -2) 1); returns 1.4142135623730951d0, i.e. sqrt(2)

(defun polyrootd (coefs x &optional (threshold 1.0d-14) (iters 20))
  (setf coefs (mapcar #'(lambda(z)(coerce z 'double-float))coefs))
    (let ((dp (deriv coefs)))
      (oneroot  #'(lambda(x)(polyevald coefs x))
		#'(lambda(x)(polyevald dp x))
		(* 1.0d0 x)
		(* 1.0d0 threshold)
		iters)))

;; this is the same functionality, but using higher (QD) precision.
;; the inputs coefs and x may be given as exact integers, floats, rationals.
;; (oct::polyroot '(1 0 -2) 1); returns sqrt(2), but if you want more precision:
;; (oct::polyroot '(1 0 -2) 1 1.0d-60)  returns
;;0.141421356237309504880168872420969807856967187537694807317667973796Q1

;;this is called oct:polyroot. You may prefer other ways to do it, 
;;e.g. mpfr::polyroot or some interval method, or some method
;; computing derivatives.

(defun polyroot (coefs x &optional (threshold 1.0d-50) (iters 20))
  (setf coefs (mapcar #'into coefs))
  (let ((dp (deriv coefs)))
    (oneroot  #'(lambda(x)(polyeval coefs x))
	      #'(lambda(x)(polyeval dp x))
	      (into x)
	      (into threshold)
	      iters)))

;; try (polyroot '(1 0 -4) 1 1d-60 20)

;; ONEROOT is considerably more versatile: it can find root of other
;; functions, not just polynomials.
#|
(defun tt (x)(- (cos x) x))  ;; easy near 0.73
(defun ttd(x) (- (+ (sin x) 1))  )
(oneroot #'tt #'ttd (into 0) (into 1.0d-64) 20)  ;; uses oct
(oneroot #'tt #'ttd 0.0d0 1.0d-38 20) ;;uses double-float.

|#
	

;;;;;;;;;;;;;;;;;;;FFT!!!;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  
; Fourier Transform Spectral Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;Routines translated with permission by Kevin A. Broughan from ;;;;;;;;;;;
;;Numerical Recipies in Fortran Copyright (c) Numerical Recipies 1986, 1989;;;;
;;;;;;;;;;;;;;;Modified by Ken Olum for Common Lisp, April 1996;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; notes 4/27/05, RJF.  
;; more notes, 2/28/06 RJF. Converting h:/lisp/fft.lisp to OCT arithmetic.
;; slightly rearranged for OCT, 10/29/07  
  
;; see comments at lisp/fft.lisp.  We use four1 only.  The input data
;; will be overwritten by the result.  The inverse fft is indicated by
;; :isign -1, though it must be multiplied by 1/nn to work.
;; functions:
;;	four1: fourier transform (FFT) in one dimension
  
(defparameter *zz* (into 0))
(defparameter *one* (into 1))

(defun v2dfa(a &optional (m (length a))	)
  ;;coerce a vector of oct numbers (or lisp numbers) of length m to a
  ;; oct array of length 2m, since here complex numbers are stored in 2
  ;; adjacent oct locations. Caution: same objects are being used. Do not
  ;; mutate them.
  (let* ((k (length a))
	 (ans (make-array (cl::* 2 m) :allocation :lispstatic-reclaimable ))
	 (zz (copy_mac *zz*));zero
	 (h nil))
    (declare (fixnum k m)(optimize (speed 3)(safety 1)))
    (dotimes (i k)	;just point to the actual numbers, or convert if needed
      (declare (fixnum i))
      (setf (aref ans (cl:* 2 i))
	(if (oct-p (setf h (aref a i))) h (oct:into h))) ;; here we convert.
      ;;(setf (aref ans (cl:1+ (cl:* 2 i))) (copy_mac zz))
      (setf (aref ans (cl:1+ (cl:* 2 i))) zz )); just use zero
    (loop for i fixnum from (* 2 k) to (1-(* 2 m)) do 
	  (setf  (aref ans i) zz)) 
    ans))

;; (v2dfa #(30 40 50) 4)
;;  --> #(0.3Q2 0.Q0 0.4Q2 0.Q0 0.5Q2 0.Q0 0.Q0 0.Q0)


(defmethod ga::outof ((x oct))(oct2lisp x))

(defmethod oct2lisp((x oct))
  (if (excl::nan-p (aref (oct-real x) 0)) (aref (oct-real x) 0)
					; if qd contains a NaN, use it.
      (apply #'cl:+  (map 'list #'rational (oct-real x)))))

(defun dfa2v(a &optional (m (/ (length a)2)))
  ;; Coerce real parts back
  ;; to integers, more or less.  a is an array of even length.  If you
  ;; know that there are trailing zeros, set the actual length with
  ;; the optional second parameter m.
  (let* ((k (/ (length a) 2))
	 (ans (make-array m)))
    (declare (fixnum k m)(optimize (speed 3)(safety 1)))
    (dotimes (i m ans)
      (declare (fixnum i))
      (setf (aref ans i)(round (ga::outof (aref a (cl::* 2 i))) k)))))

;;(dfa2v  (v2dfa #(256 256 256) 4))
;; -->  #(64 64 64 0)  is correct.

;; this works!
(defun polymultfft(r s)
  (declare (optimize (speed 3)))
  ;;compute the size Z of the answer. Z is a power of 2.
  ;; compute complex array r, increased to size S
  ;; compute complex array r, increased to size S
  ;; compute two FFTs
  ;; multiply pointwise
  ;; compute inverse FFT  * 1/n
  ;; convert back to array and return answer
  (let* ((lr (length r))
	 (ls (length s))
	 (lans (cl:+ lr ls -1))
	 (z (ash 1 (ceiling (cl:log lans 2)))) ; round up to power of 2
	 (rfft (four1 (v2dfa r z) z))
	 (sfft (four1 (v2dfa s z) z))
	 (ans (make-array (* 2 z))))
    (declare (fixnum z))
    (dotimes (i (* 2 z))(setf (aref ans i) (copy_mac *zz*)))
    (prodarray rfft sfft z ans)
    (dfa2v(four1 ans z :isign -1) lans)))

(defun polysquare(r) 
  ;;just square the polynomial r. saves one fft. so you can compare times.
  (let* ((lr (length r))
	 (lans (+ lr lr -1))
	 (z (ash 1 (ceiling (log lans 2)))) ; round up to power of 2
	 (rfft (four1 (v2dfa r z) z))
	 (ans (make-array (* 2 z))))
    (prodarray rfft rfft z ans)
    (dfa2v(four1 ans z :isign -1) lans)))

#+ignore
(defun polytime(r s)  ;; this shows that much of the time is in v2dfa etc
  (let* ((lr (length r))
	 (ls (length s))
	 (lans (+ lr ls -1))
	 (z (ash 1 (ceiling (log lans 2)))) ; round up to power of 2
	 (r1 (v2dfa r z))
	 (s1 (v2dfa s z))
	 (sfft 0)
	 (rfft  (four1 r1 z)))
    (start-profiler)
  (time (progn (setf sfft (four1 s1 z))
    (setf sfft (four1 s1 z))
    (setf sfft (four1 s1 z))
    (setf sfft (four1 s1 z))
    (setf sfft (four1 s1 z))))
    (show-flat-profile)
	 ;;(prod (prodarray rfft sfft z ))
	 ;;(ans (time(four1 prod z :isign -1)))
	 )
    ;;(dfa2v ans lans)
    )

#+ignore
(defun polytime2(r)  ;; this shows that much of the time is in v2dfa etc
  (let* ((lr (length r))
	 (lans (+ lr lr -1))
	 (z (ash 1 (ceiling (log lans 2)))) ; round up to power of 2
	 (r1 (v2dfa r z))
	 (start-profiler)
	 (rfft (time (four1 r1 z)))
	 (prod (prodarray rfft rfft z  rfft))
	 (ans (four1 prod z :isign -1)))
    (show-flat-profile)
  (dfa2v ans lans)))

;; Utility to copy an array because this fft will clobber input.
;; We don't use it here, but here's a way to write it.

;;(defun copyarray(a)			; of octs
;;  (map 'array #'(lambda(r)(copy_mac r)) a))

(defun prodarray(r s len ans)
  ;; r and s are the same length arrays
  ;; compute, for i=0, 2, ..., len-2
  ;; ans[i]:=  r[i]*s[i]-r[i+1]*s[i+1] ;; real part
  ;; ans[i+1]:=r[i]*s[i+1]+s[i]*r[i+1] ;; imag part
  (declare (fixnum len)(optimize (speed 3)(safety 1)))
  (let ()
    ;;((ans (make-array (* 2 len))))
  (dotimes (i len ans)
    (let* ((ind (* 2 i))
	   (ind1 (1+ ind))
	   (a (aref r ind))
	   (b (aref r ind1))
	   (c (aref s ind))
	   (d (aref s ind1)))
      (declare ;(type aqd a b c d)
       (fixnum i ind ind1))
       (setf (aref ans ind)(copy(with-temps (- (* a c)(* b d)))))
       (setf (aref ans ind1)(copy(with-temps (+ (* a d)(* b c)))))
   ))))

(defparameter oct2pi (make-oct :real octi::+oct-2pi+))
(defparameter octpi/2 (make-oct :real octi::+oct-pi/2+))

(defparameter onehalf (into 1/2))

;;0.62831853071795864769252867665590057683943387987502116419498891846Q1

;;; this is a fairly generic FFT that works, but not optimized much.
;;; we leave it here just in case you want to copy it for other 
;;; generic arithmetic packages
;;#+ignore  
;; this works, but is not optimized 
(defun copy(x)(if (oct-p x)(copy_mac x)(copy-tree x))) ;; whatever..
#+ignore
(defun four1 (data nn &key (isign 1))
  (declare (type fixnum nn isign))
 (prog ((wr (copy *zz*)) 
	(wi (copy *zz*)) 
	(wpr (copy *zz*))
	(wpi (copy *zz*))
	(wtemp (copy *zz*)) 
        (theta (copy *zz*)) 
	(tempr (copy *zz*)) 
	(tempi (copy *zz*))
	(j 0) (n 0) (m 0) (mmax 0) (istep 0))
   (declare
	;;(type aqd wr wi wpr wpi wtemp theta tempr tempi)
    (fixnum j n m mmax istep))
   
  (setf n (* 2 nn)) 
  (setf j 1) 
  (do ((i 1 (+ i 2)))
      ((> i n) t)
      (declare (fixnum i))
    (when (> j i) 
     (setf tempr (aref data (1- j)))
     (setf tempi (aref data j)) 
     (setf (aref data (1- j)) (aref data (1- i)))
     (setf (aref data j) (aref data i)) 
     (setf (aref data (1- i)) tempr)
     (setf (aref data i) tempi))
    (setf m (floor (/ n 2)))
 label1
    (when (and (>= m 2) (> j m))
     (setf j (- j m)) (setf m (floor (/ m 2)))
     (go label1))
    (setf j (+ j m))) 
  (setf mmax 2) 
 label2 
  (when (> n mmax)
    (setf istep (cl::* 2 mmax))
    (setf theta  (/ oct2pi (* isign mmax)))
    (setf wpr  (* -2 (expt (sin (* 1/2 theta)) 2)))
    (setf wpi (sin theta)) (setf wr (copy *one*)) (setf wi (copy *zz*))
    (do ((m 1 (+ m 2)))
	((cl::> m mmax) t)
      (declare (fixnum  m))
      (do ((i m (+ i istep)))
	  ((> i n) t)
	(declare (type fixnum i))
	(setf j (+ i mmax))
	(setf tempr  (- (* wr (aref data (1- j)))
				    (* wi (aref data j))))
	(setf tempi  (+ (* wr (aref data j))
				    (* wi (aref data (1- j)))))
	(setf (aref data (1- j)) (- (aref data (1- i)) tempr))
	(setf (aref data j) (- (aref data i) tempi))
	(setf (aref data (1- i)) (+ (aref data (1- i)) tempr))
	(setf (aref data i) (+ (aref data i) tempi)))
      (setf wtemp wr)
      (setf wr (+  (* wr wpr) (* (* -1 wi) wpi) wr))
      (setf wi (+  (* wi wpr) (* wtemp wpi) wi)))
    (setf mmax istep)
    (go label2)) 
   (return data)))

;; hacking four1 for speed, keeping space consumption down

(defun four1 (data nn &key (isign 1))
  (declare (type fixnum nn isign) (optimize (speed 3)(safety 1)))
  (prog ((wr (copy_mac *zz*)) 
	 (wi (copy_mac *zz*)) 
	 (wpr (copy_mac *zz*))
	 (wpi (copy_mac *zz*))
	 (wtemp (copy_mac *zz*)) 
	 (theta (copy_mac *zz*)) 
	 (halftheta (copy_mac *zz*)) 
	 (cost (copy_mac *zz*))
	 (tempr (copy_mac *zz*)) 
	 (tempi (copy_mac *zz*))
	 (one (copy_mac *one*))
	 (zero (copy_mac *zz*))
	 (temprx 0) (tempix 0)
	 (j 0) (n 0) (m 0) (mmax 0) (istep 0))
	(declare  (fixnum j n m mmax istep))
	(setf n (cl:* 2 nn)) 
	(setf j 1) 
	(do ((i 1 (cl:+ i 2)))
	    ((cl:> i n))
	  (declare (fixnum i))
	  (when (cl:> j i) 
	    (setf temprx (aref data (cl:1- j)))
	    (setf tempix (aref data j))
	    (setf (aref data (cl:1- j)) (aref data (cl:1- i)))
	    (setf (aref data j) (aref data i)) 
	    (setf (aref data (cl:1- i)) temprx)
	    (setf (aref data i) tempix))
	  (setf m (cl:floor n 2))
	  label1
	  (when (and (cl:>= m 2) (cl:> j m))
	    (setf j (cl:- j m)) (setf m (cl:floor m 2))
	    (go label1))
	  (setf j (cl:+ j m))) 
	(setf mmax 2) 
	label2 
	(when (cl:> n mmax)
	  (setf istep (cl:* 2 mmax))
	  (dsetv theta (into (cl:* isign mmax)))
	  (dsetv theta (/ oct2pi theta))
	  (dsetv halftheta (* 1/2 theta))
	  (octi::sincos-oct-t (oct-real halftheta)(oct-real wpr)(oct-real cost))
	 (dsetv wpi  (* 2(* wpr cost))) ;; 2*sin(t/2)*cos(t/2)
	 (dsetv wpr  (* -2 (* wpr wpr)))
	 (dsetv wr one)
	 (dsetv wi zero)
	  (do ((m 1 (cl:+ m 2)))
	      ((cl:> m mmax) t)
	    (declare (fixnum m))
	    (do ((i m (cl:+ i istep)))
		((cl:> i n) t)
	      (declare (fixnum i))
	      (setf j (cl:+ i mmax))

	      (dsetv tempr  (- (* wr (aref data (cl:1- j)))
			       (* wi (aref data j))))
	      
	      (dsetv tempi (+ (* wr (aref data j))
			      (* wi (aref data (cl:1- j)))))

	      (setf (aref data (cl:1- j)) (- (aref data (cl:1- i)) tempr))
	      ;; (dsetv (aref data (cl:1- j)) (- (aref data (cl:1- i)) tempr))
	      
	      (setf (aref data j) (- (aref data i) tempi))
	      ;;(dsetv (aref data j) (- (aref data i) tempi))
	      
	      (dsetv (aref data (cl:1- i))   
		     (+ (aref data (cl:1- i)) tempr))
	      (dsetv (aref data i) (+  (aref data i) tempi)))
	    (dsetv wtemp wr)
	    (dsetv wr  (+  (* wr wpr) (* (- wi) wpi) wr))
	    (dsetv wi  (+  (* wi wpr) (* wtemp wpi) wi))  )
	  (setf mmax istep)
	  (go label2)) 
    (return data)))
)  

#|  what the answer should be ...
(defun t1()(polymultfft #(1  2 3 4 5 6) #(7 8 9)));; test
(t1)
#(7 22 46 70 94 118 93 54)
(four1 (v2dfa #(1 2 3 4 5 6) 16) 16) ;; except higher precision
#(21.0d0 0.0d0 4.203712543852026d0 17.12548253340269d0
  -9.656854249492387d0 2.999999999999995d0 1.4918055861931159d0
  -4.857754915068683d0 2.999999999999997d0 4.0d0 ...)  ;; works with unoptimized..

(defun randpol(n m) ;; array of ints
  (let ((ans (make-array n))
	(lim (expt 2 m)))
    (dotimes (i n ans)(setf (aref ans i) (random lim)))))

(setf r (randpol 512 332))
(time (progn (polymultfft r r) nil))
(time (progn (polymultfft qr qr) nil))
(setf s (make-array 512 :initial-element 1))
(time (polymultfft s s) ))
;; This is similar in speed to wxmaxima in GCL:
;; s:rat(sum(x^i,i,0, 511),x)$
;; s*s$  ;; time is .6 sec, vs .8 sec for fft.



|#



