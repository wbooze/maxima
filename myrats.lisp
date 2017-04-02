;;  A basis for rational arithmetic with infinities
;;  constructed by overloading generic arithmetic. 
;;  Richard Fateman, November, 2005

(defpackage :ra				;uses generic arithmetic
  (:use :ga :cl)
  (:shadowing-import-from 
   :ga
   "+" "-" "/" "*" "expt"		;binary arith
   "=" "/=" ">" "<" "<=" ">="		;binary comparisons
   "1-" "1+" "abs"
   )
    
  (:export "ri" "union" "intersection"))

(require "ga")
(in-package :ra)

(defstruct (ra (:constructor ra (n d )))n d ) ;structure for rational

(defun into(num  &optional (den  1 denp))
  ;; into takes 2 args, num and denom,  or just one, a number
  (if (not denp) (let ((r (rational num)))
		   (ra (numerator r)(denominator r)))
    ;; there is a denominator
    (let* ((a (rational num))
	   (b (rational den))
	   (n (* (numerator a)(denominator b)))
	   ( d (* (numerator b)(denominator a)))
	   ;(g (gcd n d))
	   )
      (reducera n d))))
 

;;; try (into -5 0) (into 5 0) (into 0 5) (into 1 2) (into 1/2) (into -6 3)
;;; should we make (into 2)  just 2?


      
      
    
  

(defmethod print-object ((a ra) stream)
  (format stream "[~a/~a]"  (ra-n a)(ra-d a)))


;; must figure out ra version of sin, cos, tan, etc.
;;  for infinity and nan.
;; must figure out =, >, <, 

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
	,@body))))
;;; based on...
;;;from Figure 18.3: Destructuring on structures. from On Lisp, P. Graham

;; take 2 rationals and grab their insides.

(defmacro with-ra2 (struct1 struct2 names1 names2 &body body)
  (let ((gs1 (gensym))
	(gs2 (gensym)))
    `(let ((,gs1 ,struct1)
	   (,gs2 ,struct2))
       (let ,(append 
	      (mapcar #'(lambda (f field)
			  `(,f (,(symb "ra-" field) ,gs1)))
		      names1
		      '(n d))
	      (mapcar #'(lambda (f field)
			  `(,f (,(symb "ra-" field) ,gs2)))
		      names2
		      '(n d)))
	 ,@body))))

(defmacro with-ra (struct1 names1 &body body)
  (let ((gs1 (gensym)))
    `(let ((,gs1 ,struct1))
       (let  ,(mapcar #'(lambda (f field)
			  `(,f (,(symb "ra-" field) ,gs1)))
		      names1
		      '(n d))
	 ,@body))))

(defmethod ga::two-arg-+ ((r ra)(s ra))
  ;; adding 2 rationals,  a/b+c/d --> (a*d+c*b)/(b*d), reduced
  (with-ra2 r s (a b)(c d) 
	    (reducera (+ (* a d)(* c b))
		      (* b d))))

(defmethod ga::two-arg-+ ((r ra)(s integer))
  ;; adding 2 rational+ number,  a/b+s --> (a+s*b)/b, reduced
  (with-ra r (a b)
	    (reducera (+ a(* s b))
		      b)))

(defmethod ga::two-arg-+ ((s integer)(r ra))
  ;; adding 2 rational+ number,  a/b+s --> (a+s*b)/b, reduced
  (with-ra r (a b)
	    (reducera (+ a (* s b))
		  b)))

(defun reducera (num den)
 (let  ((comfac (gcd num den))) ;common factor
   (cond ((= num 0) (if (= den 0) (ra 0 0) 0)) ;; or  (ra 0 1) ?
	 ((= den 0) (ra (signum num) 0))
	 ;;((= comfac 0) (ra (signum num) 0))

	 ((= comfac den) (/ num comfac)) ;; reduce to integer.
	 ((< den 0) (ra (/ (- num) comfac)(/ (- den) comfac)))
	 (t (ra (/  num comfac)(/ den comfac))))))
 
(defmethod ga::two-arg-* ((r ra)(s ra))
  (with-ra2 r s (a b)(c d)
	      (reducera (* a c)
			(* b d))))

(defmethod ga::two-arg-* ((r ra)(s integer))
  (with-ra r  (a b)
	      (reducera (* a s)
			b)))

(defmethod ga::two-arg-* ((s integer)(r ra))
  (with-ra r  (a b)
	      (reducera (* a s)
			b)))

(defmethod ga::two-arg-/ ((r ra) (s integer))
  (* r (ra 1 s)))

(defmethod ga::two-arg-/ ( (s integer)(r ra))
  (with-ra r (a b) (reducera (* s b) a)))

(defmethod ga::two-arg-/ ((r ra) (s ra))
   (with-ra2 r s (a b)(c d)
	      (reducera (* a d)
			(* b c))))


;; need two-arg-- 
;; need to do more stuff for sin cos etc.

(defmethod ga::sin ((s ra))
  (with-ra  s (n d)
	    (if (/= d 0) (cl::sin (/ n d))
	      (error "please fix program for */0 rational  sin ~s" s))))


		      