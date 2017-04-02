;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ma; Base: 10 -*-
;;  A basis for a simple mathematical expression program 
;;  constructed by overloading generic arithmetic. A simple computer
;;  algebra system (CAS). Basic idea: by overloading +, *, etc the ops can use
;;  symbols, not just numbers. The data type ma  is used for math expressions
;;  Use  (ma 'x)   or the "syntactic sugar" %x to create a math object x.
;;  Richard Fateman, November, 2005

(eval-when '(:load-toplevel) (require "ga")(require "simpsimp") (require "p2i")(require "df"))
(in-package :ma)

;; define the structure for a math expression.
(defstruct (ma (:constructor ma (e)))e (simp nil))
;; fundamentally, ma is any lisp expression, just with an "ma" tag.
;; we can add other info, e.g. "simp" flag.
;; ma objects will be printed in an infix form by p2i, with this
;; default print-object method.  To see the "inside" lisp, of u,  try (ma::ma-e u).
;; p2i is defined in the file p2i.lisp

(defmethod print-object ((a ma) stream)(format stream "~a" (p2i (ma-e a))))

;;A read macro hack to establish the syntactic sugar %x. You can also do %(+ x y z).
;; ;;  %(+ foo bar)  is the same as (ma '(+ foo bar))
(set-macro-character #\% #'(lambda(st char)(declare (ignore char))(ma (read st t nil t))))

;; in a manner similar to the code in ga.lisp, we define some
;; routines to help use define the two-arg-X operations, this time
;; on ma objects, or mixed ma+number objects.

(defmacro defarithmetic (op)
  (let ((two-arg
	 (intern (concatenate 'string "two-arg-" (symbol-name op))
		 :ga )) )
    `(progn
       ;; new defmethods for ma. 
       (defmethod ,two-arg ((arg1 ma) (arg2 ma))
	 (ma (list ',op (ma-e arg1)(ma-e arg2))))
       (defmethod ,two-arg ((arg1 number) (arg2 ma))
	 (ma (list ',op arg1 (ma-e arg2))))
       (defmethod ,two-arg ( (arg1 ma)(arg2 number))
	 (ma (list ',op (ma-e arg1) arg2)))
       (compile ',two-arg)
       (compile ',op)
       ',op)))

(defarithmetic +)
(defarithmetic -)
(defarithmetic *)
(defarithmetic /)
(defarithmetic expt)


;; A rule to define rules for ma only
(defmacro r (op s) ;;
  `(progn
    (defmethod ,op ((a ma))
      (ma (list ',op (ma-e a))))
    (setf (get ',op 'diff)'(lambda(x) ,s))  ; store s, the derivative, unused for now.
    ))

;; add as many rules as you can think of here.
;; should insert them in the shadow list too.

(r sin (cos x))  
(r cos (* -1 (sin x)))
(r tan (expt (cos x) -2))
(r asin (expt (+ 1 (* -1 (expt x 2))) -1/2))
(r acos (* -1 (expt (+ 1 (* -1 (expt x 2))) -1/2)))
(r one-arg-atan (expt (+ 1 (expt x 2)) -1))
(r sinh (cosh x))
(r cosh (sinh x))
(r atanh (expt (1+ (* -1 (expt x 2))) -1))
(r one-arg-glog (expt x -1))
(r exp (exp x))
(r sqrt (* 1/2 (expt x -1/2)))
(r abs x) ;; not always
(r 1-  1)
(r 1+  1)

;; equality needs to be specified. Order predicates don't make sense.
(defmethod two-arg-=((a ma) (b ma)) (cl::eql (ma-e a) (ma-e b)))
(defmethod two-arg-=((a ma) (b t)) (cl::eql (ma-e a) b))
(defmethod two-arg-=((a t) (b ma)) (cl::eql (ma-e b) a))
(defmethod two-arg-=((a t) (b t)) (cl::eql a b))
(defmethod two-arg-/=((a t) (b t)) (not (two-arg-= a b)))

;; a way to declare functions of multiple arguments..
;; at some future time, use ma::nr

(defmacro nr (op) ;;
  `(progn
     (defmethod ,(car op) ,(cdr op) 
		(ma (list ',(car op) ,@(cdr op) )))))

;; (nr (foo3 a b c)) ; then we can
;; use foo3 as a generic function
;; for example,   (foo3 1 (+ %x 3 4) 5)
;; instead of    %(foo3 1 (+ %x 3 4) 5)

;;(nr (atan2 x y)) ;; define atan2 generally 
;; but define it for numbers this way, as cl::atan 
;;(defmethod atan2 ((a number)(b number))(cl::atan a b))

;; how to deal with unknown functions of arb. number of args? e.g.
;;(defun bar(&rest x)(ma (list 'bar (cons 'seq x))))
;;(defun seq(&rest x) (cons 'seq x))
		
;; This next piece may be useless, but in :user package, try 
;; (meval '(+ 3 x  y(* 5 6))) -->  30+((3+x)+y)

(defun cl-user::meval(r)  (ma (ga::simp(ma-e(ga::eval (ga::re-intern r :ga))))))

;;;;; The rest of this file looks at how to interface with
;; two  simplifiers. One rudimentary simplifier is in the file simpsimp.lisp

;;Here's the interface:  
;; This one destroys the unsimplified version
(defmethod gasimp((x ma))(if (cl::eq (ma-simp x) 'gasimp) ; already simped
			     x
			   (let ((r(ga::simp (ma-e x))))
			     (setf (ma-e x) r)
			     (setf (ma-simp x) 'gasimp)
			     (if (numberp r) r x) ; demotes numbers? or just x?
			     )))
(defmethod gasimp((x t)) x)		; not a math object? don't simplify it.
(defmethod gasimp((z df::df))		;how to simplify df of math objects
    (df::df (gasimp(df::df-f z))(gasimp(df::df-d z))))

(defmethod ga::ratsimp((x ma))		;ratsimp is a more powerful simplifier for polys
  (if (cl::eq (ma-simp x) 'ratsimp)	; already simped
      x
    (let ((r(ratsimp (ma-e x))))	;ratsimp defined in rat1.lisp and dependent files
      (setf (ma-e x) r)
      (setf (ma-simp x) 'ratsimp)
      (if (numberp r) r x)		; demotes numbers? or just x?
      )))

(defmethod ratexpand((x ma))		;ratsimp is like ratsimp, using distr. law
  (if (cl::eq (ma-simp x) 'ratsimp)	; already simped
      x
    (let* ((r (ratexpand(ma-e x))))
      (setf (ma-e x) r)
      (setf (ma-simp x) 'ratsimp)
      (if (numberp r) r x)		; demotes numbers? or just x?
      )))

(defmethod ratexpand((z df::df))
    (df::df (ratexpand(df::df-f z))(ratexpand(df::df-d z))))

(defmethod ratsimp((x ma))
  (if (cl::eq (ma-simp x) 'ratsimp)	; already simped
      x
    (let ((r(ratsimp (ma-e x))))
      (setf (ma-e x) r)
      (setf (ma-simp x) 'ratsimp)
      (if (numberp r) r x)		; demotes numbers? or just x?
      )))

(defun simp(x)(gasimp x))

(defmethod unratsimp((x ma))		;unset the simp flag of x
  (setf (ma-simp x) nil)
  x)
;; examples
(defun ex(x)(ex1 x 1 15))

(defun ex1(x i lim)  ;; generate Taylor summation exactly for exp()
  (if (= i lim) 0 (+ 1 (* x  (ex1 x (+ i 1) lim)(expt i -1)))))

(defun p(m x)(cond ((= m 0) 1)
		   ((= m 1) x)
		   (t (*  (/ 1 m) (+ (* (- (* 2 m) 1) x (p (- m 1) x))
				     (* (- 1 m) (p (- m 2) x)))))))

;; that formula in infix form  "(1/m)*((2*m-1)*x*p(m-1,x)+(1-m)*p(m-2,x))"
;; consider (gasimp (p 4 %x))
;; (ratexpand *)  yields the nicer form
;; (1/8)*(3+(-30)*x^2+35*x^4)

;; How to facilitate the insertion of lisp information into math data..  
;; Let us say that you have constructed a math object, say x+y+45, and
;; you wish to impose on it some information, that x should be
;; replaced by 3,  or perhaps by y-45.

;; (setf target %(+ x y 45))
;; (masubst 3 %x target)
;; (masubst %(+ y -45) %x target)

(defmethod masubst ((new ma) (old ma) (targ ma))
  (simp (ma (subst (ma-e  new)(ma-e old)(ma-e targ) :test #'equal))))

(defmethod masubst ((new t) (old ma) (targ ma))
  (simp (ma (subst new (ma-e old)(ma-e targ) :test #'equal))))

;; this next method is not as easy to characterize semantically.  It
;; looks at Lisp bindings and tries calling functions again.

(defmethod meval ((targ ma))
 (simp   (meval1 (ma-e targ))))

(defun meval1 (s)(cond((numberp s) s)
		      ((symbolp s)(if (boundp s) (ma (symbol-value s)) (ma s)))
		      ((ma-p s) (ma (meval1 (ma-e s))))
		      (t (apply (car s)(mapcar #'meval1 (cdr s))))))

;; e.g. try (setf x 3) (meval target)












