;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;; dcomp, use instead of /or with/  df.lisp
;;  structure for f,d: f is function value, and d derivative, default 0
(in-package :user)

(defstruct (df (:constructor df (f &optional (d 0)))) f d )
(defmethod print-object ((a df) stream)(format stream "<~a, ~a>" (df-f a)(df-d a)))

(defun treefloat(x) ;; convert all numbers to double-floats
		    (cond ((null x) nil)
			  ((numberp x)(coerce x 'double-float))
			  ((atom x) x)
			  (t (mapcar #'treefloat x))))
;;; dcomp stuff

(defvar *difprog* nil);; the text of the program
(defvar *bindings* nil) ;; bindings used for program

(defun dcomp (f x p) 
  ;; the main program
  (let ((*difprog* nil)
	(*bindings* nil))
    (multiple-value-bind
	(val dif)
       (dcomp1 f x p)
      (emit `(values ,val ,dif))
      `(let ,*bindings* 
	 ,(format nil "~s wrt ~s" f x)
	 ,@(nreverse *difprog*)))))

;; In general this might need to be multi-dimensional, depending
;; on the variable  x .
(defvar *lvkd* nil) ;list of variables with known derivs

;; Assist in compilation of a function f and its derivative at a point x=p
;; dcomp1 changes *bindings* and *difprog* as it munches on the expression f.


(defun dcomp1 (f x p)
  (declare (special *v*))
  (cond((atom f)
	(cond ((eq f x) (values p 1.0d0))
	      ((assoc f *lvkd*)		;list-of-variables-with-known-derivs
	       (values f (cdr (assoc f *lvkd*))))
	      ((numberp f)(values f 0.0d0))
	      (t (let ((r (make-dif-name f x)))
		   (push r  *bindings*)
		   (emit `(setf ,r 0.0d0)) ; unnecessary? already bound to 0.0d0
		   (push (cons f r) *lvkd*)
		   (values f r)))))
       (t
	(let* ((op (first f))
	       (program (get op 'dopcomp)))	;otherwise, look up the operator's d/dx
	  (if program
	      (funcall program (cdr f) x p)
	    (error "~% dcomp ~s" f) ;; for now
	    ;; otherwise.. what to do?
	    ;; I would like to end the function here, but we need
	    ;; to do more to handle defdiff functions.
	   	   )))))
		   
(defun gentempb(r)
  (push (gentemp r) *bindings*)
  (car *bindings*))

(defun emit(item)(unless (and (eq (car item) 'setf)
			      (eq (cadr item)(caddr item)))
		   (push item *difprog*)))

;; simple unary  f(x) programs to compile pieces.
;; to make more, follow the template.
;;We define them all with the same template.

(defmacro dr(op s)
  (setf(get op 'dopcomp) ;;define a rule for returning v, d for dcomp
    (dr2 op (treefloat s))))

(defun dr2 (op s) ;; a rule to make rules for dcomp
   `(lambda(l x p)		
		  (cond ((cdr l)
			 (error "~&too many arguments to ~s: ~s" ',op l))
			(t (let ((v (gentempb 'f))
				 (d (gentempb 't)))
			     (multiple-value-bind
				 (theval thedif)
				 (dcomp1 (car l) x p)
			       (emit (list 'setf v (list ',op theval)))
			       (emit (list 'setf d (*chk thedif (subst theval 'x ',s)))))
			     (values v d))))))

;; Here's how the rule definition program works.
;; Set up rules.
(dr tan (power (cos x) -2)) 
(dr sin (cos x))
(dr cos (* -1 (sin x)))
(dr asin (power (+ 1 (* -1 (power x 2))) -1/2))
(dr asin (power (+ 1 (* -1 (power x 2))) -1/2))
(dr acos (* -1 (power (+ 1 (* -1 (power x 2))) -1/2)))
(dr atan (power (+ 1 (power x 2)) -1))
(dr sinh (cosh x))
(dr cosh (sinh x))
(dr log (power x -1))
(dr exp (exp x))
(dr sqrt (* 1/2 (power x -1/2)))
;; etc etc
;; What can't we do this way? Consider --
;;(dr 'gamma '(* (psi 0 x) (gamma a)))  
;; fails on 2 grounds. No built-in (gamma number) evaluation
;; also, psi has 2 arguments. Need to handle partial derivatives!
;; perhaps extend dr to handle cases like
;; (dr '(f u v w ) a b c) 

;; Next, functions of several arguments depending on x need some thought.
;; These include + - * / expt.

;;derivative operators for compilation
(setf (get '* 'dopcomp)  '*rule)
(setf (get '+ 'dopcomp)  '+rule)
(setf (get '/ 'dopcomp)  '/rule)
(setf (get '- 'dopcomp)  '-rule)
(setf (get 'power 'dopcomp) 'power-rule)
(setf (get 'expt 'dopcomp)  'expt-rule)

(defun +rule (l x p)
  (let ((valname (gentempb 'f))
        (difname (gentempb t)))
   (multiple-value-bind (v d) (dcomp1 (car l) x p)
                   (emit `(setf ,difname ,d ,valname ,v)))
    (dolist (i (cdr l))(multiple-value-bind (v d) (dcomp1 i x p)
                   (emit `(setf ,difname ,(+chk d difname)))
                   (emit `(setf  ,valname ,(+chk v valname)))))
    (values valname difname)))

(defun -rule (l x p)
  (let ((valname (gentempb 'f))
        (difname (gentempb t)))
    (cond ((cdr l)
	   (multiple-value-bind (v d) (dcomp1 (car l) x p)
	     (emit `(setf ,difname ,d ,valname ,v)))
	   (dolist (i (cdr l))(multiple-value-bind (v d) (dcomp1 i x p)
				(emit `(setf ,difname (- ,difname ,d)))
				(emit `(setf  ,valname (- ,valname ,v ))))) )
	  ;; just one arg
	  (t (multiple-value-bind (v d) (dcomp1 (car l) x p)
	       (emit `(setf ,difname (- ,d) ,valname (- ,v))))))
    (values valname difname)))
  

;; (- x y z) means (+ x (* -1 y) (* -1  z)).  (- x) means (* -1 x)

(defun *rule (l x p)
  (let ((valname (gentempb 'f))
	(difname (gentempb t)))
    (multiple-value-bind (v d) (dcomp1 (car l) x p)
		   (emit `(setf ,difname ,d ,valname ,v)))
    (dolist (i (cdr l))(multiple-value-bind (v d) (dcomp1 i x p)
		   (emit `(setf ,difname ,(+chk (*chk v difname)
						(*chk d valname))))
		   (emit `(setf 
				,valname ,(*chk v valname)))))
    (values valname difname)))


(defun /rule (l x p)
  (let ((vname (gentempb 'f))
	          (dname (gentempb t)))
	      (multiple-value-bind (v d) (dcomp1 (car l) x p)
		(emit `(setf ,dname ,d ,vname ,v)))
	      (multiple-value-bind (v d) (dcomp1 (cadr l) x p)
		(emit `(setf ,dname (/ (- (* ,v ,dname)(* ,vname ,d))
				       (* ,v ,v))
			     ,vname (/ ,vname ,v))))
	      (values vname dname)))


(defun power-rule (l x p)
  (cond ((cddr l) (error "~&too many arguments to power: ~s" l))
	;; now we assume that everything is a-ok
	;; i.e. power has only two arguments, the second argument
	;; which is the power to be raised to is independent of x
	;; so we can use for sqrt, cube-root etc.
	(t  (let ((vname (gentempb 'f))
	          (dname (gentempb t)))
	      (multiple-value-bind (v d) (dcomp1 (car l) x p)
		;;; We could use +chk and *chk in the following emissions
		;;; but they are really inessential.
	      (emit `(setf ,vname (power ,v ,(cadr l))))
	      (emit `(setf ,dname (* ,d (power ,v (1- ,(cadr l))) ,(cadr l))))) (values vname dname)))))

(defun expt-rule (l x p)
  "this is the general power rule where the exponent could be an 
    arbitrary function of x"
  (cond ((cddr l) (error "~&too many arguments to expt : ~s" l))
	((numberp (cadr l))(power-rule l x p))
	;; we had z = (expt f g)
	;; z' = z*(g*log f)' = z*(g*f'/f + g'*log f)
	(t  (let ((vname (gentempb 'f))
	          (dname (gentempb t)))
	      (multiple-value-bind (v d) (dcomp1 (cadr l) x p)
		(emit `(setf ,vname ,v ,dname ,d)))
	      (multiple-value-bind (v d) (dcomp1 (car l) x p)
		(emit `(setf ,dname ,(+chk `(/ ,(*chk vname d) ,v)
					   (*chk dname `(log ,v)))))
		(emit `(setf ,vname (expt ,v ,vname)))
		(emit `(setf ,dname ,(*chk vname dname))))
	      (values vname dname)))))


;; the next two programs, "optimize": 
;; generated code so as to not add 0 or mult by 0 or 1
(defun +chk (a b)(cond ((and (numberp a)(= a 0)) b)
		       ((and (numberp b)(= b 0)) a)
		       (t `(+ ,a ,b))))

(defun *chk (a b)(cond ((and (numberp a)(= a 1)) b)
		       ((and (numberp b)(= b 1)) a)
		       ((and (numberp a)(= a 0)) a)
		       ((and (numberp b)(= b 0)) b)
		       (t `(* ,a ,b))))

;;;; this is the main program for compiling

(defun dc (f x &optional (otherargs nil)) 
  ;; produce a program, p(v) ready to go into the compiler to
  ;; compute f(v), f'(v), returning result as a structure, a df
  (let ((*difprog* nil)
	(*bindings* nil)
	(*lvkd* (list '(pi . 0))) ; for example
	(*subexp* (make-hash-table :test #'equal))
        (*v* (gentemp "g")))
    (declare (special *v*))
    (multiple-value-bind
	(val dif)
	(dcomp1 f x *v*)
      (emit `(df ,val ,dif))
      `(lambda (,*v* ,@otherargs)
	 	 ,(format nil "~s wrt ~s" f x)
	 ;;; comment these declares  if you prefer v's type to be unknown
	  (declare (double-float ,*v*))
	  (declare (optimize (speed 3)(debug 0)(safety 0)))
	  ;    (assert (typep ,*v* 'double-float))
	 (let ,(mapcar #'(lambda (r)(list r 0d0)) *bindings*)
	 (declare (double-float ,@*bindings*))
	   ,@(nreverse *difprog*))))))

;; A plausible way to use it is as follows:

(defmacro defdiff (name arglist body) ;; put the pieces together
  (progn
    (setf (get name 'defdiff) name)
  (let ((r (dc body (car arglist) (cdr arglist)))) ;; returns a df.
    `(defun  ,name , (cadr r) ,@(cddr r)))))

;; usage (defdiff f (x) (* x (sin x)))

;; Then you can call (f 3.0d0)

(defun if-rule (l x p)
  (let ((valname (gentempb 'f))
        (difname (gentempb t)))
    (emit `(multiple-value-setq
	       (,valname ,difname)
	     ;; assume only variables, not derivatives in condition
	     (if ,(subst p x (car l))
		 (funcall (function ,(dc (cadr l) x)) ,p)
	       (funcall (function ,(dc (caddr l) x)) ,p))))
    (values valname difname)))

(setf (get 'if 'dopcomp)	; if
  'if-rule)

(setf (get 'progn 'dopcomp)  'progn-rule)

(defun progn-rule(l x p)
  (mapc #'(lambda (k)(dcomp1 k x p)) (butlast l))
  (dcomp1 (car (last l)) x p))

(setf (get 'setf 'dopcomp)  'setf-rule)

(defun setf-rule(l x p)
  (if (not(symbolp (car l)))(error "dcomp cannot do setf ~s" (car l)))
  (multiple-value-bind (v d)
      (dcomp1 (cadr l) x p)
      (emit `(setf ,(car l) ,v))
      (let ((dname (make-dif-name (car l) x)))
	(push dname *bindings*)		;**
	(emit `(setf ,dname ,d))
	(push (cons (car l) dname) *lvkd*))
      (values v d)))

(defun make-dif-name(s x) ;s is a symbol. make a new one like s_dif_x
 (intern (concatenate 'string (symbol-name s) "_DIF_" (symbol-name x))))




