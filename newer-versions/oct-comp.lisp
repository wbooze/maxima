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
       (oct_copy_into  (oct-real ,gg) (oct-real ,(cadr ex)))
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
	     (oct_copy_into a1 tt)
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
;; or  (defun hypot(x y)(copy (with-temps (sqrt (+ (* x x)(* y y))))))
;; need the call to copy to make a  copy of the result before calling hypot again. 

;; set up the environment for with-temps and dsetv here

(eval-when (compile load eval)
  (mapc #'(lambda(h) (setf (get h 'argnum) 2)) '(atan2 log2 setq))
;; for now assume they are given only one arg and if they are given 2 signal an error with dsetv.
  (mapc #'(lambda(h) (setf (get h 'argnum) 1)) '(atan log))
;;  (defun qd_setq (a b)(qd_copy_into b a) b)
 )

;;;;;;;;;;;::::There's a lot more in generic/qd.lisp
;;;;;;;;;not yet converted to OCT.
