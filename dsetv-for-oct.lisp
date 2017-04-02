;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: oct; Base: 10 -*-
;; implementing dsetv and with-temps for OCT
;; modification of similar stuff from RJF's qd.lisp package.
;; must fill in every "etc"

(in-package :oct)
(eval-when (load compile eval)
  (mapc #'(lambda(r)
	    (let ((op (first r))
		  (progname (second r))
		  (octi-targ (third r)))
	  (setf (get op 'argnum) 2)
	  (setf (get progname 'argnum) 2)
	  (setf (get op 'oct-program) octi-targ)
	  (setf (get progname 'oct-program) octi-targ) ;; after macroexpand-all
	  ) )
	'((+ two-arg-+ octi::add-qd-t)
	  (- two-arg-- octi::sub-qd-t)
	  (* two-arg-* octi::mul-qd-t)
	  (/ two-arg-/ octi::div-qd-t) ;;etc
	      ))
  
  ;; one-arg functions
   (mapc #'(lambda(r)
	    (let ((op (first r))
		  (progname (second r))
		  (octi-targ (third r)))
	  (setf (get op 'argnum) 1)
	  (setf (get progname 'argnum) 1)
	  (setf (get op 'oct-program) octi-targ)
	  (setf (get progname 'oct-program) octi-targ) ;; after macroexpand-all
	  ) )
	 '((sin one-arg-sin octi::sin-qd-t)
	   (cos one-arg-cos octi::cos-qd-t)
	   (tan one-arg-tan octi::tan-qd-t)
	   (sqrt one-arg-sqrt octi::sqrt-qd-t)
	   (sqr  one-arg-sqr  octi::sqr-qd-t) ;square
	   ;;etc
	      ))
   
  ;; we might wish to distinguish expt from power [integer exponent]
  

;; until there are real target versions of sin, cos, do this.
;; unfortunately this makes the target version strictly slower
;; than the untargeted version, just the reverse of expectation.

(defun octi::sin-qd-t(x targ)(copy-octi (octi::sin-qd x) targ))
(defun octi::cos-qd-t(x targ)(copy-octi (octi::cos-qd x) targ))
(defun octi::tan-qd-t(x targ)(copy-octi (octi::tan-qd x) targ))
;;(defun octi::sqrt-qd-t(x targ)(copy-octi (octi::sqrt-qd x) targ))

;;etc


;; this works only if we have a full oct compiler environment, which we need if the rest of this is to work.

(defmacro dsetv
    (targ ex)
  ;; try  (dsetv a (+ b qc)) 
  ;; should be faster than (setf a (+ b c)). maybe 2X.
  ;; All the logic below is done during macro-expansion,
  ;; which means it is usually done at compile time. Run time
  ;; is therefore not penalized for macro-expansion, and benefits because
  ;; temporaries are allocated once, regardless of how many times the
  ;; program is executed.  If you use dsetv from an interpreted
  ;; program it will be slow, however, because it will do the macro
  ;; expansion followed by the execution, each time it is used.
  ;;(setf ex (macroexpand ex))  
  ;; rjf's qd version expanded (+ a b c) into (two-arg-+ a (two-arg-+ b c))
  ;; Not so for oct, which leaves + as a function, but defines a compiler macro.
  ;;so we do this
  (if (consp ex)
  (let ((cmf (compiler-macro-function (car ex))))
    (if cmf (setf ex (funcall cmf ex nil)))) ) ;
  
 ;; (format t "~% expanding ex=~s" ex)
  
  (cond 
   ((atom ex) `(into ,ex ,targ))
   ((eq (car ex) 'into) `(into ,@(cdr ex)  ,targ))
   ;; put in extra clauses here as we figure them out
   ;;((eq (car ex) 'progn) `(progn ,))
   
   (t 
    (let* ((op (car ex))
	   (args (cdr ex))
	   (the-op (get op 'oct-program))
	   (argnum (get op 'argnum)))
      (cond 	       
       ((not the-op);; not a previously listed op
	`
	   (let* ((lval ,targ)
		  (a1 (qd-value  (,op ,@ args)))
		  (tt (qd-value lval)))
	     (declare (optimize speed)
		      (type (simple-array double-float (4)) a1 tt))
	     (copy-octi a1 tt)
	     lval))
       ((not (eql argnum (length args))) 
	(error "dsetv was given operator ~s which expects ~s args, but was given ~s --  ~s" 
	       op argnum (length args) args))
       (t
	(case argnum
	  (1;; one argument.
	   `(let ((a1 (qd-value ,(macroexpand `(with-temps ,(car args)))))
		    ;;(a1 (qd-value ,(car args)))
		    (tt (qd-value ,targ)))
		(declare (optimize speed)(type (simple-array double-float (4)) a1 tt))
		;; could also check other args for being type qd
		;; could also allow for args to be si, ui, dd, etc.
		(,the-op a1 tt)
	      ,targ))
	  (2
	   `(let ((a1 (qd-value ,(macroexpand `(with-temps ,(car args)))))

		  (a2 (qd-value ,(macroexpand `(with-temps ,(cadr args)))))

		  (tt (qd-value ,targ)))
		(declare (optimize speed)(type (simple-array double-float (4)) a1 a2 tt))
		(,the-op a1 a2 tt)
		,targ
		))
	  (otherwise (error "argnum is wrong for op ~s " op))
	  )))))))


;; If this next line works at compile time, we might have a chance
;; of compiling this whole file. 
(defconstant +oct1+ #q1)  


;;;;;;;;;;;;;;;;;;more efficiency hackery follows.;;;;;;;;;;;;;;;;


;; we allocate a few private "registers" for a function, and re-use
;; them each time the function is call. This is helpful especially if
;; we are in a loop. No need to tell anyone else about a few temp
;; locations, especially if they are GC'd when truly inaccessible.
;; That's what is below.


(defmacro with-temps(expr)
  (let ((*names* nil)
	(*howmany* 0))
  ;;  (format t "~% with-temps expanding ~s " expr)
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
			   ;; just a symbol name? maybe aref? better be the right type, a qd.
			   (t  r))))
      (if (consp expr)
      (let ((cmf (compiler-macro-function (car expr))))
	(if cmf (setf expr (funcall cmf expr nil)))))
      
      (setf expr (macroexpand expr))
      
       (ct1 expr)
     ;; (ct1 expr); count the temporaries
    (setf *names* (genlist *howmany*))
    (maketemps expr))))

(defun into(x &optional  ;; this is used by dsetv, translated into rtoy/oct
	      (targ (make-instance 'qd-real :value (make-qd-d 0d0))))
  (cond ((typep x 'oct::qd-real)
	 (copy-oct x targ) targ)
	;; this is a stopgap. maybe make it smarter with qfloat-t?
	(t (into (qfloat x +oct1+) targ))))

(defun copy-oct (from to)
  (let ((inf (qd-value from))
	(int (qd-value to)))
    (copy-octi inf int)
    to))

(defun copy-octi(inf int)
  (dotimes (i 4 nil)(declare (optimize (speed 3)(safety 0))(fixnum i))
    (setf (aref int i)(aref inf i))))

    
); end of eval-when

;;An attempt to use these for a trig function, in this case 
;; modeled after sincos-taylor in octi.
		  
;;; assumes |a| <  pi/2048
;;new version, uses qd-reals !!!
;; sets sin and cos values 
;; always returns nil
(defun sincos-taylor-t (a sint cost)  ;; with 2 targets for sin and cos

  (let ((thresh (cl:* octi::+qd-eps+ (abs (qd-0 (qd-value a))))))
    (when (zerop a)
      (dsetv sint 0)
      (dsetv cost 1)
      (return-from sincos-taylor-t nil))
    (let* ((x (with-temps (- (* a a))))
	   (s #q2);will be overwritten
	   (p #q3);will be overwritten
	   (m 1d0))
      (declare (double-float m))
      (dsetv p a)
      (dsetv s a)
      (loop
	 (dsetv p (* p x))
	 (setf m (cl:+ m 2.0d0))
	;;(dsetv p (/ p (into (* m (- m 1))))) ;; this line could be done better with div-qd-d
	(let ((pin(qd-value p)))
	  (oct-internal::div-qd-d-t pin (cl:* m (cl:1- m)) pin))
	
	 (dsetv s (+ s p))
	;;(format t "p = ~A~%" (qd-0 p))

	(when (<= (abs (qd-0 (qd-value p))) thresh)
	   (return)))
      ;; cos(c) = sqrt(1-sin(c)^2).  This seems to work ok, even
      ;; though I would have expected some round-off errors in
      ;; computing this.  sqrt(1-x^2) is normally better computed as
      ;; sqrt(1-x)*sqrt(1+x) for small x.
      (dsetv sint s)
      (dsetv cost (sqrt (- 1 (sqr s)))) ;right

      nil)))








