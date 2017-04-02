;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: octi; Base: 10 -*-
;; implementing dsetv and with-temps for OCTI  (not oct)

;; started with dsetv version of dec 6, 2007
;; probably should feed back the changes with block, progn, etc.

;; probs?  what to do with  (macroexpand '(dsetv w (let ((x 3)(y 4))(+ x y))))
;; an error?
;;  (macroexpand '(dsetv w (let ((x (into 3))(y (into 4)))(+ x y))))
;; works better.


(in-package :octi)
(eval-when (load compile eval)
  (mapc #'(lambda(r)
	    (let ((op (first r))
		  (progname (second r))
		  (octi-targ (third r)))
	  (setf (get op 'argnum) 2)
	  (setf (get progname 'argnum) 2)
	  (setf (get op 'octi-program) octi-targ)
	  (setf (get progname 'octi-program) octi-targ) ;; after macroexpand-all
	  ) )
	
	;; depending on whose package you are using, either +  or two-arg-+
	;; or maybe excl::+_2op
	;; might appear, after macro-expansion.
	'((+ two-arg-+ add-qd-t)
	  (excl::+_2op two-arg-+ add-qd-t)
	  (- two-arg-- sub-qd-t)
	  (excl::-_2op two-arg-- sub-qd-t)
	  (* two-arg-* mul-qd-t)
	  (excl::*_2op two-arg-* mul-qd-t)
	  (/ two-arg-/ div-qd-t)
	  (excl::/_2op two-arg-/ div-qd-t) ;;etc
	  (excl::>_2op two-arg-> >-qd-t)  ;; see below

	      ))
  
  ;; it's not so neat to have  (excl::>_2op two-arg-> >-qd-t)
  ;; because the target isn't an octi, nor is the target necessary at all.
  ;
  
  ;; we need another class of programs: 2 arg functions returning true/false.
  ;; more leg work.
  ;; or just do this:
  
  (defun >-qd-t(a b ignore)
    (declare(ignore ignore))
    (or (> (aref a 0)(aref b 0))
	(and (=  (aref a 0)(aref b 0))
	     (> (aref a 1)(aref b 1)))
					;etc
	))

  
  
  ;; one-arg functions
   (mapc #'(lambda(r)
	    (let ((op (first r))
		  (progname (second r))
		  (octi-targ (third r)))
	  (setf (get op 'argnum) 1)
	  (setf (get progname 'argnum) 1)
	  (setf (get op 'octi-program) octi-targ)
	  (setf (get progname 'octi-program) octi-targ) ;; after macroexpand-all
	  ) )
	 '(
	   ;; controversy: should sqrt/sin/cos/tan
	   ;; be supported by dsetv in compiled mode, or just
	   ;; by consing up the data type?
	   ;; if supported, we must implement sin-qd-t etc in octi.
	   ;; if unsupported, all we need is sin  in quad-real
	   ;; it also means that sqrt can return a quad-complex.
	   ;;etc
	      ))
   
  ;; we might wish to distinguish expt from power [integer exponent]
  

;; this works only if we have a full oct compiler environment, which we need if the rest of this is to work.

(defmacro dsetv
  (targ ex)
  ;; try  (dsetv a (+ b c)) 
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
  (setf ex (cmexpand ex))		;check for compiler macro, too
  
   ;(format t "~% expanding ex in dsetv, ex=~s" ex)
  
  (cond 
   ((atom ex) `(into ,ex ,targ))
   ((eq (car ex) 'into) `(into ,@(cdr ex)  ,targ))
   (t ;;(setf ex (cmexpand `(with-temps ,ex)))
    ;;(setf ex (cmexpand ex))
    (let* ((op (car ex))
	   (args (cdr ex))
	   (the-op (get op 'octi-program))
	   (argnum (get op 'argnum))
	   ;;(theval (cmexpand (with-temps `(,op ,@ args))))
	 ;;  (theval (cmexpand (with-temps  ex)))
	   )
      (cond 	       
       ((not the-op);; not a previously listed op
	`(copy-octi ,(cmexpand `(with-temps ,ex)) ,targ))
       ((not (eql argnum (length args))) 
	(error "dsetv was given operator ~s which expects ~s args, but was given ~s --  ~s" 
	       op argnum (length args) args))
       (t
	(case argnum
	  (1;; one argument.
	  ;; (format t "~% one arg ~s" args)
	   `(,the-op  ,(cmexpand `(with-temps ,(car args))) , targ))

	  (2
	   

	      `(,the-op ,(cmexpand `(with-temps ,(car args)))
		       ,(cmexpand `(with-temps ,(cadr args)))   ,targ))

	  (otherwise (error "argnum is wrong for op ~s " op))
	  )))))))

(defmacro with-temps(expr)
  (let ((*names* nil)
	(*howmany* 0))
     ;; (format t "~% with-temps expanding ~s " expr)
    (labels ((genlist(n)(loop for i from 1 to n collect (into i)));make a list of fresh qd items
	     (ct1 (r);; count temporaries needed
		  (cond ((numberp r) (incf *howmany*))
			((not (consp r)) r)
			(t (incf *howmany*)
			   (mapc #'ct1 (cdr r)))))
		
	     (maketemps(r)		;change r=(+ a (* b c)) to  temp storage .
	       
	    ;;   (format t "~% expand with-temps for r=~s" r)
		       (cond ((numberp r) (into r))
			     ((atom r) r)
			     ((get (car r) 'argnum); known operator
			      `(dsetv ,(pop *names*)
				      ,(cons (car r)(mapcar #'maketemps (cdr r)))))
			     ;; just a symbol name? maybe aref? better be the right type, a qd.
			   
			     ((member (car r) '(let let*))
   
			      (cons (car r); let or let*
				    (cons (mapcar 
					   #'(lambda(x); pair: name, value
					       (list (car x); variable
						     ;;(cmexpand `(with-temps ,(cadr x)))
						     (cmexpand (cadr x))
						     
						     ))
					   (cadr r))
					  (mapcar #'(lambda(x)
						      (cmexpand `(with-temps ,x)))(cddr r))
	       
					  )))
			     ((member (car r) '(block tagbody if))
			     ;; (format t "~%block, tagbody if =~s" r)
			       
				   
					  (mapcar #'(lambda(x)
						      (cmexpand `(with-temps ,x))) r ))
	       
			      
			     ((member (car r) '(setq setf progn block tagbody psetq))
				      (cons (car r); setq, setf, progn. expand everything after.
					  (mapcar #'(lambda(x)
						      ;;(cmexpand `(with-temps ,x))
						      (cmexpand x)
						      )
						  (cdr r))
	       
					  ))
				     
			     (t  r))))
      
      (setf expr (cmexpand expr))
      
      (ct1 expr)
      ;; (ct1 expr); count the temporaries
      (setf *names* (genlist *howmany*))
      (maketemps expr))))

(defun copy-octi(inf int)
  (dotimes (i 4 nil)(declare (optimize (speed 3)(safety 0))(fixnum i))
    (setf (aref int i)(aref inf i))))

(defun cmexpand(ex)  ;; compiler  macro and regular macro expand
 (if (consp ex)  
  (macroexpand    (let ((cmf (compiler-macro-function (car ex))))
		    (if cmf (funcall cmf ex nil)  ex)))
  ex))

    
); end of eval-when











#| working examples..

octi(17): (macroexpand '(with-temps (let ((x 3)(y 4)) (+ x y))))
(let ((x #(3.0d0 0.0d0 0.0d0 0.0d0)) (y #(4.0d0 0.0d0 0.0d0 0.0d0)))
  (add-qd-t x y #(1.0d0 0.0d0 0.0d0 0.0d0)))


 octi(20): (macroexpand '(dsetv  w (let ((x 3)(y 4)) (+ x y))))
(copy-octi (let ((x #(3.0d0 0.0d0 0.0d0 0.0d0))
                 (y #(4.0d0 0.0d0 0.0d0 0.0d0)))
             (add-qd-t x y #(1.0d0 0.0d0 0.0d0 0.0d0)))
           w)

octi(37): (cmexpand '(with-temps (setf (aref w 3) 2 q 25)))
(progn (let* ((#:g255099 w) (#:g255100 #(2.0d0 0.0d0 0.0d0 0.0d0)))
         (excl::.inv-s-aref #:g255100 #:g255099 3))
       (let* ((#:g255201 #(25.0d0 0.0d0 0.0d0 0.0d0)))
         (setq q #:g255201)))

octi(73):  (pprint (cmexpand '(with-temps (dotimes (i 100) (dsetv w (+ w 1))))))

(block nil
  (let ((i 0))
    (tagbody
      #:Tag75
        (if (>= i 100) (progn (return-from nil (progn))) nil)
        (tagbody (add-qd-t w #(1.0d0 0.0d0 0.0d0 0.0d0) w))
        (let () (setq i (+ i 1)) nil)
        (go #:Tag75))))


;;but you need to do this
;;(with-temps (dotimes (i 100) (dsetv w (+ w (into i)))))
;; to incorporate i into the loop body


[6] octi(69):  (pprint (cmexpand '(with-temps (let ((i 123))(dsetv x (+ x i))))))

(let ((i 123)) (add-qd-t x i x))
[6] octi(70):  (pprint (cmexpand '(with-temps (let ((i (with-temps 123)))(dsetv x (+ x i))))))

(let ((i #(123.0d0 0.0d0 0.0d0 0.0d0))) (add-qd-t x i x))


(pprint (cmexpand '(with-temps (do ((i 1 (1+ i)))((> i 10) 'foo) (dsetv w (+ w i))))));; not good, but see..

(pprint (cmexpand '(with-temps (do ((i (into 1)(with-temps(+ i 1)) ))((with-temps (> i 10)) 'foo) (dsetv w (+ w i))))))

(block nil
  (let ((i (into 1)))
    (tagbody
      #:Tag199
        (if (>-qd-t i #(10.0d0 0.0d0 0.0d0 0.0d0)
                    #(1.0d0 0.0d0 0.0d0 0.0d0))
            (progn (return-from nil (progn 'foo)))
          nil)
        (tagbody (add-qd-t w i w))
        (let ()
          (setq i
                (add-qd-t i #(1.0d0 0.0d0 0.0d0 0.0d0)
                          #(1.0d0 0.0d0 0.0d0 0.0d0)))
          nil)
        (go #:Tag199))))




|#

