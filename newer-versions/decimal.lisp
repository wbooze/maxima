;;; -*-  Mode: Lisp; Package: dec; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;  A basis for exact decimal arithmetic, at least for + and *
;;  constructed by overloading generic arithmetic. 
;;  Richard Fateman, September, 2006
;;  just copying over stuff from the interval package, modifying as
;; we go

(require :ga) (provide :dec)

(defstruct (dec (:constructor dec (frac ex cl::&optional form))) frac ex (form nil) )
;;(defstruct (dec (:constructor dec (frac ex) frac ex (form nil))))
;;structure for decimal. 
;; Fraction (a signed integer) exponent (an integer, power of 10)
;; form is an optional format directive for printing.
;; implied decimal point is to the right of the fraction.

(defmethod print-object ((a dec) stream)
  ;; a more elaborate version would look at the format directive.
  ;; doing formatted output ala common lisp could be swiped from some
  ;; open-source system.  Could support
  ;; ~w,d,k,overflowchar,padcharF
  ;; ~w,d,k,overflowchar,padchar,exponentcharE
  ;; ~w,d,k,overflowchar,padchar,exponentcharG
  ;; see section 22.3.3 of ANSI CL standard.

  (format stream "~a.e~a"  (dec-frac a)(dec-ex a)))

;; must figure out dec version of sin, cos, tan, etc.
;; must figure out =, >, <, union, intersection.

;; from Graham, On Lisp, macro hackery
(defun mkstr (&rest args)
  (with-output-to-string (s)(dolist (a args) (princ a s))))

(defun symb (&rest args) (values (intern (apply #'mkstr args))))
(eval-when (compile)
  (defun mkstr (&rest args)
  (with-output-to-string (s)(dolist (a args) (princ a s))))
  (defun symb (&rest args) (values (intern (apply #'mkstr args)))))

(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
      (let ,(mapcar #'(lambda (f)
			`(,f (,(symb name f) ,gs)))
		    fields)
	,@body))));;; e.g. (with-struct (ri- lo hi) r (f lo hi))
;;; based on...
;;;from Figure 18.3: Destructuring on structures. from On Lisp, P. Graham
;;; modified from code in interval file
;; take 2 decs and grab their insides. Then
;; do something with them. sample usage...
;;(with-dec2 dec1 dec2 (frac1 ex1)(frac2 ex2) 
;;  (dec (compute-new-fraction)(compute-new exponent)))
(defmacro with-dec2 (struct1 struct2 names1 names2 &body body)
  (let ((gs1 (gensym))
	(gs2 (gensym)))
    `(let ((,gs1 ,struct1)
	   (,gs2 ,struct2))
       (let ,(append 
	      (mapcar #'(lambda (f field)
			  `(,f (,(symb "dec-" field) ,gs1)))
		      names1
		      '(frac ex))
	      (mapcar #'(lambda (f field)
			  `(,f (,(symb "dec-" field) ,gs2)))
		      names2
		      '(frac ex)))
	 ,@body))))

(defmacro with-dec (struct1 names1  &body body)
  (let ((gs1 (gensym)))
    `(let ((,gs1 ,struct1))
       (let  ,(mapcar #'(lambda (f field)
			  `(,f (,(symb "dec-" field) ,gs1)))
		      names1
		      '(frac ex))
	 ,@body))))

(defmethod ga::two-arg-+ ((r dec)(s dec))
  ;; adding 2 decs
  (with-dec2 r s (frac1 ex1)(frac2 ex2) 
	     (let ((newfrac 0)(newex 0))
	       ;; align fractions
		    (cond ((cl::>= ex1 ex2) 
			   (setf newfrac (cl::+ frac2  (cl::* frac1 (cl::expt 10 (cl::- ex1 ex2)))))
			   (setf newex ex2))
			  (t
			   (setf newfrac (cl::+ frac1  (cl::* frac2 (cl::expt 10 (cl::- ex2 ex1)))))
			   (setf newex ex1)))
		    (cond ((cl::= 0 newfrac)(dec 0 0))
			  (t (dec newfrac newex))))))


(defmethod ga::two-arg--((r dec) s)  ;;includes s dec.
  (+ r (- s)))

(defmethod ga::two-arg-- (s (r dec))
  (+ s (- r)))

;; (- r) for decimal r will be implemented by (* -1 r), so see two-arg-*


(defmethod ga::two-arg-+ ((s integer)(r dec))
  (+ r (dec s 0)))

(defmethod ga::two-arg-+ ((r dec)(s rational))  ;rational is integer or ratio
  (let ((h (decit s)))
    (if (dec-p h)(+ r h) ;; if possible, convert other arg to decimal, add
      (+ s   (dec-to-rational r)))))

(defmethod dec-to-rational((r dec))
   (cl:* (dec-frac r) (cl::expt 10 (dec-ex r))))

					;generic +

(defmethod ga::two-arg-+ (s (r dec)) ;; just reverse args
  (+ r s))

(defmethod ga::two-arg-+ ((r dec) s)
    (with-dec r (frac ex)
	      (+ s (cl::* frac (cl::expt 10 ex)))))

;; other methods like dec+interval  or dec + symbol..

(defmethod normal((r dec)) ;; shift off trailing 0 in fractions
  (with-dec r (frac ex)
	    (if (= 0 frac) (dec 0 0)
		(loop (multiple-value-bind (q r)(truncate frac 10)
			(unless (= r 0)(return (dec frac ex)))
			;; increment exponent by 1, divide fraction by 10
			;; maintains the value exactly.
			(incf ex)(setf frac q))))))
	    
  ;; for example, normalize will change (dec 1000 -4) to (dec 1 -1) 

;; MULTIPLICATION

;; a*10^b * c*10^d --->  (a*c)*10^(b+d).

(defmethod ga::two-arg-* ((r dec)(s dec))
  ;; multiplying 2 decs
  ;; need normal so 2.e0 X 5.e0 =1.e1 not 10.e0
 (normal (with-dec2 r s (frac1 ex1)(frac2 ex2)
	     (dec (* frac1 frac2)(+ ex1 ex2)))))

;; figure out special cases for s convertible to decimal. See two-arg-+, above
(defmethod ga::two-arg-* ((r dec) s)
    (decit (with-dec r (frac ex)
	      (* s frac (cl::expt 10 ex)))))

(defmethod ga::two-arg-* (s (r dec))
   (decit (with-dec r (frac ex)
		    (* s frac (cl::expt 10 ex)))))

(defmethod ga::two-arg-/ ((s dec) (r dec))
    (decit(with-dec2 s r (frac1 ex1) (frac ex)
	      (* (cl::/ frac1 frac) (cl::expt 10 (cl::- ex1 ex))))))

(defmethod ga::two-arg-/ (s (r dec))
    (decit(with-dec r (frac ex)
	      (/ s frac (cl::expt 10 ex)))))

(defmethod ga::two-arg-/ ((r dec) s)
   (decit (with-dec r (frac ex)
	      (/ (cl::* frac (cl::expt 10 ex)) s))))

(defmethod ga::negativep ((r dec)) ;; or should this be minusp?
  (cl::< (dec-frac r) 0))

(defmethod ga::two-arg-= ((r dec) (s dec)) 
  (with-dec2 s r (frac1 ex1) (frac ex)
	     (and (cl::= frac1 frac) (cl::= ex1 ex))))

(defmethod ga::two-arg-= ((r dec) s) 
  (= (dec-to-rational r) s))

(defmethod ga::two-arg-= (s (r dec)) 
  (= (dec-to-rational r)))

(defmethod ga::two-arg-expt ((r dec) (s integer)) 
  (cond((cl::= s 0)(dec 1 0))
       ((cl::> s 0) (with-dec r (frac ex)
			      (dec (cl::expt frac s)(cl::* s ex))))
       (t ;; negative exponent ;; probably will be left as ratio
	(decit (expt (dec-to-rational r) s)))))

;; need dec^non_integer, dec^dec, other^dec [for completeness]
  

;; could define 
;; zerop
;; <,>, <=, <=
;; 

(defmethod ga::sin ((s dec))  ;; compute sin in double precison
  (with-dec  s (frac ex) (sin (cl::* frac (expt 10.0d0 ex)))))

(defmethod ga::cos ((s dec))  ;; compute cos in double precison
  (with-dec  s (frac ex) (cos (cl::* frac (expt 10.0d0 ex)))))

;; etc.  We could do this using bigfloats instead.
(defun decit(q) 
  ;; convert a rational to decimal if possible
  ;; otherwise just returns the rational.
  (if (not(rationalp q)) q
  (let* ((a (numerator q))(b (denominator q))
	(d 0) (r 0))
    (loop 
      (if (= b 1) (return (dec a d)))
      (setf r (gcd b 10))
      (cond ((= r 10) (decf d)(setf b (/ b 10)))
	    ((> r 1)
	     (setf b (* b (/ 10 r)))
	     (setf a (* a (/ 10 r)))
	     )
	    (t ;; r=1, no decimal version.
	     (return q)))))))


#| If there is a nontrival gcd with 10, it is because b has a factor of 2 or a factor of 5.
It is clear when there is a factor of 2, because (evenp b) = t. and (ash b 1) divides by 2.
Is there a factor of 5?
Note that 5 = 101[base 2].  That is, 5 * X = X + 4 X = X + (ash X 2).
Should we make a case for doing this fast, or just use the simple code above?
Or the code below..

|#

#+ignore
(defun decit2(q) 
  ;; convert a rational to decimal if possible
  ;; otherwise just returns the rational.
  ;; maybe less work than decit.
  (if (not(rationalp q)) q
  (let* ((a (numerator q))(b (denominator q))
	 (c a)(d 0) (r 0)
	 (e (ceiling (integer-length b) 3.321927))
	 (cp10 (expt 10 e)))
    (if (= b 1) (return-from decit2 (dec a d)))
    (setf r (gcd b cp10))
    (if(= r b);  b is a power of 10
	  (normal (dec (* a (/ cp10 r)) (- e)))
	    ;; no decimal version.
	     q))))




