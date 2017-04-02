(in-package :maxima)
;; extracted from nformat.lisp
;; objective: to make nformat (and hence displa) work for objects which are atomic
;; but are in fact structures or instances of classes (CLOS objects).

;; I think that nothing else breaks, and in fact nformat should be considerably
;; faster.  It is now data-directed, and for example does not check every non-atomic
;; subexpression to see if it is a bigfloat or poisson series, which it almost certainly
;; is not.

;;; RJF 12/17/07

(defun nformat (form) ;; from nformat.lisp
  (cond ((atom form)
	 (cond ((and (numberp form) (minusp form)) (list '(mminus) (nformat(- form))))
	       ((realp form) (form-real form))
	       ((eq t form) (if in-p t '$true))
	       ((eq nil form) (if in-p nil '$false))

	       ((and displayp (car (rassoc form aliaslist :test #'eq)))) 
	       ;; objects which are atoms (e.g. instances of classes or defstructs)
	       ;; end up here.
	       (t (let ((prog (get (type-of form) 'format-prog))) ;;changed to be data directed
		 (if prog (funcall prog form) form)))))
	((atom (car form)) form) ;probably a left-over lisp form, not macsyma/maxima
	((eq 'mmacroexpanded (caar form)) (nformat (caddr form)))
	((null (cdar form)) form)	;not simplified? leave it alone?
	;; almost everything is handled by data-directed dispatch to form pgms
	(t (let ((prog (get (caar form) 'format-prog))) 
	     (if prog (funcall prog form) form)))))

(defun form-mpois(form) (nformat ($outofpois form)))

(defun form-rat(form)
  (cond ((minusp (cadr form))
		(list '(mminus) (list '(rat) (- (cadr form)) (caddr form))))
	       (t (cons '(rat) (cdr form)))))

(defun form-bigfloat(form)
	 (if (minusp (cadr form))
	     (list '(mminus) (list (car form) (- (cadr form)) (caddr form)))
	   (cons (car form) (cdr form))))


(setf (get 'mplus 'format-prog) 'form-mplus); defined later, so #' not used.
(setf (get 'mtimes 'format-prog) 'form-mtimes)
(setf (get 'mexpt 'format-prog) 'form-mexpt)
(setf (get 'mrat 'format-prog) 'form-mrat)
(setf (get 'mpois 'format-prog) #'form-mpois)
(setf (get 'bigfloat 'format-prog) #'form-bigfloat)
(setf (get 'rat 'format-prog) #'form-rat)


;; a floating point string, e.g. "~e", "~f", 
;; or even "~10,4e" could be used here.
(defparameter $floatformat "~s")

(defparameter $integerformat "~s")

(defun form-real(r)
  (cond 
   ((floatp r)
      (format nil  
	      ;; account for possibility that maxima will not have strings yet.
	      ;; in which case it could look like |&~s|.
	      ;; convert it to a string on first use, anyway.
	      (if (stringp $floatformat) $floatformat 
		(setf $floatformat (subseq (symbol-name $floatformat) 1)))
	      r))
	((integerp r)
	   (format nil  
	      ;; account for possibility that maxima will not have strings yet.
	      ;; in which case it could look like |&~s|.
	      ;; convert it to a string on first use, anyway.
	      (if (stringp $integerformat) $integerformat 
		(setf $integerformat (subseq (symbol-name $integerformat) 1))) r))
	;; do you want to do something for other real types? e.g. ratios?
	;; probably not ratios.
	(t r))) 

(defun form-mexpt (form &aux exp)
  (cond ((and $sqrtdispflag (alike1 1//2 (caddr form))) (list '(%sqrt) (cadr form)))
	((and $sqrtdispflag (alike1 -1//2 (caddr form)))
	 (list '(mquotient) 1 (list '(%sqrt) (cadr form))))
	((and (or (and $%edispflag (eq '$%e (cadr form)))
		  (and $exptdispflag (not (eq '$%e (cadr form)))))
	      (not (atom (setq exp (nformat (caddr form)))))
	      (eq 'mminus (caar exp)))
	 (list '(mquotient) 1 (if (or (equal 1 (cadr exp)) (string= (cadr exp) "1")) (cadr form)
				  (list '(mexpt) (cadr form) (cadr exp)))))
	(t (cons '(mexpt) (cdr form)))))

;; extension for other types...

;; for example, we can do this, for real intervals
#|(setf (get 'ri::ri 'format-prog) #'form-ri)
(defun form-ri(form)
  (format nil "[~a,~a]" (ri-lo form)(ri-hi form)))

;; and for quad-doubles (octs)

(setf (get 'oct::quad-real 'format-prog) 'oct::oct2string)
|#







