(in-package :ma)
 ;;; convert to infix expression... / improved from OCR/sockets/tilu-aserve.cl
;;; This is the 10 percent of the code that does 90 percent of the work.
;;; Most, but not all, unnecessary parens are left out.

(defun infixop(x)
  (member x '(+ * ^ -  / > < >= <= = and \|\| && or expt aref _)))

(defun getprec(x)(or (get  x   'prec)     100))

(defparameter preclist
    '((or &&)  (and \|\|)(not)
      (< > <= >= = )
      (+ -)
      (* /)
      (^ expt)
      (_)))

(eval-when (load) ;set up precedences according to list
  (let ((count 1))
  (dolist (i preclist)
    (mapc #'(lambda (r) (setf (get r 'prec) count)) i)
    (incf count))))

(defun op(x) (setf x (car x)) 
       (case x
	 (expt '^)
	 (and  '&&)
	 (or '\|\|)
	 (aref '_)
	 (1- 'sub1)			;otherwise (1- x) becomes confusing "1-(x)" not x-1
	 (! 'fact)			;postfix x! changed to fact(x)
	 (otherwise x)))

(defun ma::p2i(x)  (parenfix x))

(defun parenfix(x  &optional (upper-prec 0))
  ;;parenthesis-inserting prefix walk and print of a tree
  (cond ((or (and (numberp x) (< x 0));; like -3  ->  (-3)
	     (typep x 'ratio));; like 1/2   -> (1/2)
	 (format nil "(~a)" x) )
	((not (consp x))
	 (format nil"~a" x));; symbol or pos. number
	((and (null (cddr x))(eq (car x) '-))
	 (format nil "(-~a)"(cadr x)))
	((not (infixop (op x)));; case of f(x) or f(x,y,z)
	 (format nil "~a(~a)" (parenfix (op x))(intersperse "," (cdr x) 0)))
	;; treat +*/^ ? not sure about ^ where left/right prec differs
	(t (format nil (if (> (getprec (op x)) upper-prec) "~a" "(~a)") 
		   (intersperse (op x)(cdr x) (getprec (op x)))))))

  ;; op=+, l = (a b c) prints  a+b+c. parens inserted if appropriate.
(defun intersperse (q lst prec)
    (format nil (format nil "~a~a~a" "~{~a~^" q "~}" )
	    (mapcar #'(lambda (k) (parenfix k prec)) lst)))





