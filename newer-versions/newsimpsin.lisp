    (in-package :maxima)

(defun maxima-type(x)
  (cond ((atom x)(type x))
	(t (caar x))))


(defun simp-%sin (form y z &aux thetype) 
  (oneargcheck form)
  (setq y (simpcheck (cadr form) z))
  (setf thetype (maxima-type y))
  (simp-%sin1 y))

(setf (get '%sin 'operators) #'simp-%sin)

(defmethod simp-%sin1((a double) ty )(sin a))
(defmethod simp-%sin1((a single) ty )(sin a))
(defmethod simp-%sin1( a (ty (eql 'mrat))) ;;; taylor? rat?
  (bigfloat-sin a))
(defmethod simp-%sin1((a (eql '$ind)))'(($interval) -1 1))
(defmethod simp-%sin1((a (eql (taylorp a) '$ind))(($interval) -1 1)))

(defmethod simp-%sin1((y t));; everything else
    (cond 
	((taylorize (mop form) (second form)))
	((and $%piargs (cond ((zerop1 y) 0)
			     ((has-const-or-int-term y '$%pi) (%piargs-sin/cos y)))))
	((and $%iargs (multiplep y '$%i)) (mul '$%i (cons-exp '%sinh (coeff y '$%i 1))))
	((and $triginverses (not (atom y))
	      (cond ((eq '%asin (setq z (caar y))) (cadr y))
		    ((eq '%acos z) (sqrt1-x^2 (cadr y)))
		    ((eq '%atan z) (div (cadr y) (sqrt1+x^2 (cadr y))))
		    ((eq '%acot z) (div 1 (sqrt1+x^2 (cadr y))))
		    ((eq '%asec z) (div (sqrtx^2-1 (cadr y)) (cadr y)))
		    ((eq '%acsc z) (div 1 (cadr y)))
		    ((eq '$atan2 z) (div (cadr y) (sq-sumsq (cadr y) (caddr y)))))))
	((and $trigexpand (trigexpand '%sin y)))
	($exponentialize (exponentialize '%sin y))
	((and $halfangles (halfangle '%sin y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (cons-exp '%sin (neg y))))
	(t (eqtest (list '(%sin) y) form)))


  
  (simp-
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (cond ((zerop1 y) 0)
			     ((has-const-or-int-term y '$%pi) (%piargs-sin/cos y)))))
	((and $%iargs (multiplep y '$%i)) (mul '$%i (cons-exp '%sinh (coeff y '$%i 1))))
	((and $triginverses (not (atom y))
	      (cond ((eq '%asin (setq z (caar y))) (cadr y))
		    ((eq '%acos z) (sqrt1-x^2 (cadr y)))
		    ((eq '%atan z) (div (cadr y) (sqrt1+x^2 (cadr y))))
		    ((eq '%acot z) (div 1 (sqrt1+x^2 (cadr y))))
		    ((eq '%asec z) (div (sqrtx^2-1 (cadr y)) (cadr y)))
		    ((eq '%acsc z) (div 1 (cadr y)))
		    ((eq '$atan2 z) (div (cadr y) (sq-sumsq (cadr y) (caddr y)))))))
	((and $trigexpand (trigexpand '%sin y)))
	($exponentialize (exponentialize '%sin y))
	((and $halfangles (halfangle '%sin y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (cons-exp '%sin (neg y))))
	(t (eqtest (list '(%sin) y) form))))