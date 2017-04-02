;; Who me? write a derivative program??
;; This is not a great program. It produces unnecessarily unsimplified
;; answers, consing up expressions that shortly  will be multiplied by zero.
;; This program is, however, quite short.  Try  (d (sym (* (sin x)(cos x))) 'x)
;; (d '(foo v) 'v)  returns "unevaluated" as (d (foo v) v).

(defmethod d ((x ma) (var ma))(gasimp (ma (d (ma-e x) (ma-e var)))))
(defmethod d ((x ma) (var t))(gasimp (ma (d (ma-e x)  var))))

(defmethod d((e t) (v t))
  (if(atom e)(if(eq e v)1 0)
    (funcall(get(car e)'d #'(lambda (e v) `(d ,e, v))) e v)))

(defmacro dr(op s)`(setf(get ',op 'd) ;;define a rule to diff operator op!
                (compile() '(lambda(e v)
                             (let((x(cadr e)))
                               (list '* (subst x 'x ',s) (d x v)))))))
;; data on derivatives
(dr cos (* -1 (sin x)))
(dr sin (cos x))
(dr tan (expt (cos x) -2))
(dr log (expt x -1))
(dr asin (expt (+ 1 (* -1 (expt x 2))) -1/2))
(dr acos (* -1 (expt (+ 1 (* -1 (expt x 2))) -1/2)))
(dr atan (expt (+ 1 (expt x 2)) -1))
(dr sinh (cosh x))
(dr cosh (sinh x))
(dr atanh (expt (1+ (* -1 (expt x 2))) -1))
(dr log (expt x -1))
(dr exp (exp x))
(dr sqrt (* 1/2 (expt x -1/2)))

(setf(get '+ 'd)  ;; rules for +, *, expt must handle n args, not just 1
  #'(lambda(e v) `(+,@(mapcar #'(lambda(r)(d r v))(cdr e)))))
(setf(get '* 'd)
  #'(lambda(e v) `(*,e(+,@(mapcar #'(lambda(r) `(*,(d r v)(expt,r -1)))(cdr e))))))
(setf(get 'expt 'd)
  #'(lambda(e v) (if (numberp (caddr e))
		     `(* ,(caddr e) ,(d (cadr e) v)(expt ,(cadr e) (- ,(caddr e) 1)))
		   `(*,e,(d `(*,(caddr e)(log,(cadr e)))v)))))
(setf(get 'power 'd) ;; maybe use (power x n) for x^n instead of expt, if n indep of x.
  #'(lambda(e v) `(* ,(caddr e),(d (cadr e) v)(power ,(cadr e) (- ,(caddr e) 1)))))






