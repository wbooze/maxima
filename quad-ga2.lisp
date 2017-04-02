(in-package :ga)
;;look in quad-ga for documentation

(defun legendre_pd (k x)
  (assert (and (typep k 'fixnum)(<= 0 k)))
  (case k 
    (0 (values 1 0))
    (1 (values x 1))
    (otherwise
     (let ((t0 1)
	   (t1 x)
	   (ans 0))
       (declare (fixnum k))
       (loop for i from 2 to k  do
	     (setf ans     (/ (- (* (1- (* 2 i)) x t1) (* (1- i) t0)) i)
		   t0 t1
		   t1 ans))
       (values        t1
		      ;; (1/(x^2-1))*k*(x*P[k](x)-P[k-1](x))
		      ;; except if abs(x)=1 then use
		      (if (= 1 (abs x))
			  ;; 1/2*k*(k+1)*x^(k+1)
			  (/ (* k (1+ k) (if (oddp k) 1 x)) 2)
			(/ (* k (- (* x t1)t0))  (1-  (* x x)))))))))

;; Here is a Newton iteration to converge to a root of
;; legendre_p[n](x) starting from a particular guess.
;; The answer's precision will depend on the generic arithmetic
;; in  (- guess (/ val deriv))

(defun improve_lg_root(n guess)
  (multiple-value-bind (val deriv)(legendre_pd n guess)
    (if (= deriv 0) (if (= val 0) 
			val 
		      (error "newton iteration failed at ~s"guess))
      ;;new_guess= guess-f(x)/f'(x)
      (- guess (/ val deriv)))))


		
    

;;The weights are w[j]:=  -2/ ( (n+1)* P'[n](x[j])*P[n+1](x))
;; this program computes them. 
(defun legendre_pdwt (k x)		; compute -2 / (  k * P'[k-1](x)*P[k](x))
  (assert (and (typep k 'fixnum)(< 1 k)))	; k>=2
  (/ -2 
     (* k
	(let 
	    ((t0 1)
	     (t1 x)
	     (ans 0) )
	  (declare (fixnum k))
	  (loop for i from 2 to (1- k)  do
		(setf ans     (/ (- (* (1- (* 2 i)) x t1) (* (1- i) t0)) i)
		      t0 t1
		      t1 ans))
	  (*				;deriv of p[k-1]
	   (let((k (1- k))) ;use formula above..
	     (if (= 1 (abs x))
		 (/ (* k (1+ k) (if (oddp k) 1 x)) 2)
	       (/ (* k (- (* x t1)t0)) (1- (* x x)))))
	   ;; legendre_p[k](x)
	   (/ (- (* (1- (* 2 k)) x t1) (* (1- k) t0)) k))))     ))


;; put almost all the pieces together:
;; Gaussian integration of the function fun using
;; n legendre points between -1 and 1, using whatever
;; precision is default for generic arithmetic.
(defun int_gs_l1(fun n) 

  (let* ((abscissae (improvedroots (setroots n) n))   ;; the points to evaluate fun at.
	 (np1 (+ n 1))
	 (weights (map 'array  #'(lambda(r)(legendre_pdwt np1 r))
		       abscissae)))
    ;; compute the sum.

    (loop for i from 0 to (1- n) sum
	  (* (funcall fun (aref abscissae i))(aref weights i)))
    
    ))



(defun int_gs_l1bf(fun n)  ;; bigfloats

  (let* ((np1 (+ n 1))
	 (v nil)
	 (myprec 38) ; might be 53, but maybe not all correct
	 (sum (mpfr::into 0))
	 (goal-precision (mpfr::get-prec))
	 (nph (/ 1.0d0(+ n 0.5d0)) ))
    ;; we step from point 1 to point n, summing along the way
    (loop for j from 1 to n do
	  (setf v (cos(* pi (- j 0.25d0) nph))) ;initial approx.
	  (loop for k from 1 to 4 do ;; 4 iterations in double-float, from init
		(setf v (improve_lg_root n v )))
	  ;;  do this refinement some number of times..
	  (setf v (mpfr::into v))
	  (setf myprec 38) 
	  (loop while (< myprec goal-precision) do
		(mpfr::set-prec (min goal-precision (setf myprec (* 2 myprec))))
		(setf v (improve_lg_root n v)))
	  ;; now that v is accurate enough put it into weighted sum
	  (setf sum (+ sum (* (legendre_pdwt np1 v) (funcall fun v)))))
    sum))


#|
:pa :ga
(mfpr::set-prec 200) ; bits of fraction
(int_gs_l1bf #'exp 10)			; 10 points
(int_gs_l1bf #'exp 20)			; 20 points
(int_gs_l1bf #'exp 40)

;; compare to correct answer 
(let ((k (exp (mpfr::into 1)))) (- k (/ 1 k)))
|#

