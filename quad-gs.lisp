;;; Gaussian Quadrature, arbitrary precision,
;;; written in lisp by Richard Fateman, Jan 4, 2007
;;; inspired in part by QuadGS version 1.0 of June 26,2004, by Sherry Li
;;; refer to http://crd.lbl.gov/~xiaoye/quadrature.pdf

;;; Some comment lines  are taken verbatim from quad-gs.h or quad-gs.cpp
#|
   This Gaussian Quadrature scheme computes an integral from [-1,1] as
   the sum(w[j]*f(x[j]), j=0,n-1) where the values of the abscissas
   x[j] are roots of the nth degree Legendre polynomial P[n](x) on
   [-1,1], and the weights w[j] are -2/(n+1)/P'[n](x[j])/P[n+1](x[j]).

   To find the x[j], compute a starting value of cos(pi*(j-1/4)/(n+1/2)). 
   Then iterate until converged at full precision using Newton iteration.
   The iteration can be computed in modest precision, doubling at each step.

   The value of a Legendre polynomial P[n] can be computed recursively by
   P[0](x)=0, P[1](x)=1, P[k+1](x)= 1(k+1)* ((2k+1)*x*P[k](x)-k*P[k-1](x)).

   the derivative P'[n](x) = n (x*P[n](x)-P[n-1](x))/(x^2-1).

   This formula is poor if there is a singularity at an endpoint.
   |#

#| Compared to Sherry Li's code,  the work needed for  "ARPREC", 
involving storage stuff, is unnecessary.

In the part not yet written, we go
through some number of levels of abscissa-weight pairs, doubling the
number of pairs at each level. After obtaining a result at the first
level, we apply additional levels until we are satisfied or else have to
go back and compute more abscissa-weight pairs.

We can also dynamically increase the working precision of the
evaluation of the integrand f and the abscissa-weights, 
saving some time at lower precision if we are not even close. |#

;; This program legendre_p
;; will compute the P[k](x) Legendre polynomials. k is a fixnum. x is
;; any kind of floating point number: single, double, qd, MPFR.  It is
;; not actually needed, but it explains the next two
#+ignore
(defun legendre_p (k x)
  ;; P[0](x)=0, P[1](x)=1,
  ;; P[k](x)= 1/k* ((2k-1)*x*P[k-1](x)-(k-1)*P[k-2](x)).
  ;; or
  ;;      (/ (- (* (1- (* 2 k)) x t1) (* (1- k) t0)) k)
  (assert (and (fixnump k)(<= 0 k)))
  (case k 
    (0 1)
    (1 x)
    (otherwise
     (let ((t0 1)
	   (t1 x)
	   (ans 0))
       (declare (fixnum k))
       (loop for i from 2 to k  do
	     (setf ans     (/ (- (* (1- (* 2 i)) x t1) (* (1- i) t0)) i)
		   t0 t1
		   t1 ans))
       t1))))

;; legendre_pd returns legendre_p(k,x) and its derivative.
;;Note that d/dx P[n](x) =  (1/(x^2-1))*n*(x*P[n](x)-P[n-1](x)) except for x=+-1
;; This program is used with Newton iteration to refine roots of P[n](x).

(defun legendre_pd (k x)
  (assert (and (fixnump k)(<= 0 k)))
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

;; Compute an initial array of points that approximate roots of
;; Legendre polynomials for 0<=j<n: ar[0].. ar[n-1], good enough for
;; seeds for Newton iteration.

(defun setroots(n) ;;set up initial approximations for roots of legendre[n]
  (let ((ar (make-array n))	; initially floats, later bigfloats
	(nph (/ 1.0d0(+ n 0.5d0)))) ;one over n plus half
    (loop for j from 1 to n do
	  (setf (aref ar (1- j))(cos(* pi (- j 0.25) nph))))
    ar))

(defun improvedroots(a &optional (iter 4)) ;changes array a.  
  
  ;; I think that 4 is enough, in double precision, but 
  ;; for iterations in higher precision, iter=2 will suffice to
  ;; double the number of correct digits
  (loop for k from 1 to iter do
	(loop for j from 0 to (1- iter) do
	      (setf (aref a j)(improve_lg_root iter (aref a j)))))
  a)

;;The weights are w[j]:=  -2/ ( (n+1)* P'[n](x[j])*P[n+1](x))
;; this program computes them. 
(defun legendre_pdwt (k x)		; compute -2 / (  k * P'[k-1](x)*P[k](x))
  (assert (and (fixnump k)(< 1 k)))	; k>=2
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

  (let* ((abscissae (improvedroots (setroots n)))   ;; the points to evaluate fun at.
	 (np1 (+ n 1))
	 (weights (map 'array  #'(lambda(r)(legendre_pdwt np1 r))
		       abscissae)))
    ;; compute the sum.
    (reduce #'+  (map 'list #'(lambda(x w)(* w (funcall fun x)))
		      abscissae
		      weights))))

(defun int_gs_l1(fun n) 

  (let* ((abscissae (improvedroots (setroots n)))   ;; the points to evaluate fun at.
	 (np1 (+ n 1))
	 (weights (map 'array  #'(lambda(r)(legendre_pdwt np1 r))
		       abscissae)))
    ;; compute the sum.
    (loop for i from 0 to (1- (length abscissae)) sum
	  (* (funcall fun (aref abscissae i))(aref weights i)))
    
    ))


;; to make this work for arbitrary precision: Assume use has set a
;; goal accuracy.  figure out working precision.  See if the accuracy
;; criterion has (probably) been met.  The driver may also have to
;; transform some other integral limits, including limits at infinity,
;; to -1,1. 

;;   And a super-good driver using symbolic tools will look at
;;   the integrand and remove singularities, detect symmetries,
;;   oscillatory functions, etc.  And compile the function as needed.

;; RJF Jan 6, 2007.
