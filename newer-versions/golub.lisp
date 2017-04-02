;; Calculation of Gauss Quadrature Rules
;; based on paper 

;;Gene H. Golub, John H. Welsch, ``Calculation of Gauss Quadrature Rules,''
;;Math. Compu 23, no 106 (Arpil, 1969) pp 221-230.

;; original program in Algol 60. Not all of the code in the paper
;; is used here: we are just doing Legendre polynomials for now.
;; Also use same array allocation locations 1 to n, at least for now, leaving
;; zero element sometimes blank to keep indexing uniform with document.
(defun legenabc (n a b)
  ;;Supply the coefficients (A, B) of the normalized recurrence
  ;;relation for Legendre Polynomials.
  (loop for i from 1 to (1- n) do (setf (aref a i) 0)
				  (setf (aref b i) (/ i (sqrt (1- (* 4 i i))))))
  (setf (aref a n) 0)
  (values  a b))


(defun gaussquadrule (n)
    ;;(n a b c muzero symm t w)
    ;; for legendre, symm=true. t and w are output, a,b,c, working arrays.
  (let ((A (make-array (1+ n)))
	(W (make-array (1+ n)))
	(T (make-array (1+ n))) ;; all 1..n as written
	(B (make-array (1+ n))) ;;  0 .. n
	(muzero 2) (aa 0)(b2 0)(det 0)(lambda 0)(lambd1 0)(lambd2 0)
	(rho 0)(norm 0) (cj 0)(eigmax 0)(r 0)(i 0)(m 0)(m1 0)(k 0)(eps 0)
	(aj 0)(st 0)(f 0)(ct 0)(q 0)(wj 0)
	)
    
    	;;(MU (make-array (1+ (* 2 n)))) ;; 0 .. 2*n
    (legenabc n A B)
    
    ;; Given the coefficients (A, B, C) of the three term recurrence
    ;; relation: P(K)=(A(k)x+B(K))*P(K-1)-C(K)*P(K-2), this procedure
    ;; computes the absicssa T and the weights W of the Gaussian type
    ;; guadrature rule asoiciated with the orthogonal polynomial
    ;; by QR type iteration with origin shifting. 
    ;;setup
    (setf (aref B 0) 0)
    (setf norm 0)
  (loop for i from 1 to (1-  n) do
	(setf norm  (max norm (+ (abs(aref B(1- i)))(abs(aref A i)))))
	(setf (aref W i) 0))
  (setf norm  (max norm (+ (abs(aref A n))(abs(aref B (1- n))))))
  (format t "~%norm=~s" norm)
  (setf eps (* norm  (expt 2.0d0 -50)) ) ;; compute wrt current precision
  (setf i n);;??
  (setf (aref W i) 1) ;; what is i??
  (setf (aref W n) 0)
  (setf m n)
  (setf lambda (setf lambd1 (setf lambd2 (setf rho norm))))
  ;; look for convergence of lower diagonal element
  ;; inspect loop 
  (loop
    (if (= m 0) (return (sorttwo W T n)))
    ;; 
    (format t "~%m=~s" m)
    ;; return absicssas and  weights in pairs, ordered
    (setf i (setf k (setf m1 (1- m)))) ;; decrement m until exit
    (cond ((<= (abs(aref B m1)) eps)
	   (setf (aref T m) (aref A m))
	   (setf (aref W m) (* muzero (sq (aref W m))))
	   (setf rho (min lambd1 lambd2))
	   (setf m m1))
	  ;;the condition is then (> (abs(aref B m1)) eps)
	  (t  
	   ;;small off diagonal element means matrix can be split
	   (loop while  (> (abs(aref B (decf i))) eps) do (setf k i))
	   ;; Find eignevalues of lower 2X2 and select accelerating shift
	   (setf b2 (sq(aref B m1)))
	   (setf det (sqrt (+ (sq(- (aref A m1) (aref A m))) (* 4 b2))))
	   (setf aa (+ (aref A m1)(aref A m)))
	   (setf lambd2 (* 1/2 (if (>= aa 0) (+ aa det)(- aa det))))
	   (setf lambd1 (/(- (* (aref A m1)(aref A m)) b2) lambd2))
	   (setf eigmax (max lambd1 lambd2))
	   (if (<= (abs (- eigmax rho)) (* 1/8 (abs eigmax)))
	       (setf lambda(setf rho eigmax))
	     (setf rho eigmax))
	   ;; Transform block from k to m
	   (setf cj (aref B k)
		 (aref B (1- k)) (- (aref A k) lambda))
	   (loop for j from k to m1 do
		 (setf r (sqrt (+ (sq cj)(sq(aref B (1- j))))))
		 (setf st (/ cj r)
		       ct (/ (aref B (1- j)) r)
		       aj (aref A j)
		       (aref B (1- j)) r
		       cj (* (aref B (1+ j)) st)
		       (aref B (1+ j)) (- (* (aref B (1- j))ct))
		       f (+ (* aj ct)(* (aref B j) st))
		       q (+ (* (aref B j) ct)(* (aref A (1+ j)) st))
		       (aref A j) (+(* f ct)(* q st))
		       wj (aref W j)
		       (aref A (1+ j)) (* aj (aref A (1+ j))(- (aref A j)))
		       (aref W j) (+ (* wj ct)(* (aref W (1+ j)) st))
		       (aref W (1+ j))( - (* wj st) (* (aref W (1+ j)) ct))))
	   (setf (aref B (1- k)) 0))))	; end of loop at insert
  ))
		       
(defun sq(x)(* x x))

(defun sorttwo ( T W n)
    (let ((pairs nil))
      (loop for i from 1 to n do (push (cons (aref T i)(aref W i)) pairs))
      
      (sort pairs #'< :key #'car)))
				 
  
  
(defun gr(n)(gaussquadrule n))
