(defun kg (n m) 
  (declare (fixnum n m)) ;; tell lisp that n, m are not very big.
  (cond ((and (= m 2)(evenp n)) 
           (ash (kg (ash n -1) 1) (ash n -1)))
          ((<= n m) n)
          (t (* (kg n (ash m 1))
                (kg (- n m)(ash m 1))))))

(in-package :user)

(defun kgtop (n) 
  (let ((shift 0))
    (declare(special shift))
    (ash (kg1 n 1) shift)))

(defun kg1 (n m)
  (declare (fixnum n m)
	   (special shift))
  (cond ((and (evenp m)(evenp n)) 
	 (print shift)
	 (incf shift (ash n -1))
           (kg1 (ash n -1) (ash m -1)))
          ((<= n m) n)
          (t (* (kg1 n (ash m 1))
                (kg1 (- n m)(ash m 1))))))

(defun kg (n) 
  (let ((shift 0))
    (labels 
	((kg1 (n m)
	   (cond ((and (evenp n)(> m 1)) 
		  (incf shift (ash n -1))
		  (kg1 (ash n -1) (ash m -1)))
		 ((<= n m) n)
		 (t (* (kg1 n (ash m 1))
		       (kg1 (- n m)(ash m 1)))))))
      (ash (kg1 n 1) shift)  )))

(in-package :gmp)


(defun gkg(n);; using resource res for allocating GMPZ
  (declare (fixnum n)(optimize (speed 3)(safety 0)(debug 0)))
  (let* ((res nil) (shift 0)
	 (L (ceiling (cl::log n 2)))
	 (aa (alloc-gmpz))
	 (a (gmpz-z aa)))
    (declare (fixnum L shift n))
    (labels
	((k4i (n m ans)			; ans is a gmp number.
	      (declare (fixnum n m)  )
	      (cond
	       ((and (evenp n)(cl::> m 1)) 
		(cl::incf shift (ash n -1))
		(k4i (ash n -1) (ash m -1) ans))
	       ((cl::<= n m)(mpz_set_si ans n))
	       (t(let ((b (vector-pop res)))
		   (k4i (cl::- n m) (setq m(ash m 1)) b)
		   (k4i n m ans)
		   (mpz_mul ans ans b)
		   (cl::incf (fill-pointer res))
		   )))))
      (setq res (make-array L :fill-pointer 0))
      ;; (dotimes (i L)(vector-push (gmpz-z (alloc-gmpz2 (ash 1 (+ 3 i)))) res))
       (dotimes (i L)(vector-push (gmpz-z (alloc-gmpz)) res))
      (k4i n 1 a)
      (mpz_mul_2exp a a shift)
      aa)))






					