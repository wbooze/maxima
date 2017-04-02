;;; transcribed from P. Luschny's split recursive Java code
(in-package :gmp)
(defun sprfact(n)
  (declare (fixnum n))  
  (let  ((p 1) (r 1) (NN 1) (log2n (floor (log n 2)))
	 (h 0) (shift 0) (high 1) (len 0))
    (declare (fixnum log2n h shift high len NN))
    (labels ((prod(n)
	       (declare (fixnum n))
		(let ((m (ash n -1)))
		  (cond ((= m 0) (incf NN 2)) 
			((= n 2) (* (incf NN 2)(incf NN 2)))
			(t  (* (prod (- n m)) (prod m)))))))
    
      (declare (fixnum n))
      (cond ((< n 0)(error "bad arg ~s to factorial" n))
	    ((< n 2) 1)
	    (t
	     (loop while (/= h n) do
		   (incf shift h)
		   (setf h (ash n (- log2n)))
		   (decf log2n)
		   (setf len high)
		   (setf high (if (oddp h) h (1- h)))
		   (setf len (ash (- high len) -1))
		   (cond ((> len 0)
			  (setf p (* p (prod len)))
			  (setf r (* r p)))))
	     (ash r shift))))))

;; minimally changed. p and r are made gmp numbers. It works.

(defun gsprfact2(n)
  (declare (fixnum n))  
  (let  ((p (gmp::into 1)) (r (gmp::into 1)) (NN 1) (log2n (floor (log n 2)))
	 (h 0) (shift 0) (high 1) (len 0))
    (declare (fixnum log2n h shift high len NN))
    (labels ((gprod(n)
	       (declare (fixnum n))
		(let ((m (ash n -1)))
		  (cond ((= m 0) (incf NN 2)) 
			((= n 2) (* (incf NN 2)(incf NN 2)))
			(t  (* (gprod (- n m)) (gprod m)))))))
    
      (declare (fixnum n))
      (cond ((< n 0)(error "bad arg ~s to factorial" n))
	    ((< n 2) 1)
	    (t
	     (loop while (/= h n) do
		   (incf shift h)
		   (setf h (ash n (- log2n)))
		   (decf log2n)
		   (setf len high)
		   (setf high (if (oddp h) h (1- h)))
		   (setf len (ash (- high len) -1))
		   (cond ((> len 0)
			  
			  (setf p (* p (gprod len)))
			  (setf r (* r p)))))
	     (mpz_mul_2exp (gmpz-z r) (gmpz-z r) shift) ;; arithmetic shift
	     r)))))

;;; I'm using cl::* etc. to avoid generic discrimination costs
;;; in case the compiler didn't figure that out.

;;;; use a stack for resources, saves time and space.
;;;; this is the best version of gsprfact so far.
;;;; this is very slightly slower than built-in GMP factorial.

(defun gsprfact(n)
  (declare (fixnum n))  
  (let*  ((pp (into 1)) (p (gmpz-z pp))
	  (rr (into 1)) (r (gmpz-z rr))
	  (NN 1)
	  (mlog2n (-(floor (log n 2))))
	  (h 0) (shift 0) (high 1) (len 0) (ans (gmpz-z (into 1)))
	  (res (make-array(1+ (- mlog2n)) :fill-pointer 0)))
    (declare (fixnum mlog2n log2n h shift high len NN))
    (labels ((gprod(n ans)
	       (declare (fixnum n))
	       (let ((m (cl::ash n -1)))
		 (cond ((cl::= m 0) 
			(mpz_set_si ans (incf NN 2)))
		       ((cl::= n 2)
			(mpz_set_si ans (cl::* (cl::incf NN 2)(cl::incf NN 2))))
		       (t (let ((b (vector-pop res))) ;b is a temporary gmpz
			     (gprod m b) ;set b
				(gprod (cl::- n m) ans) ;set ans
				(mpz_mul ans b ans) ;set ans
				(cl::incf (fill-pointer res)) ; remove b
				)))
		     ans)))
      
  	(dotimes (i  (1+ (- mlog2n)))
	  (vector-push  (gmpz-z (alloc-gmpz2 256)) res)) ;initialize

	(loop while (cl::/= h n) do
	      (cl::incf shift h)
	      (setf h (cl::ash n mlog2n))
	      (cl::incf mlog2n)
	      (setf len high)
	      (setf high (if (oddp h) h (cl::1- h)))
	      (setf len (cl::ash (cl::- high len) -1))
	      (cond ((cl::> len 0)
			(mpz_mul p p (gprod len ans))
			(mpz_mul r r p)
			)))
	(mpz_mul_2exp r r shift)
	rr)))

;; compare to

(defun k4(n);; using resource instead of allocation each time. Faster!!
  (declare (fixnum n) (optimize (speed 3)(safety 0)))
  (let ((res nil))
    (labels
	((k4i (n m ans)			; ans is a gmp number.
	   (declare (fixnum n m)
		    (type (simple-array  (signed-byte 32) (3))  ans))
	   (if(cl::<= n m)(mpz_set_si ans n)
	     (let ((b (vector-pop res)))
	       (declare (type (simple-array  (signed-byte 32) (3))  b))
	       (k4i (cl::- n m) (setf m(ash m 1)) b)
	       (k4i n m ans)
	       (mpz_mul ans ans b)
	       (incf (fill-pointer res)) ; (vector-push b gmpres)
	       ))))
      (let* ((L (ceiling (cl::log n 2)))
	     (a (alloc-gmpz)))
	(declare (fixnum L))
	(setf res (make-array L  :fill-pointer 0))
	;; allocate enough space for temps.
	;; fill the temps with gmp numbers, zeros.  Just the insides though.
	(dotimes (i L)(vector-push  (gmpz-z (alloc-gmpz)) res))
	(k4i n 1 (gmpz-z a))
	a))))




;; maxfactx can be done nicely in gmpz, and is 
;; about 5X faster on windsor with generic gmp library;
;; with pentium-2 version, it is 14X to 16X faster.
;; cutting short the iteration by memoization would also
;; help.  20,000! in 70ms.

(setf  *print-circle* t)

(defun gmaxfactx (n)
  (declare (fixnum n)(optimize (speed 3)(safety 0)))
  (let* ((z (if (< n 100) 5 100))
	 (aloop (gcloop z))) ; heuristic
    (loop for  i from (1+ z)  to n
	do  ;;(setf (car aloop) (* (car aloop) i))
	  (mpz_mul_si (car aloop) (car aloop) i)
	    (setf aloop (cdr aloop)))
    (gloopprod aloop)))

(defun gcloop(n) ;;   (cloop 5) is #1=(5 4 3 2 1 . #1#) with gmpz insides 
  (let*((start (list (gmp::gmpz-z (gmp::into 1))))
	(end start))
    (dotimes (i (1- n) (setf (cdr end) start))
      (setq start (cons (gmpz-z (into (+ 2 i))) start)))))

(defun gloopprod (l) 
  ;; efficient product of all the approximately equal
  ;; numbers in the loop trying to keep the sizes of inputs
  ;; approximately balanced.  the circular loop l is destroyed in the
  ;; process.
  (declare (optimize (speed 3)(safety 0)))
  (cond ((eq(cdr l) l)(make-gmpz :z (car l)))
	(t ;;(setf (car l)(* (car l)(cadr l)))
	 (mpz_mul (car l)(car l)(cadr l))
	   (setf (cdr l)(cddr l))
	   (gloopprod (cdr l)))))



