;;; Initial version copied out of macsyma source code
;;; changed to use GMP
;;; Note, number sieve is just for fixnums, here.
;;; try to hack to make faster...
(in-package :user)
(eval-when '(load eval compile) (declaim (optimize (speed 3)(safety 0))))

(defvar *factors-by-bits* (make-array 69))

(defun fact (u)
  (declare (fixnum u)(optimize (speed 3)(safety 0)(debug 0)))
  (or (if (or (< u 1)(not (typep u 'fixnum))) (error "bad arg ~s to factorial" u))
      (and (typep u '(integer 0 12))
	   (aref #(1 1 2 6 24 120 720 5040 40320 362880 3628800 39916800 479001600)
		 u))
      (let ((lgn (integer-length u))
	    (res nil);; resource list
	    (h nil)
	    (halfu (ceiling u 2)))
	(declare (fixnum u lgn halfu))
	(dotimes (i lgn)
	  (push (gmp::gmpz-z (gmp::into 1)) res)
	  (setf (aref *factors-by-bits* i)(gmp::gmpz-z (gmp::into 1))))
	
	(do* ((primes (make-prime-stream))
	      (prime 3 (funcall primes prime)))
	    ((cl::> prime halfu)
	     (do ((j (cl::1- lgn) (cl::- j 1))
		  (ans  (gmp::gmpz-z (gmp::into 1))
			(progn
			  (gmp::mpz_mul ans ans ans)
			  (gmp::mpz_mul ans ans (aref *factors-by-bits* j))
			  ans)))
		 ((cl::< j 0)		;termination if
		  (gmp::mpz_mul_si (car res)
				   (prime-product-interval prime u primes (car res) (cdr res))
				   prime)
		  (gmp::mpz_mul ans ans (car res))
		  (gmp::mpz_mul_2exp ans ans (factorial-prime-power u 2))
		  (gmp::make-gmpz :z ans))
	       (declare (fixnum j))))
	  (declare (fixnum prime))
	  (do ((i 0 (cl::1+ i))
	       (pow (factorial-prime-power u prime) (cl::ash pow -1)))
	      ((cl::= pow 0))
	    (declare (fixnum i pow))
	    (when (cl::oddp pow)
	      (setf h (aref *factors-by-bits* i))
	      (gmp::mpz_mul_ui h h prime)))))))

(defun factorial-prime-power (u prime) ;; called for each prime less than u/2
  (declare (fixnum u prime)) ;; how many times does the prime p go into u!?
  (do* ((inc (floor u prime) (floor inc prime))
	(sum inc (cl::+ inc sum)))
      ((cl::< inc prime) sum)
    (declare (fixnum sum inc))))

(defun prime-product-interval (lo hi  stream  ans res
			       &aux (prime (funcall stream lo)))
  (declare (fixnum lo hi prime))
  (cond ((cl::> prime hi) (gmp::mpz_set_si ans 1))
    (t (let ((mid (floor (cl::+ prime hi) 2)))
		  (declare (fixnum mid))
		  (let ((b (car res)))
			(prime-product-interval prime mid stream b (cdr res))
			(prime-product-interval mid hi stream ans (cdr res))
			(gmp::mpz_mul ans ans b)
			(gmp::mpz_mul_ui ans ans prime)))))
  ans)

(defvar *prime-p-sieve* (make-array 4 :element-type 'bit :initial-element 1
				       :adjustable 't)
  "0th element = primep(2), nth element = primep(2n+1)")
(defmacro pcompress (prime) `(cl::ash (cl::- ,prime 1) -1))
(defmacro pdecompress (n) `(cl::+ (cl::ash ,n 1) 1))

(defun make-prime-stream (&optional (old-prime 0))
  "returns primes generator, starting >= 1 beyond optional arg to make-prime-stream, or
   >= 1 beyond optional arg>=0 to generator itself.  negative arg resets generator."
  (declare(fixnum old-prime)(optimize (speed 3)(safety 0)))
  (flet ((scan-for-prime (&optional (index old-prime))
	   (declare (fixnum index))
	   (if (cl::< index 0) (setf old-prime 0)
	     (prog (top 
		    (sieve *prime-p-sieve*)
		    (i (pcompress index)))
	      (declare (fixnum i))
	      tag (setf top (length sieve))
	       nax (unless (cl::= 0 (aref sieve (progn (when (cl::>= (cl::incf i) top)
						     (grow-sieve) (cl::decf i) (go tag))
						   i)))
		     (return (setf old-prime (if (cl::= i 0) 2 (pdecompress i)))))
		  (go nax)))))
    #'scan-for-prime))


(defun grow-sieve (&aux (old-length (length *prime-p-sieve*))
		   	(new-inverse-length (ceiling old-length 19))
			(nu-length (cl::ash new-inverse-length 5))
			(new-length  nu-length))
  (declare (fixnum new-length old-length new-inverse-length nu-length))
  (adjust-array *prime-p-sieve* new-length :initial-element 1)
  (unwind-protect
      (do* ((sieve *prime-p-sieve*)
	    (primes (make-prime-stream))
	    (largest-needed (cl::- (isqrt (cl::1- (pdecompress new-length))) (funcall primes))))
	  ((cl::> (do* ((prime (funcall primes))
		    (i (pcompress (cl::* prime (dpb 1 (byte 1 0)
						(ceiling (pdecompress old-length) prime))))
		       (cl::+ i prime)))	;odd multiples only
		  ((cl::>= i new-length) prime)
		(declare (fixnum prime i))
		(setf (aref sieve i) 0))
	      largest-needed)
	   (setf old-length new-length)))
    (adjust-array *prime-p-sieve* old-length :initial-element 1)))

