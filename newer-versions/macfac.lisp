;;; copied out of macsyma source code

(eval-when '(load eval compile) (declaim (optimize (speed 3)(safety 0))))

(defvar *factors-by-bits* (make-array 69))

(defun fact (u &aux (lgn (integer-length u)))
  (declare (fixnum u lgn)(optimize (speed 3)(safety 0)(debug 0)))
  (or (cdr (assoc u '((0 . 1) (1 . 1) (2 . 2) (3 . 6) (4 . 24) (5 . 120) (6 . 720))))
      (progn  (fill *factors-by-bits* 1 :end lgn)
	      (do* ((primes (make-prime-stream))
		    (prime 3 (funcall primes prime)))
		  ((> (* 2 prime) u)
		  
		   (do ((j (- lgn 1) (- j 1))
			(ans 1 (progn
		     
;				 (format t "~%factors[~s]= ~s,  ans= ~s "  j
;				   (aref *factors-by-bits* j) ans)
						  
				 (* ans ans (aref *factors-by-bits* j)))))
		       ((< j 0) (ash (* ans prime (prime-product-interval prime u primes))
				     (factorial-prime-power u 2)))
		     (declare (fixnum j)) ))
		 (declare (fixnum prime ))
		(do ((i 0 (+ i 1))
		     (pow (factorial-prime-power u prime) (ash pow -1)))
		    ((= pow 0))
		  (declare (fixnum i pow))
		  (when (oddp pow)
		    (setf (aref *factors-by-bits* i) (* prime (aref *factors-by-bits* i))))
		  
		 ;; (format t "~% i=~s fbb=~s" i  (aref *factors-by-bits* i))
		  )))))

(defun factorial-prime-power (u prime)
  (declare (fixnum u prime))
  (do* ((inc (floor u prime) (floor inc prime))
	(sum inc (+ inc sum)))
      ((< inc prime) sum)
    (declare (fixnum inc))))

(defun prime-product-interval (lo hi &optional (stream (make-prime-stream))
			       &aux (prime (funcall stream lo)))
  (declare (fixnum lo hi prime))
 (if (> prime hi) 1
    (* prime (let ((mid (floor (+ prime hi) 2)))
	       (declare (fixnum mid))
		 (* (prime-product-interval prime mid stream)
		    (prime-product-interval mid hi stream)))))
  )

(defvar *prime-p-sieve* (make-array 4 :element-type 'bit :initial-element 1
				       :adjustable 't)
  "0th element = primep(2), nth element = primep(2n+1)")
(defmacro pcompress (prime) `(cl::ash (- ,prime 1) -1))
(defmacro pdecompress (n) `(+ (cl::ash ,n 1) 1))

(defun make-prime-stream (&optional (old-prime 0))
  "returns primes generator, starting >= 1 beyond optional arg to make-prime-stream, or
   >= 1 beyond optional arg>=0 to generator itself.  negative arg resets generator."
  (flet ((scan-for-prime (&optional (index old-prime))
	   (if (< index 0) (setf old-prime 0)
	    (prog (top (sieve *prime-p-sieve*) (i (pcompress index)))
	      (declare (fixnum i))
	      (and (bignump index) (bignump (pcompress index))
		      (error "sorry, bignum prime sieve index ~d illegal on c architectures."
			     index))
	       tag (setf top (length sieve))
	       nax (unless (= 0 (aref sieve (progn (when (>= (incf i) top)
						     (grow-sieve) (decf i) (go tag))
						   i)))
		     (return (setf old-prime (if (= i 0) 2 (pdecompress i)))))
		  (go nax)))))
    #'scan-for-prime))

;;(defmacro bignump(x) `(typep ,x 'bignum))


(defun grow-sieve (&aux (old-length (length *prime-p-sieve*))
		   	(new-inverse-length (ceiling old-length 19))
			(nu-length (ash new-inverse-length 5))
			(new-length  nu-length))
 (declare (fixnum new-length))
 (when (bignump nu-length)
	   (error "sorry, bignum prime sieve length ~d illegal on c architectures."
		  nu-length))
  (adjust-array *prime-p-sieve* new-length :initial-element 1)
  (unwind-protect
     (do* ((sieve *prime-p-sieve*)
	   (primes (make-prime-stream))		;!
	   (largest-needed (- (isqrt (1- (pdecompress new-length))) (funcall primes))))	;2
	  ((> (do* ((prime (funcall primes))
		    (i (pcompress (* prime (dpb 1 (byte 1 0)
						(ceiling (pdecompress old-length) prime))))
		       (+ i prime)))		;odd multiples only
		   ((>= i new-length) prime)
		#-lispm (declare (fixnum prime i))
		(setf (aref sieve i) 0))
	      largest-needed)
	   (setf old-length new-length)))
    (adjust-array *prime-p-sieve* old-length :initial-element 1)))
