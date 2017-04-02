;; Some lisp functions for playing with representation of functions
;; by their Chebyshev series coefficients.  This makes sense
;; primarily if you are interested in functions defined on [-1,1].
;; The functions could be translated or scaled, or pasted together
;; piecewise, as done by the Chebfun group (see their web page).


;; Evaluating a function given by its Chebyshev coefficients, i.e. 
;;f(x)=sum'a_jT_j(x)  where sum' means 1/2*a_0 ... */

;; Write down the Chebyshev polynomial recurrence here for reference
;; t_0 =1
;; t_1 =x
;; t_n = 2*x*T_{n-1} -T_{n-2}

;; If the sequence of coeffs of f is 
;; FA={a_0, a_1, ..., a_n}   of length n+1,

;;we can evaluate f by this:
;; this is probably a bad way since it typically starts with the
;; big numbers and adds small ones to it. 

(defun eval_ts(fa pt) ;; fa = (a0 a1 a2 ...)  pt = point at which to eval.
  (if (or (null fa)(> pt 1.0d0)(< pt -1.0d0)) 0.0d0
	 (let ((tnm 1.0d0) (tn pt) ;t_{n-1}=1, t_n= pt
	       (sum (* 0.5d0 (car fa))))  ;; halve the 0th coeff.
	   (pop fa)
	   (				;until (null fa)
	    while fa
	     (incf sum (* (pop fa) tn)) ;; add a_n*t_n
	     (setf tn
	       (- (* 2.0d0 pt tn) 
		  (prog1 tnm (setf tnm tn))))) ;recurrence for next t_n
	   sum)))


;; clenshaw algorithm version...

(defun eval_ts2(a x) ;; clenshaw algorithm
  ;; evaluate p=sum'(a_nT_n(x), i=0 to N)  a is chebseries
  ;; sum' multiplies a_0 by 1/2
  ;; works.
  (declare (optimize (speed 3)(safety 0))
	   (double-float x))
  
  (let* ((bj2 0.0d0)
		(bj1 0.0d0)
		(bj0 0.0d0))
	   (declare(double-float bj2 bj1 bj0))
	   (setf a (reverse a))  ;; could hack this away. maybe later
	   (loop while (cdr a) do
		 (setf bj0
		   (+ (* 2.0d0 x bj1)
		      (- bj2)
		      (the double-float (pop a))))
		 (setf bj2 bj1 bj1 bj0))
	   (+ (* x bj1) (* 0.5d0(pop a))(- bj2))))

	    
;; example of a modest function would be this:

;(defun sfun(r)(eval_ts '(1 1 .5d0 .2d0 .01d0)  r))

;; we can show this is equivalent to this polynomial..

;;0.01 + 0.4*x + 0.92*x^2 + 0.8*x^3 + 0.08*x^4

;; Now, can we obtain the coeff list from any function, including this
;; one by evaluating the formula below.  Or less obviously but
;; equivalently and faster via a version of the FFT, not
;; supplied here. 

#+ignore;; maybe faster version below, using cache for cos.
(defun aj (f j n)
  (let ((in (/ 1.0d0 n)))		; inverse of n
    (* in 2 (- 2 (if (= j n) 0 1))
    (loop for k from 0 to (1- n) sum
	  (let((h(* pi (+ k 0.5d0) in)))
	    (* (cos (* h j))
	       (funcall f (cos h))))))))

;; all the a_j for the function f.

(defun ajs(f n)(loop for j from 0 to (1- n) collect (aj f j n)))

;;(ajs #'sfun 5)
;; compare to (1 1 .5 .2 .01)

;;We can make this faster by creating a cache of cosines at the
;;"Gauss-Lobatto" points.

(defvar *cosc* (make-hash-table))

(defun cachecos(r)(or (gethash r *cosc*)
		       (setf (gethash r *cosc*) (cos r))))
(defun aj (f j n)
  (let ((in (/ 1.0d0 n)))		; inverse of n
    (* in 2 (- 2 (if (= j n) 0 1))
    (loop for k from 0 to (1- n) sum
	  (let((h(* pi (+ k 0.5d0) in)))
	    (* (cachecos (* h j))
	       (funcall f (cachecos h))))))))


;; Heuristic for how many terms we need... Let mm=max(abs(v)), v in ajs.
;; Look for 2 terms t10, t11, say, such that t10/mm and t11/mm are both
;; down in the noise. Say less than 2^-50.


(defun ccs(f &optional (eps #.(expt 2.0d0 -45))) ;; chop chebyshev series
  (declare (optimize (speed 3)(safety 0)))
  (let ((m 4)
	(as nil))
    (;;until
     while (not  (or (null(trailsbig (setf as (ajs f m)) eps)) (> m 128)))
      (setf m (round (* 1.4 m))))
    ;; either we gave up or we won.
    (if (>= m 128) (error "could not find chebyshev series of ~s terms for ~s with tolerance ~s" m f eps)
      as)))

;; trailsbig returns t if either of the 2 trailing coefs are too big
(defun trailsbig(h eps)
  (let ((mm 0)
	(last2 (last h 2))
	(compare 1.0))
    (loop for i in h do (setf mm (max mm (abs i))))
    (setf compare (* mm eps))
     (or (>(abs(first last2)) compare)  ;; condition that trailing nums too big
	 (> (abs(second last2))compare))))


;;; another heuristic which insists that the last two terms are both
;;; down in the noise for TWO successive trials

(defun ccs2(f &optional (eps #.(expt 2.0d0 -45))) ;; chop chebyshev series
  (let* ((m 4)
	 (as0 (ajs f m))
	 (tbas0 (trailsbig as0 eps))
	 (as1 (ajs f  (round (* 1.4 m))))
	 (tbas1 (trailsbig as1 eps)))
    
    (;until (or  (and (null tbas0)  (null tbas1)) (> m 256))
	while (not	(or  (and (null tbas0)  (null tbas1)) (> m 256)))			;  (break "t")
      (setf m (round (* 1.4 m))
	    tbas0 tbas1 
	    as1 (ajs f m) 
	    tbas1 (trailsbig as1 eps)))

    ;; either we gave up or we won.
    (if (> m 256) (error "could not find chebyshev series ~s"f)
      as1)))

    

;; write out an array of a function at gauss-obatto points.

(defun f2gop(f n)
  (let ((ans (make-array n :element-type 'double-float))
	(in (/ 1.0d0 n)))
    (loop for i from 0 to (1- n) do 
	 (setf (aref ans i) (funcall f (cos (* pi (+ i 0.5d0) in)))))
    ans))
	
;; now if a function is given by its gauss-lobatto point values,
;; we could convert to array of Cheby coeffs this way...
;; assume now f is an array of size n, indexed from 0 to n-l.

;; or again, use FFT, if we can figure it out :) 

(defun ajstab(f)
  (let* ((n (length f)) (in (/ 1.0d0 n)))
    (loop for j from 0 to (1- n) collect (ajtab f j n in))))

(defun ajtab (f j n in)
  (* in 2 (- 2 (if (= j n) 0 1))
    (loop for k from 0 to (1- n) sum
	  (let((h(* pi (+ k 0.5d0) in)))
	    (* (cachecos (* h j))
	       (aref f k))))))


;;  pushing this further, for use by Maxima, see the file chebformax.lisp






