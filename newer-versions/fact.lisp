;A faster factorial program for computing n! for large n.
#|
fact(n):= f(n,1);

f(n,m) := if m<=n then n else f(n, 2m)* f(n - m; 2m);

(defun f (n m) (if (<= n 1) 1 (* n (f (- n m) m))))

(defun g(n);; n is positive integer
  (multiple-value-bind
      (q r)
      (floor n 3)
    (* (expt 3 q) (f q 3) (f n 3)(f (- n 1) 3)(f (- n 2) 3))))

(defun g3(n);; n is positive integer
  (multiple-value-bind
      (q r)
      (floor n 3)
       (case r
      (0 (* (expt 3 q)(g2 q)(f (- n 1) 3)(f (- n 2) 3))) ;right
      (1 (* n (g2 (1- n))))
      (2 (* n (1- n)(g3 (- n 2)))))))


(defun g2(n);; n is positive integer
  (if (< n 2) 1
  (multiple-value-bind
      (q r)
      (floor n 2)
       (case r
	 (0 ;;(* (expt 2 q)(g2 q)(f (- n 1) 2))
	    (ash (* (g2 q)(f (- n 1) 2)) q)) ;same as above
	 (1 (* n (g2 (1- n))))))))

;; (h2 n) is factorial of n, but faster.
(defun hf2(n);; n is positive integer
  (if (< n 2) 1
    (if (evenp n)
	(let ((q (ash n -1)))
	    (ash (* (hf2 q)(f (- n 1) 2)) q))
      (* n (hf2 (1- n))))))

(defun ff(n m) (if (= m 1) (f n m) ;; faster than (f n m) sometimes.
		 (multiple-value-bind 
		     (q r)(floor n m)
		   (if (= 0 r)(* (expt m q) (hf2 q))
		     (f n m)))))
(defun f (n m) (if (<= n 1) 1 (* n (f (- n m) m))))


;; for mpfr ...




(defun hm2(n);; n is positive integer
  (if (< n 2) (cs 1)
    (if (evenp n)
	(let ((q (ash n -1))
	      (a nil))
	  (setf a (* (hm2 q)(mf (- n 1) 2)))
	  (mpz_mul_2exp (gmpz-z a) (gmpz-z a) q) ;mult by 2^q
	  a)
      (* n (hm2 (1- n))))))

(defun mf (n m) (if (cl::<= n 1) (cs 1) (* n (mf (- n m) m))))



(defun k (n m) ;; (k n 1) is n!
  (if (<= n m) n
    (* (k n (* 2 m))
       (k (- n m)(* 2 m)))))

;;variations on k..

(defun k (n m) ;; (k n 1) is n!
  ;; no speed difference on ACL 7.0 with above. 69 bytes of code vs 167, above.
  (declare (fixnum n m) (optimize (speed 3)(safety 0)(debug 0)))
  (if (<= n m) n
    (* (k n (the fixnum (ash m 1)))
       (k (the fixnum (- n m))(the fixnum (ash m 1))))))


(defun k (n m) ;; (k n 1) is n!
  ;; no speed difference on ACL 7.0 with above. 69 bytes of code vs 167, above.
  (declare (fixnum n m) (optimize (speed 3)(safety 0)(debug 0)))
  (if (<= n m) n
    (let ((z  (* (k n (the fixnum (ash m 1)))
		 (k (the fixnum (- n m))(the fixnum (ash m 1))))))
      (if (fixnump z) (incf fixz)(incf nofixz))
      (if (< z #.(expt 2.0 53)) (incf floz)(incf nofloz))
      z)
    ))


(defun mk (n m) ;; (k n 1) is n!
;;; slightly slower, uses much more storage
  (declare (fixnum n m) (optimize (speed 3)(safety 0)(debug 0)))
  (if (<= n m) (coerce n 'double-float)
    (let ((z  (* (mk n (the fixnum (ash m 1)))
		 (mk (the fixnum (- n m))(the fixnum (ash m 1))))))
     ;; (if (fixnump z) (incf fixz)(incf nofixz))
      ;;(if (> z #.(expt 2.0 53)) (round z) z) ;; not right answer!
      (if (> z #.(expt 2.0 24)) (round z) z)
      ) ))


;; instrument it



(defun *ex(r s) ;; exact mult
  (let ((g (* r s)))
    (if (and(floatp g)(> g 9.0d15)) (* (round r)(round s))
      g)))

(defun *ex(r s) ;; exact mult
  (if (and (floatp r)(floatp s))
        (let ((g (* r s)))
	  (if (> g 9.0d15)(progn (incf *redo*) (* (round r)(round s))) g))
    (* r s)))

#+ignore
(defun *ex(r s) ;; exact mult 
  ;; the change from 9d15 to 1d19 doesn't let anything else slip past 2000!
  (if (and (floatp r)(floatp s))
        (let ((g (* r s)))
	  (if (> g 1.0d19)(progn (incf *redo*) (* (round r)(round s))) g))
    (* r s)))


(defun kf (n m) ;; (k n 1) is n!  ;; use float if possible
  (if (<= n m) n
    (*ex (kf n (* 2 m))
	 (kf (- n m)(* 2 m)))))

(defun kf2 (n m) ;; (k n 1) is n!  ;; use float if possible
  (if (<= n m) n
    (let ((p (kf2 n (* 2 m)))
	  (q (kf2 (- n m)(* 2 m))))
      (if (and (floatp p)(floatp q))
	  (let ((g (the double-float
		     (* (the double-float p)(the double-float q)))))
	    (declare (double-float g))
	    (if (> g 1.0d15) ;; if 19, fails?
		(progn (incf *redo*) (* (round p)(round q)))
	      (progn (incf *no* )g)))
	(* p q)))))



(defun kf4 (n m) ;; (k n 1) is n!  ;; use float if possible
  (declare (optimize (speed 3)(safety 0))
					;(double-float n m)
	   (fixnum n m)
	   (special *c*))
  (if (<= n m) (coerce n 'double-float)
    (let ((p (kf4 n (ash m 1)))
	  (q (kf4 (- n m)(ash m 1))))
      (cond ((and (floatp p)(<= p #.(sqrt(expt 2.0d0 53)))) ;; 9.490626562425156d+7
		(the double-float (* (the double-float p)(the double-float q))))
	    ;; either p is not a float or it is too big
	       (t (incf *c*)(* (round p)(round q)))))))


(defun kg(n)
  (let ((*c* 0))
    (declare (special *c*))
    (kf4 n 1)
    ;(print *c*)
    ))

(defun kf4 (n m) ;; (k n 1) is n!  ;; use float arith if possible
  (declare (optimize (speed 3)(safety 0))   (fixnum n m))
  (if (<= n m) (coerce n 'double-float)
    (let ((p (kf4 n (* 2 m)))
	  (q (kf4 (- n m)(* 2 m))))
      ;; p is bigger than q, so test only p
      (cond ((and (floatp p)(<= p #.(sqrt(expt 2.0d0 53)))) ;; 9.490626562425156d+7
	     (the double-float (* (the double-float p)(the double-float  q))))
	    ;; either p is not a float or it is too big
	    (t ;(incf *c*)
	       (* (round p)(round q)))))))

;; OR  use fixnums when possible....

(defun kf4 (n m) ;; (k n 1) is n!  ;; use fixnum arith if possible
  (declare (optimize (speed 3)(safety 0))   (fixnum n m))
  (if (<= n m) n
    (let ((p (kf4 n (ash m 1)))
	  (q (kf4 (- n m)(ash m 1))))
      ;; p is bigger than q, so test only p
      (cond ((and (fixnump p)(<= p 21170)) ;;sqrt most positive fixnum
	     (the fixnum (* (the fixnum p)(the fixnum  q))))
	    ;; either p is not a float or it is too big
	    (t ;;(incf *c*) ;testing info
	     (* p q ))))))


(defun kf4 (n m) ;; (k n 1) is n!  ;; use fixnum arith if possible
  (declare (optimize (speed 3)(safety 0))   (fixnum n m))
  (if (<= n m) n
    (let ((p (kf4 n (ash m 1)))
	  (q (kf4 (- n m)(ash m 1))))
      ;; p is bigger than q, so test only p
      (cond ((fixnump p)
	     (* (the fixnum p)(the fixnum q)))
	    ;; either p is not a float or it is too big
	    (t ;;(incf *c*) ;testing info
	     (* p q ))))))

|#
  
;;OR look at alternatives in
;;http://www.luschny.de/math/factorial/index.html

;; our thinking mostly corresponds to the split recursive version,
;; simple but mostly fast. With 2X of best upto some huge number.
;; 


(defun k3 (n m ans)			; ans is a gmp number.
  (declare (fixnum n m))
  (cond ((cl::<= n m)(mpz_set_si (gmpz-z ans) n) ans)
	(t
	(let ( (b (alloc-gmpz)))
	  (k3 (- n m) (setf m(ash m 1)) b)
	  (k3 n m ans)
	  (mpz_mul (gmpz-z ans)(gmpz-z ans)(gmpz-z b))
	  ans))))

;; this is not as fast as k3
(defun k4 (n m ans)			; ans is a gmp number.
  (declare (fixnum n m))
  (cond ((cl::<= n m)(mpz_set_si (gmpz-z ans) n) ans)

	((and (cl::= m 2)(evenp n))
	 (let  ((q (ash n -1)))
	  (declare (fixnum q m))
	  ;;(ash (kk q 1) q)
	  (k4 q 1 ans)
	  (mpz_mul_2exp (gmpz-z ans)(gmpz-z ans) q)
	  ))
	
	(t
	(let ( (b (alloc-gmpz)))
	  (k4 (- n m) (setf m(ash m 1)) b)
	  (k4 n m ans)
	  (mpz_mul (gmpz-z ans)(gmpz-z ans)(gmpz-z b))
	  ans))))


(defun kk (n m) ;; (kk n 1) is n!
  (declare (fixnum n m))
  (cond((<= n m) n)
       
       
       ((and (= m 2)(evenp n))
	(let  ((q (ash n -1)))
	  (declare (fixnum q m))
	  (ash (kk q 1) q)))
      #+ignore ;; faster ignoring this clause, at least with ACL arith.
      ((and (> m 2)(= 0 (mod n m)))
	(let  ((q (/ n m)))
	  (declare (fixnum q m))
	  (*(expt m q)(kk q 1))))
       
       (t(* (kk (- n m)(setf m (ash m 1)))
       (kk n m)
       ))))


;      ((and (> m 2)(= 0 (mod n m)))
;	(let  ((q (/ n m)))
;	  (declare (fixnum q m))
;	  (* (mpz_ui_pow_ui targ m q)
;	     (kk q 1))))


(defun kkit(n m)
  (declare (fixnum n m))
  (do ((i n (- i m))
       (p 1 (* i p)))
      ((< i  m) p)
    (declare (fixnum  i))))  ;; not as fast as kk, ever?
  
(defun ff(n m) (if (= m 1) (f n m) ;; faster than (f n m) sometimes.
		 (multiple-value-bind 
		     (q r)(floor n m)
		   (if (= 0 r)(* (expt m q) (hf2 q))
		     (f n m)))))







(defun k4(n);; using resource instead of allocation each time. Faster!!
  (declare (fixnum n))
  (let ((res nil))
    (labels
	((k4i (n m ans)			; ans is a gmp number.
	   (declare (fixnum n m))

	   (if(cl::<= n m)(mpz_set_si ans n)
	     (let ((b (vector-pop res)))
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
	;; the spaces in this vector will get enlarged as needed, pretty fast.
	(k4i n 1 (gmpz-z a))
	a)))) ;; copied from gmp.lisp 3/15/06


(defun k4f(n);; using resource instead of allocation each time. Faster!!
  (declare (fixnum n))
  (let ((res nil)
	(dres (make-array 1 :fill-pointer 0 :initial-element 0.0d0))); or more??
    (labels
	((k4i (n m ans)			; ans is a gmp number.
	   (declare (fixnum n m))

	   (if(cl::<= n m)  (setf (aref ans 0) (coerce n 'double-float))
	     ;;(mpz_set_si ans n) ;;; no, this stuff below won't work
	     (let ((b (vector-pop dres)))
	       (k4i (cl::- n m) (setf m(ash m 1)) b)
	       (k4i n m ans)
	       (mpz_mul ans ans b)
	       (incf (fill-pointer res)) ; (vector-push b gmpres)
	       ))))
      (let* ((L (ceiling (cl::log n 2)))
	     (a (alloc-gmpz)))
	(declare (fixnum L)
		 (type (simple-array 'double-float (*)) dres))
	(setf res (make-array L  :fill-pointer 0))
	(setf dres (make-array L :fill-pointer 0 :initial-element 0.0d0))
	;; allocate enough space for temps.
	;; fill the temps with gmp numbers, zeros.  Just the insides though.
	(dotimes (i L)(vector-push  (gmpz-z (alloc-gmpz)) res))
	;; the spaces in this vector will get enlarged as needed, pretty fast.
	(k4i n 1 (gmpz-z a))
	a))))




;;; this program below works for n up to 22, e.g. (kf4i 22 1 0). Then bignums are
;;; needed. also we need
;;;(setf res (make-array 32 :element-type 'double-float :initial-element 0.0d0))
;;; good up to factorial of (2^32)

(defun kf4i (n m index) ;; (k n 1) is n!  ;; use float if possible
  (declare (optimize (speed 3)(safety 0));(:explain :types)
	   (type (simple-array double-float (32)) res)
	   (fixnum n m index) )
  (cond ((<= n m) (setf (aref res index)(coerce n 'double-float)))
	(t (kf4i n (the fixnum (ash m 1))  index)
	   (kf4i (the fixnum (- n m))(the fixnum (ash m 1)) (the fixnum (1+ index)))
	   (setf (aref res index)
	     (*  (aref res index)(aref res (1+ index)))
	     ))))


(defun kx(n)
  (setf bigres (make-array 32  :initial-element 0))
  (kf4i n 1 0))
(defun kf4i (n m index) ;; (k n 1) is n!  ;; use float if possible
  (declare (optimize (speed 3)(safety 0));(:explain :types)
	   (type (simple-array double-float (32)) res)
	   (type (simple-array t) bigres)
	   (fixnum n m index) )

  (cond ((<= n m) (setf (aref res index)(coerce n 'double-float)))
	(t (kf4i n (the fixnum (ash m 1))  index)
	   (kf4i (the fixnum (- n m))(the fixnum (ash m 1)) (the fixnum (1+ index)))
	   (if (< (aref res index)  #.(sqrt(expt 2.0d0 53))) ;; 9.490626562425156d+7
	       (setf (aref res index)
		 (*  (aref res index)(aref res (1+ index))) )
	     
	     (progn  ;; now need bignums.
	       (if (zerop(aref bigres index))(setf (aref bigres index)(round (aref res index))))
	  ;;     (setf (aref bigres index)(round (aref res index)))
	    ;;   (if (zerop(aref bigres (1+ index))) 
	;;	   (setf (aref bigres (1+ index))(round (aref res (1+ index)))))
	       (setf (aref res index) most-positive-double-float)
	       (setf (aref bigres index)
		 (*  (aref bigres index)(aref bigres (1+ index))) ))))))

	

;; another try

(defun kf4i (n m index) ;; (k n 1) is n!  ;; use float if possible
  (declare (optimize (speed 3)(safety 0));(:explain :types)
	   (type (simple-array double-float (32)) res)
	   (type (simple-array t) bigres)
	   (fixnum n m index) )

  (cond ((<= n m) (setf (aref res index)(coerce n 'double-float)))
	(t (kf4i n (the fixnum (ash m 1))  index)
	   (kf4i (the fixnum (- n m))(the fixnum (ash m 1)) (the fixnum (1+ index)))
	   (cond ((< (aref res index)  #.(sqrt(expt 2.0d0 53))) ;; 9.490626562425156d+7
		  (setf (aref res index)
		 (*  (aref res index)(aref res (1+ index)))))
		 ((zerop (aref bigres index))
		  (setf (aref res index) most-positive-double-float)
		  (setf (aref bigres (1+ index))(round(aref res (1+ index))))
		  (setf (aref bigres index) (* (round(aref res index))
					       (aref bigres (1+ index)))))

		 (t  (setf (aref bigres index)
		       (* (aref bigres index)(aref bigres (1+ index))) ))))))


;;;;;;;;;;;;;;;;;this stuff works by remembering factorials
;;;; previously computed.  If you compute 1000! then computing
;;;;  1001! will take only one more multiply.


(defun k (n m min) ;; (k n 1 1) is n!
  (declare (fixnum n m min))
  (cond ((< n min) 1)
	((<= n m) n)
	(t (* (k n (ash m 1) min)
	      (k (- n m)(ash m 1) min)))))

;; (* (k 100 1 50)(k 49 1 1))  is the same as (k 100 1 1) 
;; (k n 1 n) is n

(defun kmemfac(n)
  (let ((z (lookupfact n)))
  ;; find z, the largest integer <= n such that we know z!
  (if (= (car z) n)(cdr z)
    (rememberfact n 
		  (* (k n 1 (1+ (car z))) ;; could memoize but we don't
		     (cdr z))))))

;;  new factorials can be larger or smaller than previous ones.

(defparameter oldfacts (make-array 0 :adjustable t :fill-pointer t))
(vector-push-extend (cons 0 1) oldfacts)
;; we could stock up on, say factorials of 100, 1000, 5000, 10000,

(defun rememberfact(n f)
  (vector-push-extend (cons n f) oldfacts)
  (setf oldfacts (sort oldfacts #'> :key #'car))
	f)
	
(defun lookupfact(n)
  (loop for i from 0 to (length oldfacts) 
      do (if
	     (<=  (car (aref oldfacts i)) n)
	     (return  (aref oldfacts  i)))))


(defun clearfact()(setf oldfacts (make-array 0 :adjustable t :fill-pointer t))
       (vector-push-extend (cons 0 1) oldfacts))
(clearfact)				; clear it now.


  
;;;;;;;;;;;;;;;;;;;;

;; this is a little faster. maybe fastest of the recursive split
(defun kg (n m) ;; (kg n 1) is n!. ash is arithmetic shift
  (declare (fixnum n m))
    (cond ((and (= m 2)(evenp n)) 
	   (ash (kg (ash n -1) 1) (ash n -1)))
	  ((<= n m) n)
	  (t (* (kg n (ash m 1))
		(kg (- n m)(ash m 1))))))

;;;; what Maxima does (in asum.lsp)
;;;; modified somewhat here, with declarations.

(defun maxfact (n &aux (ans 1))
  (let* ((vec (make-array (if (< n 100) 1 20) :initial-element 1))
	 (m (length vec))
	 (j 0))
 (declare (fixnum j n m j))
  (loop for  i from 1 to n
	 do (setq j (mod i m))
	 (setf (aref vec j) (* (aref vec j) i)))
  (dotimes (v m ans)
    (setq ans (* ans (aref vec v))))))


(defun maxfact (n &aux (ans 1))
  (let* ((vec (make-array (if (< n 100) 1 20) :initial-element 1))
	 (m (length vec))
	 (j 0))
  (loop for  i from 1 to n
	 do (setq j (mod i m))
	 (setf (aref vec j) (* (aref vec j) i)))
  (dotimes (v m ans)
    (setq ans (* ans (aref vec v))))))



(defun nloop(n) ;; make a loop of n items, all 1. A circular list.
  (let*((start (list 1))
	(end start))
    (dotimes (i n (setf (cdr end) start))
      (declare (fixnum n i))
      (setq start (cons 1 start)))))

(setf  *print-circle* t)

(defun cloop(n) ;;   (cloop 5) is #1=(5 4 3 2 1 . #1#)  ;not used.
  (let*((start (list 1))
	(end start))
    (dotimes (i (1- n) (setf (cdr end) start))
      (setq start (cons (+ 2 i) start)))))

(defun loopprod (l) 
  ;; efficient product of all the approximately equal
  ;; numbers in the loop trying to keep the sizes of inputs
  ;; approximately balanced.  the circular loop l is destroyed in the
  ;; process.
  (declare (optimize (speed 3)(safety 0)))
  (cond ((eq(cdr l) l)(car l))
	(t (setf (car l)(* (car l)(cadr l)))
	   (setf (cdr l)(cddr l))
	   (loopprod (cdr l)))))


;; a factorial but using loops for storage

(defun maxfact (n)
  (declare (fixnum n)(optimize (speed 3)(safety 0)))
  (let ((aloop (nloop (if (< n 100) 5 100)))) ; heuristic
    (loop for  i from 1 to n
	do  (setf (car aloop) (* (car aloop) i))
	    (setf aloop (cdr aloop)))
    (loopprod aloop)))


;;;;;;;;;;;;;;;loops for storage and memoization?


(defun maxfact (n min)
  (declare (fixnum n)(optimize (speed 3)(safety 0)))
  (let ((aloop (nloop (if (< n 100) 5 100)))) ; heuristic
    (loop for  i from min to n
	do  (setf (car aloop) (* (car aloop) i))
	    (setf aloop (cdr aloop)))
    (loopprod aloop)))

;; (* (maxfact n  7) (maxfact 6 1))  is (maxfact 7 1))

(defun kmemfac(n)
  (let ((z (lookupfact n)))
  ;; find z, the largest integer <= n such that we know z!
  (if (= (car z) n)(cdr z)
    (rememberfact n 
		  (* (maxfact n (1+ (car z)))
		     (cdr z))))))



(defun maxfactx (n)
  (declare (fixnum n)(optimize (speed 3)(safety 0)))
  (let* ((z (if (< n 100) 5 100))
	 (aloop (cloop z))) ; heuristic
    (loop for  i from (1+ z)  to n
	do  (setf (car aloop) (* (car aloop) i))
	    (setf aloop (cdr aloop)))
    (loopprod aloop)))

;; maxfactx could be done nicely in gmpz.
;; the (setf (car aloop)(* (car aloop) i) ) is one call
;; in loopprod, the * would be one call, also.
;; cutting short the iteration by memoization would also
;; help.

(defun gmaxfactx (n)
  (declare (fixnum n)(optimize (speed 3)(safety 0)))
  (let* ((z (if (< n 100) 5 100))
	 (aloop (gcloop z))) ; heuristic
    (loop for  i from (1+ z)  to n
	do  ;;(setf (car aloop) (* (car aloop) i))
	  (gmp::mpz_mul_si (car aloop) (car aloop) i)
	    (setf aloop (cdr aloop)))
    (gloopprod aloop)))

(defun gcloop(n) ;;   (cloop 5) is #1=(5 4 3 2 1 . #1#) with gmpz insides 
  (let*((start (list (gmp::gmpz-z (gmp::into 1))))
	(end start))
    (dotimes (i (1- n) (setf (cdr end) start))
      (setq start (cons (gmp::gmpz-z (gmp::into (+ 2 i))) start)))))


(defun gloopprod (l) 
  ;; efficient product of all the approximately equal
  ;; numbers in the loop trying to keep the sizes of inputs
  ;; approximately balanced.  the circular loop l is destroyed in the
  ;; process.
  (declare (optimize (speed 3)(safety 0)))
  (cond ((eq(cdr l) l)(gmp::make-gmpz :z (car l)))
	(t ;;(setf (car l)(* (car l)(cadr l)))
	 (gmp::mpz_mul (car l)(car l)(cadr l))
	   (setf (cdr l)(cddr l))
	   (gloopprod (cdr l)))))


