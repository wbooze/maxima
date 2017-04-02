(in-package :mpfr)

(defun maxima::$mpfr (expr) (maxbf2mpfr(maxima::$bfloat expr)))
(defun maxima::$mpfr2rat (expr) (mpfr2maxfrac expr))

(defun maxbf2mpfr(bf)
  (let* ((frac (second bf))
	 (expon (third bf))
	 (prec (third(first bf)))
	  (targ 0))
    (mpfr::set-prec prec)
    (setf targ (mpfr::alloc-mpfr))
    (mpfr::mpfr_mul_2si (mpfr::gmpfr-f targ) 
			(mpfr::gmpfr-f (mpfr::into frac))
			(- expon prec)			0 ) ; roundmode
    targ))

;; converting mpfr number into a bigfloat can be done by outof, which
;; passes the array of pieces to a function that puts it together, returns a fraction.

(defun mpfr2maxfrac(mp)(let* ((ans (ga::outof mp))
			      (num (numerator ans))
			      (den (denominator ans)))
			 (if (= 1 den) num 
			 (list '(maxima::rat) num den))))

(defun mpfr2maxbf(mp)($bfloat (mpfr2maxfrac mp)))

    
    ;; newton iteration to a zero of j0
    
(defun nextz(h)(mpfr::+ h (mpfr::/ (mpfr::j0 h) (mpfr::j1 h))))

(defun newt(h)(let* ((old (mpfr::into h))(new (nextz old))(macheps4 (mpfr::expt (tomp 2) (- 4 (mpfr::get-prec)))))
		(loop (if (mpfr::< (mpfr::abs (mpfr::/(mpfr::- old new) old)) macheps4) (return new))
		  		(format t "~%old=~s~%new=~%" old new)
		  (setf old new new (nextz new)))		))

(defun newt-noisy(h)(let* ((old (mpfr::into h))(new (nextz old))(macheps4 (mpfr::expt (tomp 2) (- 4 (mpfr::get-prec))))
			   (count 0))
		;(format t "~%machep*4=~s" macheps4)

		      (loop (if (mpfr::< (mpfr::abs (mpfr::/(mpfr::- old new) old)) macheps4)
				(return new))

					;(format t "~%old=~s~%new=~%" old new)
		  
			      (setf old new new (nextz new))
			   (format t "~%count=~s" (incf count))
			)))

(defun newt-noisy-ramp(h)(let* ((old (mpfr::into h))
				(new (nextz old))
				(final-prec (mpfr::get-prec))
				(macheps4 (mpfr::expt (tomp 2) (- 4 final-prec)))
				(nowprec (min final-prec 50)))
		;(format t "~%machep*4=~s" macheps4)

			   (loop (mpfr::set-prec nowprec)
			     (if (and (>= nowprec final-prec)
					  (mpfr::< (mpfr::abs (mpfr::/(mpfr::- old new) old)) macheps4))
				(return new))

					;(format t "~%old=~s~%new=~%" old new)
		  
			     (setf old new new (nextz new))
			     (setf nowprec (min final-prec (truncate (* 3/2  nowprec))))
;			     (format t "~%count=~s, prec=~s, val=~s" (incf count) nowprec new)
			     )))

;; find a zero of J[0] near point h

(defun newt-ramp(h)(let* ((old (mpfr::into h))
			  (new (nextz old))
			  (working-prec(mpfr::get-prec))
			  (final-prec (+ 4 working-prec))
			  (macheps4 (mpfr::expt (tomp 2) (- 4 final-prec))) ; a few bits of slush?
			 ;; (count 0)
			  (nowprec (min final-prec 50))) ;; how low to set?
		     (declare (fixnum working-prec final-prec nowprec)
			      (optimize (speed 3)(safety 0)))
		     (loop (mpfr::set-prec nowprec)
		       (cond ((and (>= nowprec final-prec)
				   (mpfr::< (mpfr::abs (mpfr::/(mpfr::- old new) old)) macheps4))
			      (mpfr::set-prec working-prec)
			      (return new))
			     (t (setf old new 
				      new  ;; newton iteration for Bessel J0
				      (mpfr::+ new (mpfr::/ (mpfr::j0 new) (mpfr::j1 new))))
				(setf nowprec (min final-prec (truncate (* 3/2  nowprec))))  )))))


;; find an approximation for the nth zero  (n>0) of J[0].

#| jzeroX(n,pi):=block([c=1/((4*n-1)*pi)],
if n<6 then 
[ 17203146289/7153594253, 19873019651/3600133776, 
26375592587/3047887899, 28886417038/2449758951,
19641207599/1315472229][n] else jzero(n,pi) 
|#

(defun jzero(n)  ;; zeros of J[0]
    (cond ((< n 6)
	   (mpfr::into
	   (case n
	     (1 17203146289/7153594253)
	     (2 19873019651/3600133776)
	     (3 26375592587/3047887899)
	     (4 28886417038/2449758951)
	     (5 19641207599/1315472229))))
	  (t
	   (let* ((mp_pi (mpfr::mpfr_pi))
		  (c (mpfr::expt (mpfr::* (mpfr::into (1- (* 4 n)))pi) -1))
		  (c2 (mpfr::* c c))
		  (c3 (mpfr::* c2 c))
		  (c5 (mpfr::* c3 c2))
		  (c7 (mpfr::* c5 c2))
		  (c9 (mpfr::* c7 c2))
		  (c11 (mpfr::* c9 c2))
		  (c13 (mpfr::* c11 c2)))

	   (mpfr::+
	    (mpfr::* (tomp(- n 1/4))  mp_pi)
	    (mpfr::* (tomp 1/2) c)
	    (mpfr::* (tomp -31/6) c3)
	    (mpfr::* (tomp 3779/15) c5)
	    (mpfr::* (tomp -6277237/210) c7)
	    (mpfr::* (tomp  2092163573/315) c9)
	    (mpfr::* (tomp -8249725736393/3465) c11)
	    (mpfr::* (tomp 847496887251128654/675675) c13))))))
(defun tomp(r)(mpfr::into r))

		     
	     
;;	   (n-1/4)*pi +c/2-31/6*c^3 +3779/15*c^5
;;-6277237/210*c^7 + 2092163573/315*c^9 -8249725736393/3465*c^11 +
	   ;;847496887251128654/675675*c^13)
	   
		

;;  utility functions to extract pieces of an mpfr.
;;  assume the base is 2, and the precision is ``number of bits''.
;; we could use another base by assuming precision and exponent
;; use the same base; count would have to be multiplied by log[2](base).


(defun the-expon(v) (aref (gmpfr-f v) 2))
(defun the-prec(v) (aref (gmpfr-f v) 0))

(defun bufsize-max(L) 
  ;; L is list of mpfr numbers. Find the precision needed to add
  ;; them all together without error.
  (let ((count 0)
	(max-expon -1000)
	(min-expon 1000)
	(e 0)
	(p 0))
	(loop for i in L do (incf count) 
			    ;; max location of bit
			    (setf e (the-expon i) p (the-prec i))
			    (setf max-expon (max max-expon (+ p e)))
			    (setf min-expon (min min-expon (- e p))))
	(+ count (- max-expon  min-expon))))

;; so a perfect addition can be done this way:

(defun perfect-mpfr-add(L) ;; a lisp list of mpfr numbers
  (let* ((save-prec (set-prec (bufsize-max L)))
	 (res (sum-mpfr-list L 0)))
    (set-prec save-prec)
    res))

;; (setf a (into (expt 2 1000)) b (into (expt 2 10)) c (into (expt 2 -1000)))
;; (+ (expt 2 1000) (expt 2 10)(expt 2 -1000))


;; bessel function hackery

;; Kahan estimation method noted in literature
;; MATHEMATICS OF COMPUTATION, VOLUME 26, NUMBER 120, OCTOBER 1972
;; Note on Backward Recurrence Algorithms
;; By F. W. J. Olver and D. J. Sookne

;; findb is a simple way to determine where start the reverse recursion.
;; Read the paper for more elaborateness.

;; If you need to find J_0(x) to b bits
;; assuming x is a "generic" location and not
;; smack dab on top of a zero of J_0, then
;; (findb x b) tells you the integer n that is where
;; to start the backward recursion. At J_n.

(defun findb(x b) ;; 
  ;;b is number of bits required for J[0](x) computation
  ;; We could always use findb-mpfr, but findb-fp is faster.
  (if (< b 1022) (findb-fp x b)(findb-mpfr x b)))

(defun findb-mpfr (x b)
  ;; all we need is more exponent range, not much precision.
  (let ((r 1)
	(y0 0)
	(y1 1)
	(saved (set-prec 12))
	(lim (into (expt 2 (1+ b)))))
    (declare (fixnum r)) ;; everything else is mpfr.
    (loop (cond ((> (abs y1)  lim) (set-prec saved) (return (1- r)))
		(t 
		  ;; parallel update y1 := new, y0:= y1, r:=r+1
		 (psetq y1 (- (/ (* (into (* 2 (1- r)))  y1) x)  y0)
			y0 y1 r (1+ r)) )))))


 (defun findb-fp(x b) ;;  ;;b is number of bits required for J[0](x) computation
  (let ((r 1)
	(y0 0.0d0)
	(y1 1.0d0)
	(lim  (expt 2.0d0 (1+ b))))
    (declare (fixnum r)(double-float y0 y1)(optimize (speed 3)))
    (loop (if (> (abs y1)  lim)  (return (1- r)))
      (psetq y1 (- (/ (*  2.0d0  (1- r)  y1) x)  y0)
	     y0 y1 r (1+ r))  ;; parallel update y1 := new, y0:= y1, r:=r+1
      )))



(defun bjn(x n nmax) ;; compute bessel function J_n(x)
  ;; oddly, this pgm unchanged, also computes a rational approx.
  (declare (optimize (speed 3)))
  (let*((lam 0)
       (r (list 1 0))
       (xi (/ 1 x)))
    (loop for k from (- nmax 2) by 1 downto 0 do
	  (push (-(* 2 (1+ k) xi (first r))(second r)) r)
	  (if (evenp k) (setf lam (+ lam (first r))))	  )
    (/ (nth n r) (-(* 2 lam) (first r)))))

;; it helps to boost the precision maybe by 5 bits or more so
;; as to allow for some roundoff slop.  bj0 does that.

  (defun bj0(xin)			; J[0](x) precision is default + slop
  (let* ((precnow (get-prec)) ;; current precision
	 (newprec (set-prec (+ 5 precnow))) ;; boost working precision by 5 bits
	 (res (bjn (into xin) 0 (findb xin (+ 5 precnow)))))
    (set-prec precnow)			;; restore precision
    (mpfr_prec_round (gmpfr-f res) precnow 0) ;; round 5 bits off the answer 
    res))

;; we can memoize fbindb, but we don't want to require an
;; exact match.  That is, remembering exactly (findb 40000.03 10000) is 53069
;; is not as useful as remembering (findb <anything less than 40000> 10000) is no more than 53069.
;; this version is memoizing the nearest larger integer, via (ceiling (ga::outof %))


(defvar findbmemtabp (make-hash-table :test #'eql)) ; precision

;; same as findb except sometimes much faster, for a (near) repeat.
(defun findbmem(x p)
  (let((v(typecase x 
	    (gmpfr (ceiling (ga::outof x)))
	    (integer x)
	    (number (ceiling x))
	    (t (error 'findbmem)))))
    
    (multiple-value-bind (result exists)
	(gethash p findbmemtabp) ;; find the table for this precision
      (cond
       (exists 
	  (multiple-value-bind (res2 ex2)  ;; double hashing
	      (gethash v result)  ;; find the entry for this value if exists
	    (if ex2 res2 
	      			;; else compute it
	      (setf (gethash v  result)(findb v p)))))
       (t ;; make a hash table for this precision
	(setf (gethash p findbmemtabp) (make-hash-table :test #'eql))
	(findbmem v p)			; insert into it
	)))))
;; we  have a hashtable for each precision, and within that,
;; a number for each argument.
;; a better version may be constructed from the mtfq  move-to-front-queue
;; package lower down in this file

;; a more complete bessel program would consider asymptotic methods,
;; taylor series, expansions around different points, etc. 

;; not a winner; the mpfr version is considerably faster.
#+ignore  (defun findb-int(x b) ;;  ;;b is number of bits required for J[0](x) computation
  (let ((r 1)
	(y0 0)
	(y1 1)
	(lim  (ash 2 b)))  ;;(expt 2 (1+ b))
    (declare (fixnum r)(optimize (speed 3)))
    (loop (if (> (abs y1)  lim)  (return (1- r)))
      (psetq y1 (- (/ (*  2 (1- r)  y1) x)  y0)
	     y0 y1 r (1+ r))  ;; parallel update y1 := new, y0:= y1, r:=r+1
      )))


;; move to front queues, using CLOS
;; "database" q  is an array of [dotted pairs (key0. val0) etc of length N.
;; If targ is one of the keys, say keyi, then
;; (a) the queue data in q will be rearranged so (keyi . vali) is at the head.
;; (b) (keyi . vali) will be returned.
;; It is faster to get data at the head of the queue.
;; If the searches are mostly for data never in the queue, the WHOLE list
;; will be searched and that is not so fast.
;; this version is unusual in that if the queue is too long we drop off
;; the least-recently used entry when adding a new one. The assumption is
;; that we can recompute the value we dropped off.

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3)(safety 0)(debug 0)) (fixnum N)(type (simple-array t) q)))

(defclass mtfq () ;; move-to-front queue, of limited size
  ((max-size :initarg size :accessor max-size)
   (actual-size :initform 0 :accessor actual-size)
   (q  :initform nil :accessor q)))

(defun make-mtfq(s)  (make-instance 'mtfq 'size s))

(defmethod putq((key t)(val t) (m mtfq))
  (let ((test (< (actual-size m) (max-size m))))
    (if test(incf (actual-size m)))
    (setf (q m) (cons (cons key val)(if test (q m)(nbutlast (q m) 1) )))    ))

(defmethod getq ((targ t)(m (eql nil))) nil)
  
(defmethod getq ((targ t)(m mtfq)) ;; get value and move to front or return nil
  (let ((hit nil)
	(qm (q m)))
    (cond ((null qm) nil)
	  ((eql (caar qm) targ)(cdar qm))
	  (t
	   (loop for e on qm do
		 (cond ((eql (caadr e) targ)
			;; target pair is now (cadr e), the second guy here
			(setf hit (cdr e))
			(setf (cdr e)(cddr e)) ;remove (cadr e) from list
			(setf (cdr hit) qm) ; put it on the front
			(setf (q m)  hit)
			(return (cdar hit)))))))))

;; example 
#|
(setf yy (make-mtfq 5)) ;; at most 5 will be saved
(dotimes (i 10)(putq i (* 10 i)  yy) (print (q yy))) ;;  only 5 will be left..
(dotimes (i 10)(print (getq i yy)) (print (q yy)))
|#

;; two args.  we could do it by making the index (a . b)
;; e.g. 
;;(defun putq2 (a b val m)(putq (cons a b) val m))
;;(defun getq2 (a b m)(getq (cons a b) m))
;;; OR ..

(defclass mtfq2(mtfq) ;;inherit from mtfq but two indexes, of limited sizes. size in mtfq; 
 ((max-size2 :initarg size2 :accessor max-size2) ))

(defun make-mtfq2(s s2)  (make-instance 'mtfq2 'size s 'size2 s2))

(defmethod putq2 ((a t)(b t) (val t) (m mtfq2))
  (let ((k (getq a m)))
    (cond (k (putq b val k))
	  (t (let ((z (make-mtfq (max-size2 m)))) ;not there. Make one.
	       (putq b val z)		; store b.val there
	       (putq a z m)  )))))

(defmethod getq2 ((a t)(b t) (m mtfq2))  (getq b (getq a m)))

#| ;;examples
(setf zz (make-mtfq2 5 5))
(dotimes (i 10)(putq2 i (* 10 i) (* 101 i) zz))
(dotimes (i 10)(print (getq2 i (* 10 i) zz)))
(defmethod q ((m (eql nil))) nil) ;; see the separate queues..
(dotimes (i 10)(print (q (getq i  zz))))
|#

;;;;;;;;;;apply to bessel...

(defvar findb-mtfq (make-mtfq2 40 100)) ;;max 40 different precisions, 100 points max per precision.
  
(defun findbmem-mtfq(x p)
  (let* ((v(typecase x 
	    (gmpfr (ceiling (ga::outof x)))
	    (integer x)
	    (number (ceiling x))
	    (t (error 'findbmem))))
	 (result (getq2 p v findb-mtfq)))
    (unless result
	   (setf result (findb v p))
	   (putq2 p v result findb-mtfq))
    result))
	
