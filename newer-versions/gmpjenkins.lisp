;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: mpfr; Base: 10 -*-
;;; Jenkins-Traub zero finder in MPFR
;;; April 13, 2006 RJF
;;; This is Alg. 419 CACM by Jenkins and Traub, previously
;;; converted to lisp. See file jenkins.lisp in this directory.

;;; This version uses generic arithmetic, and does not try to optimize
;;; what should be straightforward, to use destructive stores
;;; in the macro "store" below.

;;; I have not changed the parameters regarding accuracy and precision
;;; from the settings for double (maybe single??)machine floats.
;;; They should be recomputed depending on the number of bits
;;; carried by the mpfr setting. -- RJF

(in-package :mpfr)

;;(defmacro store(a b) `(setf ,a (into  ,b )))
;;(defmacro store(a b) `(setf ,a ,b)) ;; risky?

(declare (special *rndmode*))
;; even riskier
(defmacro store(a b) `(mpfr_set (gmpfr-f ,a)(gmpfr-f ,b) *rndmode*))

(defparameter $polyfactor nil)
(defun cdb(r)(into r))
(defmacro 1+$(&rest args)`(1+ (the gmpfr ,@args)))
(defmacro 1-$(&rest args)`(1- (the gmpfr ,@args)))
(defmacro f1-(&rest args)`(cl::1- (the fixnum ,@args)))	
(defmacro f1+(&rest args)`(cl::1+ (the fixnum ,@args)))	

(defmacro *f  (a b) `(ga::two-arg-* (the gmpfr ,a)(the gmpfr ,b)))
(defmacro //f (a b) `(ga::two-arg-/ (the gmpfr ,a)(the gmpfr ,b)))

(defmacro -$(&rest args)(cons '- args))
(defmacro +$(&rest args)(cons '+ args))
(defmacro *$(&rest args)(cons '* args))
(defmacro //$(&rest args)(cons '/ args))
;;; fixnum only versions
(defmacro f+(&rest args)(cons 'cl::+ (mapcar #'(lambda (m)(list 'the 'fixnum m)) args)))
(defmacro f*(&rest args)(cons 'cl::* (mapcar #'(lambda (m)(list 'the 'fixnum m)) args)))
(defmacro f-(&rest args)(cons 'cl::- (mapcar #'(lambda (m)(list 'the 'fixnum m)) args)))


(defmacro fix(x) `(truncate2fix ,x))	; I guess..
;;(defparameter logbas (log 2.0d0))




(declaim
 (optimize (speed 3))
   (special logbas infin smalno are mre cr ci sr si tr ti zr zi n nn bool conv pvr
	    pvi $partswitch $keepfloat $demoivre $listconstvars $algebraic acp-sl
	    $polyfactor polysc polysc1 $ratfac $programmode)
   (fixnum degree n nn j l l1 l2 l3 cnt1 cnt2 jj polysc polysc1)
   ) 

(declaim
 (special *pr-sl* *pi-sl* *shr-sl* *shi-sl* *qpr-sl* *qpi-sl* *hr-sl* *hi-sl*
		 *qhr-sl* *qhi-sl*))

(defun _f (number scale) (scale-float number scale)) ;; works for gmpfr too

(setq acp-sl 0.2d0) 

;; written by RJF....
;; example call:   (myroots #(1 0 0 1))

;;(defun $myroots(vv)(cons '(mlist) (myroots vv))) ;maxima call

(defun myroots (vv);; based on $allroots ; vv is a polynomial vector
  (let* ((degree (1- (length vv)))
	 (nn (1+ degree))
	 ($polyfactor nil))		;separate complex conj pairs
    (if (= degree 1) ;linear
	(return-from myroots (vector (/ (- (elt vv 0))(elt vv 1)))))
    (if (= degree 0) ;constant.
	(return-from myroots (vector )))
    ;; should probably make sure v is not complex. RJF
    ;; using all these global arrays slows us down some.
	    
    (setq *pr-sl* (reverse (map 'vector #'cdb vv)))
    (setq *pi-sl* (make-array nn :element-type 'gmpfr))
    (dotimes (i nn)(setf (aref *pi-sl* i)(into 0)))
    (setq *shr-sl* (make-array nn :element-type 'gmpfr))
    (setq *shi-sl* (make-array nn :element-type 'gmpfr))
    (setq *qpr-sl* (make-array  nn :element-type 'gmpfr))
    (setq *hr-sl* (make-array (1- nn) :element-type 'gmpfr))
    (setq *qhr-sl* (make-array (1- nn) :element-type 'gmpfr))
    (setq nn degree)
	 
 ;;;;; HERE IS THE CALL TO THE REAL PROGRAM--rjf
    (rpoly-sl degree)

    ;;; if nn is not zero,
    (if (> nn 0)   (format t "~% trouble: rpoly program:  ~s roots lost" nn ))
    
    ;; was #'complex instead of 'list. need complex MPFC ?
    (map 'vector 'list (subseq *pr-sl* 1)(subseq *pi-sl* 1))))


(defun xmyroots (vv);; based on $allroots ; vv is a polynomial vector ??
  (let* ((degree (1- (length vv)))
	 (nn (1+ degree))
	 ($polyfactor t)) ; keep conj pairs together
    (if (= degree 1) ;linear
	(return-from myroots (vector (/ (- (elt vv 0))(elt vv 1)))))
    (if (= degree 0) ;constant.
	(return-from myroots (vector )))
    ;; should probably make sure v is not complex. RJF
    ;; using all these global arrays slows us down some.
	    
    (setq *pr-sl* (reverse (map 'vector #'cdb vv)))
    (setq *pi-sl* (make-array nn :element-type 'gmpfr))
    (dotimes (i nn)(setf (aref *pi-sl* i)(into 0)))
    (setq *shr-sl* (make-array nn :element-type 'gmpfr))
    (setq *shi-sl* (make-array nn :element-type 'gmpfr))
    (setq *qpr-sl* (make-array  nn :element-type 'gmpfr))
    (setq *hr-sl* (make-array (1- nn) :element-type 'gmpfr))
    (setq *qhr-sl* (make-array (1- nn) :element-type 'gmpfr))
    (setq nn degree)
	 
 ;;;;; HERE IS THE CALL TO THE REAL PROGRAM--rjf
    (rpoly-sl degree)

    ;;; if nn is not zero,
    (if (> nn 0)   (format t "~% trouble: rpoly program:  ~s roots lost" nn ))
    
    ;; was #'complex instead of 'list. need complex MPFC ?
    (map 'vector 'list (subseq *pr-sl* 1)(subseq *pi-sl* 1))))

(defun myrootsc (vv);; complex version

  (let* ((degree (1- (length vv)))
	 (nn (1+ degree))
	 ($polyfactor nil))		;separate complex conj pairs
   
    ;; should probably make sure v is not complex. RJF
	    
    (setq *pr-sl* (reverse (map 'vector #'(lambda(x)(cdb(realpart x))) vv)))
    (setq *pi-sl* (reverse (map 'vector #'(lambda(x)(cdb(imagpart x))) vv)))
    (setq *shr-sl* (make-array nn :element-type 'gmpfr))
    (setq *shi-sl* (make-array nn :element-type 'gmpfr))
    (setq *qpr-sl* (make-array  nn :element-type 'gmpfr))
    (setq *hr-sl* (make-array (1- nn) :element-type 'gmpfr))
    (setq *qhr-sl* (make-array (1- nn) :element-type 'gmpfr))
    (setq nn degree)
	 
 ;;;;; HERE IS THE CALL TO THE REAL PROGRAM--rjf
    (rpoly-sl degree)

    ;;; if nn is not zero,
    (if (> nn 0)   (format t "~% trouble: rpoly program:  ~s roots lost" nn ))
    
    ;(map 'vector #'complex (subseq *pr-sl* 1)(subseq *pi-sl* 1))
      (map 'vector #'list (subseq *pr-sl* 1)(subseq *pi-sl* 1))))

;; end of $allroots main program.  
;; do we need complex coeff version, below? RJF

(defun cpoly-sl (degree) 
       ((lambda (logbas infin smalno are mre xx yy cosr sinr cr ci sr si tr ti zr zi bnd
		 n polysc polysc1 conv) 
		(setq mre (*$ 2.0d0 (sqrt 2.0d0) are) yy (-$ xx))
		(do ((i degree (f1- i)))
		    ((not (and (= 0 (aref *pr-sl* i)) (= 0 (aref *pi-sl* i)))) (setq nn i n (f1- i))))
		(setq degree nn)
		(do ((i 0 (f1+ i)))
		    ((> i nn))
		    (setf (aref *shr-sl* i) (cmod-sl (aref *pr-sl* i) (aref *pi-sl* i))))
		(scale-sl )
		(do nil
		    ((> 2 nn)
		     (cdivid-sl (-$ (aref *pr-sl* 1)) (-$ (aref *pi-sl* 1)) (aref *pr-sl* 0)
				(aref *pi-sl* 0))
		     (setf (aref *pr-sl* 1) cr)
		     (setf (aref *pi-sl* 1) ci)
		     (setq nn 0))
		    (do ((i 0 (f1+ i)))
			((> i nn))
			(setf (aref *shr-sl* i) (cmod-sl (aref *pr-sl* i) (aref *pi-sl* i))))
		    (setq bnd (cauchy-sl ))
		    (catch 'newroot
			   (do ((cnt1 1 (f1+ cnt1)))
			       ((> cnt1 2))
			       (noshft-sl 5)
			       (do ((cnt2 1 (f1+ cnt2)))
				   ((> cnt2 9))
				   (setq xx (prog2 nil
						   (-$ (*$ cosr xx) (*$ sinr yy))
						   (setq yy (+$ (*$ sinr xx)
								(*$ cosr yy)))) 
					 sr (*$ bnd xx) 
					 si (*$ bnd yy))
				   (fxshft-sl (f* 10 cnt2))
				   (cond (conv (setf (aref *pr-sl* nn) zr)
					       (setf (aref *pi-sl* nn) zi)
					       (setq nn n n (f1- n))
					       (do ((i 0 (f1+ i)))
						   ((> i nn))
						   (setf (aref *pr-sl* i) (aref *qpr-sl* i))
						   (setf (aref *pi-sl* i) (aref *qpi-sl* i)))
					       (throw 'newroot t))))))
		    (or conv (return t)))
		(do ((i (f1+ nn) (f1+ i)))
		    ((> i degree))
		    (setf (aref *pr-sl* i) (_f (aref *pr-sl* i) polysc1))
		    (setf (aref *pi-sl* i) (_f (aref *pi-sl* i) polysc1)))
		(do ((i 0 (f1+ i)) (j (f- polysc (f* polysc1 degree)) (f+ j polysc1)))
		    ((> i nn))
		    (setf (aref *pr-sl* i) (_f (aref *pr-sl* i) j))
		    (setf (aref *pi-sl* i) (_f (aref *pi-sl* i) j)))
		nn)
	
	;; these constants should all be made relevant to mpfr settings
	
	(log 2.0d0) most-positive-long-float least-positive-long-float
	(float-precision acp-sl )
	(into 0) (/ 1 (sqrt (into 2))) (into 0) -0.069756474d0 0.99756405d0
	(into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) nil)) 

(defun noshft-sl (l1) 
       (do ((i 0 (f1+ i)) (xni (cdb nn) (1-$ xni)) (t1 (//$ (cdb nn))))
	   ((> i n))
	   (setf (aref *hr-sl* i) (*$ (aref *pr-sl* i) xni t1))
	   (setf (aref *hi-sl* i) (*$ (aref *pi-sl* i) xni t1)))
       (do ((jj 1 (f1+ jj)))
	   ((> jj l1))
	   (cond ((> (cmod-sl (aref *hr-sl* n) (aref *hi-sl* n)) (*$ 10.0d0 are (cmod-sl (aref *pr-sl* n) (aref *pi-sl* n))))
		  (cdivid-sl (-$ (aref *pr-sl* nn)) (-$ (aref *pi-sl* nn)) (aref *hr-sl* n) (aref *hi-sl* n))
		  (setq tr cr ti ci)
		  (do ((j n (f1- j)) (t1) (t2))
		      ((> 1 j))
		      (setq t1 (aref *hr-sl* (f1- j)) t2 (aref *hi-sl* (f1- j)))
		      (setf (aref *hr-sl* j) (-$ (+$ (aref *pr-sl* j) (*f t1 tr)) (*f t2 ti)))
		      (setf (aref *hi-sl* j) (+$ (aref *pi-sl* j) (*f t1 ti) (*f t2 tr))))
		  (setf (aref *hr-sl* 0) (aref *pr-sl* 0))
		  (setf (aref *hi-sl* 0) (aref *pi-sl* 0)))
		 (t (do ((j n (f1- j)))
			((> 1 j))
			(setf (aref *hr-sl* j) (aref *hr-sl* (f1- j)))
			(setf (aref *hi-sl* j) (aref *hi-sl* (f1- j))))
		    (setf (aref *hr-sl* 0) (into 0))
		    (setf (aref *hi-sl* 0) (into 0)))))) 

(defun fxshft-sl (l2) 
       ((lambda (test pasd otr oti svsr svsi bool pvr pvi) 
		(polyev-sl)
		(setq conv nil)
		(calct-sl)
		(do ((j 1 (f1+ j)))
		    ((> j l2))
		    (setq otr tr oti ti)
		    (nexth-sl)
		    (calct-sl)
		    (setq zr (+$ sr tr) zi (+$ si ti))
		    (cond ((and (not bool) test (not (= j l2)))
			   (cond ((> (*$ #.(into 1/2) (cmod-sl zr zi))
				     (cmod-sl (-$ tr otr) (-$ ti oti)))
				  (cond (pasd (do ((i 0 (f1+ i)))
						  ((> i n))
						  (setf (aref *shr-sl* i) (aref *hr-sl* i))
						  (setf (aref *shi-sl* i) (aref *hi-sl* i)))
					      (setq svsr sr svsi si)
					      (vrshft-sl 10)
					      (and conv (return nil))
					      (setq test nil)
					      (do ((i 0 (f1+ i)))
						  ((> i n))
						  (setf (aref *hr-sl* i) (aref *shr-sl* i))
						  (setf (aref *hi-sl* i) (aref *shi-sl* i)))
					      (setq sr svsr si svsi)
					      (polyev-sl)
					      (calct-sl))
					((setq pasd t))))
				 ((setq pasd nil))))))
		(or conv (vrshft-sl 10))
		nil)
	t nil (into 0) (into 0) (into 0) (into 0) nil (into 0) (into 0))) 

(defun vrshft-sl (l3) 
       (setq conv nil sr zr si zi)
       (do ((i 1 (f1+ i)) (bool1 nil) (mp) (ms) (omp) (relstp) (tp) (r1))
	   ((> i l3))
	   (polyev-sl)
	   (setq mp (cmod-sl pvr pvi) ms (cmod-sl sr si))
	   (cond ((> (*$ 20.0d0 (errev-sl ms mp)) mp)
		  (setq conv t zr sr zi si)
		  (return t)))
	   (cond ((= i 1) (setq omp mp))
		 ((or bool1 (> omp mp) (not (< relstp 0.05)))
		  (cond ((> (*$ 0.1d0 mp) omp) (return t)) (t (setq omp mp))))
		 (t (setq tp relstp bool1 t)
		    (cond ((> are relstp) (setq tp are)))
		    (setq r1 (sqrt tp) 
			  sr (prog2 nil
				    (-$ (*$ (1+$ r1) sr) (*f r1 si))
				    (setq si (+$ (*$ (1+$ r1) si) (*f r1 sr)))))
		    (polyev-sl)
		    (do ((j 1 (f1+ j))) ((> j 5)) (calct-sl) (nexth-sl))
		    (setq omp infin)))
	   (calct-sl)
	   (nexth-sl)
	   (calct-sl)
	   (or bool
	       (setq relstp (//$ (cmod-sl tr ti) (cmod-sl sr si)) 
		     sr (+$ sr tr) 
		     si (+$ si ti))))) 

(defun calct-sl nil 
       (do ((i 1 (f1+ i))
	    ($t)
	    (hvr (setf (aref *qhr-sl* 0) (aref *hr-sl* 0)))
	    (hvi (setf (aref *qhi-sl* 0) (aref *hi-sl* 0))))
	   ((> i n)
	    (setq bool (not (> (cmod-sl hvr hvi) (*$ 10.0d0 are (cmod-sl (aref *hr-sl* n) (aref *hi-sl* n))))))
	    (cond ((not bool) (cdivid-sl (-$ pvr) (-$ pvi) hvr hvi) (setq tr cr ti ci))
		  (t (setq tr (into 0) ti (into 0))))
	    nil)
	   (setq $t (-$ (+$ (aref *hr-sl* i) (*f hvr sr)) (*f hvi si)))
	   (setf (aref *qhi-sl* i) (setq hvi (+$ (aref *hi-sl* i) (*f hvr si) (*f hvi sr))))
	   (setf (aref *qhr-sl* i) (setq hvr $t)))) 

(defun nexth-sl nil 
       (cond (bool (do ((j 1 (f1+ j)))
		       ((> j n))
		       (setf (aref *hr-sl* j) (aref *qhr-sl* (f1- j)))
		       (setf (aref *hi-sl* j) (aref *qhi-sl* (f1- j))))
		   (setf (aref *hr-sl* 0) (into 0))
		   (setf (aref *hi-sl* 0) (into 0)))
	     (t (do ((j 1 (f1+ j)) (t1) (t2))
		    ((> j n))
		    (setq t1 (aref *qhr-sl* (f1- j)) t2 (aref *qhi-sl* (f1- j)))
		    (setf (aref *hr-sl* j) (-$ (+$ (aref *qpr-sl* j) (*f t1 tr)) (*f t2 ti)))
		    (setf (aref *hi-sl* j) (+$ (aref *qpi-sl* j) (*f t1 ti) (*f t2 tr))))
		(setf (aref *hr-sl* 0) (aref *qpr-sl* 0))
		(setf (aref *hi-sl* 0) (aref *qpi-sl* 0))))
       nil) 

(defun polyev-sl nil 
  (setq pvr (setf (aref *qpr-sl* 0) (aref *pr-sl* 0))
	pvi (setf (aref *qpi-sl* 0) (aref *pi-sl* 0)))
       (do ((i 1 (f1+ i)) ($t))
	   ((> i nn))
	 ;; could do a better job of saving temps here.
	   (setq $t (-$ (+$ (aref *pr-sl* i) (*f pvr sr)) (*f pvi si)))
	   (setf (aref *qpi-sl* i) (setq pvi (+$ (aref *pi-sl* i) (*f pvr si) (*f pvi sr))))
	   (setf (aref *qpr-sl* i) (setq pvr $t)))) 

(defun errev-sl (ms mp) 
       (-$ (*$ (do ((j 0 (f1+ j))
		    (e (//$ (*$ (cmod-sl (aref *qpr-sl* 0) (aref *qpi-sl* 0)) mre) (+$ are mre))))
		   ((> j nn) e)
		   (setq e (+$ (cmod-sl (aref *qpr-sl* j) (aref *qpi-sl* j)) (*$ e ms))))
	       (+$ are mre))
	   (*$ mp mre))) 

(defun cauchy-sl nil 
       ((lambda (x xm) 
		(setf (aref *shr-sl* nn) (-$ (aref *shr-sl* nn)))
		(cond ((not (= 0 (aref *shr-sl* n)))
		       (setq xm (-$ (//$ (aref *shr-sl* nn) (aref *shr-sl* n))))
		       (cond ((> x xm) (setq x xm)))))
		(do ((f))
		    (nil)
		    (setq xm (*$ 0.1d0 x) f (aref *shr-sl* 0))
		    (do ((i 1 (f1+ i))) ((> i nn)) (setq f (+$ (aref *shr-sl* i) (*f f xm))))
		    (cond ((not (< (into 0) f)) (return t)))
		    (setq x xm))
		(do ((dx x) (df) (f))
		    ((> #.(into 0.005)  (abs (//$ dx x))) x) ;; arbitrary?
		    (setq f (aref *shr-sl* 0) df f)
		    (do ((i 1 (f1+ i)))
			((> i n))
			(setq f (+$ (*$ f x) (aref *shr-sl* i)) df (+$ (*$ df x) f)))
		    (setq f (+$ (*$ f x) (aref *shr-sl* nn)) dx (//$ f df) x (-$ x dx))))
	(exp (//$ (-$ (log (aref *shr-sl* nn)) (log (aref *shr-sl* 0))) (cdb nn)))
	(into 0))) 

(defun scale-sl nil
       (do ((i 0 (f1+ i)) (j 0) (x (into 0)) (dx (into 0)))
	   ((> i nn)
	    (setq x (//$ x (cdb (f- (f1+ nn) j)))
		  dx (//$ (-$ (log (aref *shr-sl* nn)) (log (aref *shr-sl* 0))) (cdb nn))
		  polysc1 (fix (+$ #.(into 1/2) (//$ dx logbas)))
		  x (+$ x (*$ (cdb (f* polysc1 nn)) logbas #.(into 1/2)))
		  polysc (fix (+$ #.(into 1/2) (//$ x logbas)))))
	   (cond ((= 0 (aref *shr-sl* i)) (setq j (f1+ j)))
		 (t (setq x (+$ x (log (aref *shr-sl* i)))))))
       (do ((i nn (f1- i)) (j (f- polysc) (f+ j polysc1)))
	   ((< i 0))
	   (setf (aref *pr-sl* i) (_f (aref *pr-sl* i) j))
	   (setf (aref *pi-sl* i) (_f (aref *pi-sl* i) j))))

(defun cdivid-sl (ar ai br bi) 
       ((lambda (r1) (cond ((and (= 0 br) (= 0 bi)) (setq cr (setq ci infin)))
			   ((> (abs bi) (abs br))
			    (setq r1 (//f br bi) 
				  bi (+$ bi (*f br r1)) 
				  br (+$ ai (*f ar r1)) 
				  cr (//f br bi) 
				  br (-$ (*f ai r1) ar) 
				  ci (//f br bi)))
			   ((setq r1 (//f bi br) 
				  bi (+$ br (*f bi r1)) 
				  br (+$ ar (*f ai r1)) 
				  cr (//f br bi) 
				  br (-$ ai (*f ar r1)) 
				  ci (//f br bi)))))
	(into 0))
       nil) 

(defun cmod-sl (ar ai) 
       (setq ar (abs ar) ai (abs ai))
       (cond ((> ai ar) (setq ar (//f ar ai)) (*$ ai (sqrt (1+$ (*f ar ar)))))
	     ((> ar ai) (setq ai (//f ai ar)) (*$ ar (sqrt (1+$ (*f ai ai)))))
	     ((*$ 1.41421357 ar)))) 

;;*page

;;;this is the algorithm for doing real polynomials.  it is algorithm 493 from
;;;acm toms vol 1 p 178 (1975) by jenkins.  note that array indexing starts from 0.
;;;the names of the arrays have been changed to be the same as for cpoly.
;;;the correspondence is:  p - pr-sl, qp - qpr-sl, k - hr-sl, qk - qhr-sl, svk - shr-sl,
;;;temp - shi-sl.  the roots are put in pr-sl and pi-sl.
;;;the variable si appears not to be used here


(declaim(special sr u v a b c d a1 a3 a7 e f g h szr szi lzr lzi are mre n nn nz
		  #+gcl type ui vi s $polyfactor arp-sl type)
	 (double-float a a0 a1 a3 a4 a5 a6 a7 aa are b b0 b1 b2 logbas bb betas betav bnd c c0
		 c1 c2 c3 c4 cc cosr d d0 e ee f g h infin kv lzi lzr mp mre ms omp
		 oss ots otv ovv pv relstp s sinr smalno sr ss svu svv szi szr t1 ts
		 tss tv tvv u ui v vi vv xx yy zm arp-sl)
	 (fixnum cnt degree i iflag j jj l l2 n nn nz #+gcl type) 
	) 

(defparameter arp-sl (into 1))

;;;; this is the program we want to work... RJF

(defun rpoly-sl (degree) 
       ((lambda (logbas infin smalno are mre xx yy cosr sinr aa cc bb bnd sr u v t1 szr
		 szi lzr lzi nz n polysc polysc1 zerok conv1) 
		(setq mre are yy (-$ xx))
		(do ((i degree (f1- i))) ((not (= 0 (aref *pr-sl* i))) (setq nn i n (f1- i))))
		(setq degree nn)
		(do ((i 0 (f1+ i))) ((> i nn)) (store (aref *shr-sl* i) (abs (aref *pr-sl* i))))
		(scale-sl)
		(do nil
		    ((< nn 3)
		     (cond ((= nn 2)
			    (quad-sl (aref *pr-sl* 0) (aref *pr-sl* 1) (aref *pr-sl* 2))
			    (cond ((and $polyfactor (not (= 0 szi)))
				   ;;(store (aref *pr-sl* 2) (//$ (aref *pr-sl* 2) (aref *pr-sl* 0)))
				   (mpz_div (gmpfr-f(aref *pr-sl* 2)(gmpfr-f (aref *pr-sl* 2))
						   (gmpfr-f(aref *pr-sl* 0))))
				   
				   
				  ;; (store (aref *pr-sl* 1) (//$ (aref *pr-sl* 1) (aref *pr-sl* 0)))
				   (mpfr_div (gmpfr-f(aref *pr-sl* 1)(gmpfr-f (aref *pr-sl* 1))
						    (gmpfr-f(aref *pr-sl* 0))))
				   
				   (store (aref *pi-sl* 2) (into 1)))
				  (t (store (aref *pr-sl* 2) szr)
				     (store (aref *pi-sl* 2) szi)
				     (store (aref *pr-sl* 1) lzr)
				     (store (aref *pi-sl* 1) lzi))))
			   (t 
			    (store (aref *pr-sl* 1) (-$ (//$ (aref *pr-sl* 1) (aref *pr-sl* 0))))
			    ))
		     (setq nn 0))
		  (do ((i 0 (f1+ i))) ((> i nn)) 
		    ;;(store (aref *shr-sl* i) (abs (aref *pr-sl* i)))
		    (mpfr_abs (gmpfr-f(aref *shr-sl* i))(gmpfr-f (aref *pr-sl* i)) *rndmode*)
		    )
		    (setq bnd (cauchy-sl))
		    (do ((i 1 (f1+ i)))
			((> i n))
		   ;;   (store (aref *hr-sl* i) (//$ (*$ (cdb (f- n i)) (aref *pr-sl* i)) (cdb n)))
		      (mpfr_div_si (gmpfr-f (aref *hr-sl* i))
				   (gmpfr-f (*$ (cdb (f- n i)) (aref *pr-sl* i)))
				   n *rndmode*)
		      )
		    (store (aref *hr-sl* 0) (aref *pr-sl* 0))
		    (setq aa (aref *pr-sl* nn) bb (aref *pr-sl* n) zerok (= 0 (aref *hr-sl* n)))
		    (do ((jj 1 (f1+ jj)))
			((> jj 5))
			(setq cc (aref *hr-sl* n))
			(cond (zerok (do ((j n (f1- j)))
					 ((< j 1))
					 (store (aref *hr-sl* j) (aref *hr-sl* (f1- j))))
				     (store (aref *hr-sl* 0) (into 0))
				     (setq zerok (= 0 (aref *hr-sl* n))))
			      (t (setq t1 (-$ (//$ aa cc)))
				 (do ((j n (f1- j)))
				     ((< j 1))
				     (store (aref *hr-sl* j) (+$ (*$ t1 (aref *hr-sl* (f1- j))) (aref *pr-sl* j))))
				 (store (aref *hr-sl* 0) (aref *pr-sl* 0))
				 (setq zerok (not (> (abs (aref *hr-sl* n))
						     (*$ (abs bb) are 1(into 0))))))))
		    (do ((i 0 (f1+ i))) ((> i n)) (store (aref *shi-sl* i) (aref *hr-sl* i)))
		    (do ((cnt 1 (f1+ cnt)))
			((> cnt 20) (setq conv1 nil))
			(setq xx (prog2 nil
					(-$ (*$ cosr xx) (*$ sinr yy))
					(setq yy (+$ (*$ sinr xx) (*$ cosr yy)))) 
			      sr (*$ bnd xx) 
			      u (*$ -2 sr) 
			      v bnd)
			(fxshfr-sl (f* 20 cnt))
			(cond ((> nz 0)
			       (store (aref *pr-sl* nn) szr)
			       (store (aref *pi-sl* nn) szi)
			       (cond ((= nz 2)
				      (store (aref *pr-sl* n) lzr)
				      (store (aref *pi-sl* n) lzi)
				      (cond ((and $polyfactor (not (= 0 szi)))
					     (store (aref *pr-sl* nn) v)
					     (store (aref *pr-sl* n) u)
					     (store (aref *pi-sl* nn) 1.0)))))
			       (setq nn (f- nn nz) n (f1- nn))
			       (do ((i 0 (f1+ i))) ((> i nn)) (store (aref *pr-sl* i) (aref *qpr-sl* i)))
			       (return nil)))
			(do ((i 0 (f1+ i))) ((> i n)) (store (aref *hr-sl* i) (aref *shi-sl* i))))
		    (or conv1 (return nil)))
		(cond ($polyfactor
		       (do ((i degree (f1- i)))
			   ((= i nn))
			   (cond ((= 0 (aref *pi-sl* i))
				  (store (aref *pr-sl* i) (_f (aref *pr-sl* i) polysc1)))
				 (t (store (aref *pr-sl* i) (_f (aref *pr-sl* i) (f* 2 polysc1)))
				    (setq i (f1- i))
				    (store (aref *pr-sl* i) (_f (aref *pr-sl* i) polysc1))))))
		      (t (do ((i (f1+ nn) (f1+ i)))
			     ((> i degree))
			     (store (aref *pr-sl* i) (_f (aref *pr-sl* i) polysc1))
			     (store (aref *pi-sl* i) (_f (aref *pi-sl* i) polysc1)))))
		(do ((i 0 (f1+ i)) (j (f- polysc (f* polysc1 degree)) (f+ j polysc1)))
		    ((> i nn))
		    (store (aref *pr-sl* i) (_f (aref *pr-sl* i) j))))
	#.(log 2.0d0) 
	most-positive-long-float least-positive-long-float
	(scale-float (into 1)(-(get-prec)))	;;double-float-epsilon
	(into 0) (/ 1 (sqrt (into 2))) (into 0)
	(into -0.069756474d0)(into 0.99756405d0)
	(into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) 0 0 0 0 0 t))

(defun fxshfr-sl (l2) 
       ((lambda (type a b c d e f g h a1 a3 a7) 
	 (setq nz 0)
	 (quadsd-sl )
	 (calcsc-sl )
	 (do ((j 1 (f1+ j)) (betav 0.25d0) (betas 0.25d0)
	      (oss sr) (ovv v) (tvv) (tss) (ss) (vv) (tv) (ts) (ots) (otv)
	      (ui) (vi) (s) (svv) (svu) (iflag) (vpass) (spass) (vtry) (stry))
	     ((> j l2))
	     (nextk-sl )
	     (calcsc-sl )
	     (newest-sl )
	     (setq vv vi ss (into 0))
	     (or (= 0 (aref *hr-sl* n)) (setq ss (-$ (//$ (aref *pr-sl* nn) (aref *hr-sl* n)))))
	     (setq tv 1.0d0 ts 1.0d0)
	     (cond ((not (or (= j 1) (= type 3)))
		    (or (= 0 vv) (setq tv (abs (//$ (-$ vv ovv) vv))))
		    (or (= 0 ss ) (setq ts (abs (//$ (-$ ss oss) ss))))
		    (setq tvv 1.0d0)
		    (and (< tv otv) (setq tvv (*$ tv otv)))
		    (setq tss 1.0d0)
		    (and (< ts ots) (setq tss (*$ ts ots)))
		    (setq vpass (< tvv betav) spass (< tss betas))
		    (cond ((or spass vpass)
			   (setq svu u svv v)
			   (do ((i 0 (f1+ i))) ((> i n)) (setf (aref *shr-sl* i) (aref *hr-sl* i)))
			   (setq s ss vtry nil stry nil)
			   (and (do ((bool (not (and spass
						     (or (not vpass) (< tss tvv))))
					   t)
				     (l50 nil nil))
				    (nil)
				    (cond (bool (quadit-sl )
						(and (> nz 0) (return t))
						(setq vtry t betav (*$ 0.25d0 betav))
						(cond ((or stry (not spass))
						       (setq l50 t))
						      (t (do ((i 0 (f1+ i)))
							     ((> i n))
							     (setf (aref *hr-sl* i)
								    (aref *shr-sl* i)))))))
				    (cond ((not l50)
					   (setq iflag (realit-sl ))
					   (and (> nz 0) (return t))
					   (setq stry t betas (*$ 0.25d0 betas))
					   (cond ((= 0 iflag) (setq l50 t))
						 (t (setq ui (-$ (+$ s s)) 
							  vi (*$ s s))))))
				    (cond (l50 (setq u svu v svv)
					       (do ((i 0 (f1+ i)))
						   ((> i n))
						   (setf (aref *hr-sl* i) (aref *shr-sl* i)))
					       (and (or (not vpass) vtry)
						    (return nil)))))
				(return nil))
			   (quadsd-sl )
			   (calcsc-sl )))))
	     (setq ovv vv oss ss otv tv ots ts)))
	0 (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0))) 

(defun quadit-sl nil 
       (setq nz 0 u ui v vi)
       (do ((tried) (j 0) (ee) (zm) (t1) (mp) (relstp) (omp))
	   (nil)
	   (quad-sl 1.0d0 u v)
	   (and (> (abs (-$ (abs szr) (abs lzr))) (*$ 0.01d0 (abs lzr))) (return nil))
	   (quadsd-sl )
	   (setq mp (+$ (abs (-$ a (*$ szr b))) (abs (*$ szi b))) 
		 zm (sqrt (abs v)) 
		 ee (*$ 2.0d0 (abs (aref *qpr-sl* 0))) 
		 t1 (-$ (*$ szr b)))
	   (do ((i 1 (f1+ n))) ((> i n)) (setq ee (+$ (*$ ee zm) (abs (aref *qpr-sl* i)))))
	   (setq ee (+$ (*$ ee zm) (abs (+$ a t1))) 
		 ee (-$ (*$ (+$ (*$ 5.0d0 mre) (*$ 4.0d0 are)) ee)
			(*$ (+$ (*$ 5.0d0 mre) (*$ 2.0d0 are))
			    (+$ (abs (+$ a t1)) (*$ (abs b) zm)))
			(*$ -2.0d0 are (abs t1))))
	   (cond ((not (> mp (*$ 20.0d0 ee))) (setq nz 2) (return nil)))
	   (setq j (f1+ j))
	   (and (> j 20) (return nil))
	   (cond ((not (or (< j 2) (> relstp 0.01d0) (< mp omp) tried))
		  (and (< relstp are) (setq relstp are))
		  (setq relstp (sqrt relstp) 
			u (-$ u (*$ u relstp)) 
			v (+$ v (*$ v relstp)))
		  (quadsd-sl )
		  (do ((i 1 (f1+ i))) ((> i 5)) (calcsc-sl ) (nextk-sl ))
		  (setq tried t j 0)))
	   (setq omp mp)
	   (calcsc-sl )
	   (nextk-sl )
	   (calcsc-sl )
	   (newest-sl )
	   (and (= 0 vi) (return nil))
	   (setq relstp (abs (//$ (-$ vi v) vi)) u ui v vi))) 

(defun realit-sl nil 
       (setq nz 0)
       (do ((j 0) (pv) (ee) (ms) (mp) (kv) (t1) (omp))
	   (nil)
	   (setq pv (aref *pr-sl* 0))
	   (setf (aref *qpr-sl* 0) pv)
	   (do ((i 1 (f1+ i)))
	       ((> i nn))
	       (setq pv (+$ (*$ pv s) (aref *pr-sl* i)))
	       (setf (aref *qpr-sl* i) pv))
	   (setq mp (abs pv) ms (abs s) ee (*$ (//$ mre (+$ are mre)) (abs (aref *qpr-sl* 0))))
	   (do ((i 1 (f1+ i))) ((> i nn)) (setq ee (+$ (*$ ee ms) (abs (aref *qpr-sl* i)))))
	   (cond ((not (> mp (*$ 20.0d0 (-$ (*$ (+$ are mre) ee) (*$ mre mp)))))
		  (setq nz 1 szr s szi (into 0))
		  (return 0)))
	   (setq j (f1+ j))
	   (and (> j 10) (return 0))
	   (cond ((not (or (< j 2)
			   (> (abs t1) (*$ 1.0d-3 (abs (-$ s t1))))
			   (not (> mp omp))))
		  (return 1)))
	   (setq omp mp kv (aref *hr-sl* 0))
	   (setf (aref *qhr-sl* 0) kv)
	   (do ((i 1 (f1+ i)))
	       ((> i n))
	       (setq kv (+$ (*$ kv s) (aref *hr-sl* i)))
	       (setf (aref *qhr-sl* i) kv))
	   (cond ((> (abs kv) (*$ (abs (aref *hr-sl* n)) 10.0d0 are))
		  (setq t1 (-$ (//$ pv kv)))
		  (setf (aref *hr-sl* 0) (aref *qpr-sl* 0))
		  (do ((i 1 (f1+ i)))
		      ((> i n))
		      (setf (aref *hr-sl* i) (+$ (*$ t1 (aref *qhr-sl* (f1- i))) (aref *qpr-sl* i)))))
		 (t (setf (aref *hr-sl* 0) (into 0))
		    (do ((i 1 (f1+ i))) ((> i n)) (setf (aref *hr-sl* i) (aref *qhr-sl* (f1- i))))))
	   (setq kv (aref *hr-sl* 0))
	   (do ((i 1 (f1+ i))) ((> i n)) (setq kv (+$ (*$ kv s) (aref *hr-sl* i))))
	   (setq t1 (into 0))
	   (and (> (abs kv) (*$ (abs (aref *hr-sl* n)) 10.0d0 are)) (setq t1 (-$ (//$ pv kv))))
	   (setq s (+$ s t1)))) 

(defun calcsc-sl nil 
       (setq d (aref *hr-sl* 0))
       (setf (aref *qhr-sl* 0) d)
       (setq c (-$ (aref *hr-sl* 1) (*$ u d)))
       (setf (aref *qhr-sl* 1) c)
       (do ((i 2 (f1+ i)) (c0))
	   ((> i n))
	   (setq c0 (-$ (aref *hr-sl* i) (*$ u c) (*$ v d)))
	   (setf (aref *qhr-sl* i) c0)
	   (setq d c c c0))
       (cond ((not (or (> (abs c) (*$ (abs (aref *hr-sl* n)) 100.0d0 are))
		       (> (abs d) (*$ (abs (aref *hr-sl* (f1- n))) 100.0d0 are))))
	      (setq type 3))
	     ((not (< (abs d) (abs c)))
	      (setq type 2 
		    e (//$ a d) 
		    f (//$ c d) 
		    g (*$ u b) 
		    h (*$ v b) 
		    a3 (+$ (*$ (+$ a g) e) (*$ h (//$ b d))) 
		    a1 (-$ (*$ b f) a) 
		    a7 (+$ (*$ (+$ f u) a) h)))
	     (t (setq type 1 
		      e (//$ a c) 
		      f (//$ d c) 
		      g (*$ u e) 
		      h (*$ v b) 
		      a3 (+$ (*$ a e) (*$ (+$ (//$ h c) g) b)) 
		      a1 (-$ b (*$ a (//$ d c))) 
		      a7 (+$ a (*$ g d) (*$ h f)))))
       nil) 

(defun nextk-sl nil 
       (cond ((= type 3)
	      (setf (aref *hr-sl* 0) (into 0))
	      (setf (aref *hr-sl* 1) (into 0))
	      (do ((i 2 (f1+ i))) ((> i n)) (setf (aref *hr-sl* i) (aref *qhr-sl* (f- i 2)))))
	     ((> (abs a1) (*$ (abs (cond ((= type 1) b) (a))) 10.0d0 are))
	      (setq a7 (//$ a7 a1) a3 (//$ a3 a1))
	      (setf (aref *hr-sl* 0) (aref *qpr-sl* 0))
	      (setf (aref *hr-sl* 1) (-$ (aref *qpr-sl* 1) (*$ a7 (aref *qpr-sl* 0))))
	      (do ((i 2 (f1+ i)))
		  ((> i n))
		  (setf (aref *hr-sl* i)
			 (+$ (*$ a3 (aref *qhr-sl* (f- i 2)))
			     (-$ (*$ a7 (aref *qpr-sl* (f1- i))))
			     (aref *qpr-sl* i)))))
	     (t (setf (aref *hr-sl* 0) (into 0))
		(setf (aref *hr-sl* 1) (-$ (*$ a7 (aref *qpr-sl* 0))))
		(do ((i 2 (f1+ i)))
		    ((> i n))
		    (setf (aref *hr-sl* i)
			   (-$ (*$ a3 (aref *qhr-sl* (f- i 2))) (*$ a7 (aref *qpr-sl* (f1- i))))))))
       nil) 

(defun newest-sl nil 
       ((lambda (a4 a5 b1 b2 c1 c2 c3 c4) 
		(cond ((= type 3) (setq ui (into 0) vi (into 0)))
		      (t (cond ((= type 2)
				(setq a4 (+$ (*$ (+$ a g) f) h) 
				      a5 (+$ (*$ (+$ f u) c) (*$ v d))))
			       (t (setq a4 (+$ a (*$ u b) (*$ h f)) 
					a5 (+$ c (*$ (+$ u (*$ v f)) d)))))
			 (setq b1 (-$ (//$ (aref *hr-sl* n) (aref *pr-sl* nn))) 
			       b2 (-$ (//$ (+$ (aref *hr-sl* (f1- n)) (*$ b1 (aref *pr-sl* n))) (aref *pr-sl* nn))) 
			       c1 (*$ v b2 a1) 
			       c2 (*$ b1 a7) 
			       c3 (*$ b1 b1 a3) 
			       c4 (-$ c1 c2 c3) 
			       c1 (+$ a5 (*$ b1 a4) (-$ c4)))
			 (cond ((= 0 c1) (setq ui (into 0) vi (into 0)))
			       (t (setq ui (-$ u
					       (//$ (+$ (*$ u (+$ c3 c2))
							(*$ v
							    (+$ (*$ b1 a1)
								(*$ b2 a7))))
						    c1)) 
					vi (*$ v (1+$ (//$ c4 c1))))))))
		nil)
	(into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0) (into 0))) 

(defun quadsd-sl nil 
       (setq b (aref *pr-sl* 0))
       (store (aref *qpr-sl* 0) b)
       (setq a (-$ (aref *pr-sl* 1) (*$ u b)))
       (store (aref *qpr-sl* 1) a)
       (do ((i 2 (f1+ i)) (c0))
	   ((> i nn))
	   (setq c0 (-$ (aref *pr-sl* i) (*$ u a) (*$ v b)))
	   (store (aref *qpr-sl* i) c0)
	 (setq b a a c0)))

(defun quad-sl (a0 b1 c0) 
  (declare (special szr szi lzr lzi))
       (setq szr (into 0) szi (into 0) lzr (into 0) lzi (into 0))
       ((lambda (b0 d0 e) 
		(cond ((= 0 a0 ) (or (= 0 b1 ) (setq szr (-$ (//$ c0 b1)))))
		      ((= 0 c0 ) (setq lzr (-$ (//$ b1 a0))))
		      (t (setq b0 (//$ b1 2.0d0)) ;; b/2
			 (cond ((< (abs b0) (abs c0))
				(setq e a0)
				(and (< c0 (into 0)) (setq e (-$ a0)))
				(setq e (-$ (*$ b0 (//$ b0 (abs c0))) e) 
				      d0 (*$ (sqrt (abs e)) (sqrt (abs c0)))))  ;sqrt|e|*sqrt|c0|
			       (t (setq e (-$ 1.0d0 (*$ (//$ a0 b0) (//$ c0 b0))) 
					d0 (*$ (sqrt (abs e)) (abs b0)))))
			 (cond ((< e #.(into 0))
				(setq szr (-$ (//$ b0 a0)) 
				      lzr szr 
				      szi (abs (//$ d0 a0)) 
				      lzi (-$ szi)))
			       (t (or (< b0 #.(into 0)) (setq d0 (-$ d0)))
				  (setq lzr (//$ (-$ d0 b0) a0))
				  (or (= 0 lzr ) (setq szr (//$ (//$ c0 lzr) a0)))))))
			nil)
	(into 0) (into 0) (into 0))) 

