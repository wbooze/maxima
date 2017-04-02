;;;; -*- Mode: lisp -*-
;;;;
;;;; Copyright (c) 2007 Raymond Toy
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

;;; This file contains various possible implementations of some of the
;;; core routines.  These were experiments on faster and/or more
;;; accurate implementations.  The routines inf qd-fun.lisp are the
;;; default, but you can select a different implementation from here
;;; if you want.
;;;
;;; The end of the file also includes some tests of the different
;;; implementations for speed.

(in-package #:octi)

;; This works but seems rather slow, so we don't even compile it.
#+(or)
(defun exp-qd/newton (a)
  (declare (type %quad-double a))
  ;; Newton iteration
  ;;
  ;; f(x) = log(x) - a
  ;;
  ;; x' = x - (log(x) - a)/(1/x)
  ;;    = x - x*(log(x) - a)
  ;;    = x*(1 + a - log(x))
  (let ((a1 (add-qd-d a 1d0))
	(x (make-qd-d (exp (qd-0 a)))))
    (setf x (mul-qd x (sub-qd a1 (log-qd/agm x))))
    (setf x (mul-qd x (sub-qd a1 (log-qd/agm x))))
    (setf x (mul-qd x (sub-qd a1 (log-qd/agm x))))
    x))

(defun expm1-qd/series (a)
  (declare (type %quad-double a))
  ;; Compute exp(x) - 1.
  ;;
  ;; D(x) = exp(x) - 1
  ;;
  ;; First, write x = s*log(2) + r*k where s is an integer and |r*k| <
  ;; log(2)/2.
  ;;
  ;; Then D(x) = D(s*log(2)+r*k) = 2^s*exp(r*k) - 1
  ;;           = 2^s*(exp(r*k)-1) - 1 + 2^s
  ;;           = 2^s*D(r*k)+2^s-1
  ;; But
  ;; exp(r*k) = exp(r)^k
  ;;          = (D(r) + 1)^k
  ;;
  ;; So
  ;; D(r*k) = (D(r) + 1)^k - 1
  ;;
  ;; For small r, D(r) can be computed using the Taylor series around
  ;; zero.  To compute D(r*k) = (D(r) + 1)^k - 1, we use the binomial
  ;; theorem to expand out the power and to exactly cancel out the -1
  ;; term, which is the source of inaccuracy.
  ;;
  ;; We want to have small r so the Taylor series converges quickly,
  ;; but that means k is large, which means the binomial expansion is
  ;; long.  We need to compromise.  Let use choose k = 8.  Then |r| <
  ;; log(2)/16 = 0.0433.  For this range, the Taylor series converges
  ;; to 212 bits of accuracy with about 28 terms.
  ;;
  ;;
  (flet ((taylor (x)
	   (declare (type %quad-double x))
	   ;; Taylor series for exp(x)-1
	   ;; = x+x^2/2!+x^3/3!+x^4/4!+...
	   ;; = x*(1+x/2!+x^2/3!+x^3/4!+...)
	   (let ((sum +qd-one+)
		 (term +qd-one+))
	     (dotimes (k 28)
	       (setf term (div-qd-d (mul-qd term x) (float (cl:+ k 2) 1d0)))
	       (setf sum (add-qd sum term)))
	     (mul-qd x sum)))
	 (binom (x)
	   (declare (type %quad-double x))
	   ;; (1+x)^8-1
	   ;; = x*(8 + 28*x + 56*x^2 + 70*x^3 + 56*x^4 + 28*x^5 + 8*x^6 + x^7)
	   ;; = x (x (x (x (x (x (x (x + 8) + 28) + 56) + 70) + 56) + 28) + 8)
	   (mul-qd
	    x
	    (add-qd-d
	     (mul-qd x
		     (add-qd-d
		      (mul-qd x
			      (add-qd-d
			       (mul-qd x
				       (add-qd-d
					(mul-qd x
						(add-qd-d
						 (mul-qd x
							 (add-qd-d
							  (mul-qd x
								  (add-qd-d x 8d0))
							  28d0))
						 56d0))
					70d0))
			       56d0))
		      28d0))
	     8d0)))
	 (arg-reduce (x)
	   (declare (type %quad-double x))
	   ;; Write x = s*log(2) + r*k where s is an integer and |r*k|
	   ;; < log(2)/2, and k = 8.
	   (let* ((s (truncate (qd-0 (nint-qd (div-qd a +qd-log2+)))))
		  (r*k (sub-qd x (mul-qd-d +qd-log2+ (float s 1d0))))
		  (r (div-qd-d r*k 8d0)))
	     (values s r))))
    (multiple-value-bind (s r)
	(arg-reduce a)
      (let* ((d (taylor r))
	     (dr (binom d)))
	(add-qd-d (scale-float-qd dr s)
		  (cl:- (scale-float 1d0 s) 1))))))
    
(defun log-qd/newton (a)
  (declare (type %quad-double a))
  ;; The Taylor series for log converges rather slowly.  Hence, this
  ;; routine tries to determine the root of the function
  ;;
  ;; f(x) = exp(x) - a
  ;;
  ;; using Newton iteration.  The iteration is
  ;;
  ;; x' = x - f(x) / f'(x)
  ;;    = x - (1 - a * exp(-x))
  ;;    = x + a * exp(-x) - 1
  ;;
  ;; Two iterations are needed.
  (let ((x (make-qd-d (log (qd-0 a)))))
    (dotimes (k 3)
      (setf x (sub-qd-d (add-qd x (mul-qd a (exp-qd (neg-qd x))))
			1d0)))
    x))


;;(declaim (inline agm-qd))

(defun agm-qd (x y)
  (declare (type %quad-double x y)
	   (optimize (speed 3)))
  (let ((diff (qd-0 (abs-qd (sub-qd x y)))))
    (cond ((< diff +qd-eps+)
	   x)
	  (t
	   (let ((a-mean (div-qd-d (add-qd x y) 2d0))
		 (g-mean (sqrt-qd (mul-qd x y))))
	     (agm-qd a-mean g-mean))))))

#+(or)
(defun agm-qd (x y)
  (declare (type %quad-double x y)
	   (optimize (speed 3) (space 0) (safety 0)))
  (let ((diff (qd-0 (abs-qd (sub-qd x y))))
	(x x)
	(y y))
    (declare (double-float diff))
    (loop while (> diff +qd-eps+)
      do
      (let ((a-mean (scale-float-qd (add-qd x y) -1))
	    (g-mean (sqrt-qd (mul-qd x y))))
	(setf x a-mean)
	(setf y g-mean)
	(setf diff (qd-0 (abs-qd (sub-qd x y))))))
    x))

(defun log-qd/agm (x)
  (declare (type %quad-double x))
  ;; log(x) ~ pi/2/agm(1,4/x)*(1+O(1/x^2))
  ;;
  ;; Need to make x >= 2^(d/2) to get d bits of precision.  We use
  ;;
  ;; log(2^k*x) = k*log(2)+log(x)
  ;;
  ;; to compute log(x).  log(2^k*x) is computed using AGM.
  ;;
  (multiple-value-bind (frac exp)
      (decode-float (qd-0 x))
    (declare (ignore frac))
    (cond ((>= exp 106)
	   ;; Big enough to use AGM
	   (div-qd +qd-pi/2+
		   (agm-qd +qd-one+
			   (div-qd (make-qd-d 4d0)
				   x))))
	  (t
	   ;; log(x) = log(2^k*x) - k * log(2)
	   (let* ((k (cl:- 107 exp))
		  (big-x (scale-float-qd x k)))
	     ;; Compute k*log(2) using extra precision by writing
	     ;; log(2) = a + b, where a is the quad-double
	     ;; approximation and b the rest.
	     (sub-qd (log-qd/agm big-x)
		     (add-qd (mul-qd-d +qd-log2+ (float k 1d0))
			     (mul-qd-d +qd-log2-extra+ (float k 1d0)))))))))

(defun log-qd/agm2 (x)
  (declare (type %quad-double x))
  ;; log(x) ~ pi/4/agm(theta2(q^4)^2,theta3(q^4)^2)
  ;;
  ;; where q = 1/x
  ;;
  ;; Need to make x >= 2^(d/36) to get d bits of precision.  We use
  ;;
  ;; log(2^k*x) = k*log(2)+log(x)
  ;;
  ;; to compute log(x).  log(2^k*x) is computed using AGM.
  ;;
  (multiple-value-bind (frac exp)
      (decode-float (qd-0 x))
    (declare (ignore frac))
    (cond ((>= exp 7)
	   ;; Big enough to use AGM (because d = 212 so x >= 2^5.8888)
	   (let* ((q (div-qd +qd-one+
			     x))
		  (q^4 (npow q 4))
		  (q^8 (sqr-qd q^4))
		  ;; theta2(q^4) = 2*q*(1+q^8+q^24)
		  ;;             = 2*q*(1+q^8+(q^8)^3)
		  (theta2 (mul-qd-d
			   (mul-qd
			    q
			    (add-qd-d
			     (add-qd q^8
				     (npow q^8 3))
			     1d0))
			   2d0))
		  ;; theta3(q^4) = 1+2*(q^4+q^16)
		  ;;             = 1+2*(q^4+(q^4)^4)
		  (theta3 (add-qd-d
			   (mul-qd-d
			    (add-qd q^4
				    (npow q^4 4))
			    2d0)
			   1d0)))
	     (div-qd +qd-pi/4+
		     (agm-qd (sqr-qd theta2)
			     (sqr-qd theta3)))))
	  (t
	   ;; log(x) = log(2^k*x) - k * log(2)
	   (let* ((k (cl:- 7 exp))
		  (big-x (scale-float-qd x k)))
	     (sub-qd (log-qd/agm2 big-x)
		     (add-qd (mul-qd-d +qd-log2+ (float k 1d0))
			     (mul-qd-d +qd-log2-extra+ (float k 1d0)))))))))

(defun log-qd/agm3 (x)
  (declare (type %quad-double x))
  ;; log(x) ~ pi/4/agm(theta2(q^4)^2,theta3(q^4)^2)
  ;;
  ;; where q = 1/x
  ;;
  ;; Need to make x >= 2^(d/36) to get d bits of precision.  We use
  ;;
  ;; log(2^k*x) = k*log(2)+log(x)
  ;;
  ;; to compute log(x).  log(2^k*x) is computed using AGM.
  ;;
  (multiple-value-bind (frac exp)
      (decode-float (qd-0 x))
    (declare (ignore frac))
    (cond ((>= exp 7)
	   ;; Big enough to use AGM (because d = 212 so x >= 2^5.8888)
	   (let* ((q (div-qd +qd-one+
			     x))
		  (q^4 (npow q 4))
		  (q^8 (sqr-qd q^4))
		  ;; theta2(q^4) = 2*q*(1+q^8+q^24)
		  ;;             = 2*q*(1+q^8+(q^8)^3)
		  (theta2 (mul-qd-d
			   (mul-qd
			    q
			    (add-qd-d
			     (add-qd q^8
				     (npow q^8 3))
			     1d0))
			   2d0))
		  ;; theta3(q^4) = 1+2*(q^4+q^16)
		  ;;             = 1+2*(q^4+(q^4)^4)
		  (theta3 (add-qd-d
			   (mul-qd-d
			    (add-qd q^4
				    (npow q^4 4))
			    2d0)
			   1d0)))
	     ;; Note that agm(theta2^2,theta3^2) = agm(2*theta2*theta3,theta2^2+theta3^2)/2
	     (div-qd +qd-pi/4+
		     (scale-float-qd
		      (agm-qd (scale-float-qd (mul-qd theta2 theta3) 1)
			      (add-qd (sqr-qd theta2)
				      (sqr-qd theta3)))
		      -1))))
	  (t
	   ;; log(x) = log(2^k*x) - k * log(2)
	   (let* ((k (cl:- 7 exp))
		  (big-x (scale-float-qd x k)))
	     (sub-qd (log-qd/agm3 big-x)
		     (add-qd
		      (mul-qd-d +qd-log2+ (float k 1d0))
		      (mul-qd-d +qd-log2-extra+ (float k 1d0)))))))))

#+(or)
(defun atan-d (y x)
  (let* ((r (abs (complex x y)))
	 (xx (cl:/ x r))
	 (yy (cl:/ y r)))
    (let ((z (atan (float y 1f0) (float x 1f0)))
	  (sinz 0d0)
	  (cosz 0d0))
      (format t "z = ~A~%" z)
      (cond ((> xx yy)
	     (format t "xx > yy~%")
	     (dotimes (k 5)
	       (let* ((sinz (sin z))
		      (cosz (cos z))
		      (delta (cl:/ (cl:- yy sinz)
				   cosz)))
		 (format t "sz, dz = ~A ~A~%" sinz cosz)
		 (format t "delta  = ~A~%" delta)
		 (setf z (cl:+ z delta))
		 (format t "z = ~A~%" z))))
	    (t
	     (dotimes (k 20)
	       (let ((sinz (sin z))
		     (cosz (cos z)))
		 (format t "sz, dz = ~A ~A~%" sinz cosz)
		 
		 (setf z (cl:- z (cl:/ (cl:- xx cosz)
				       sinz)))
		 (format t "z = ~A~%" z)))))
      z)))

#||
(defvar *table*)
(defvar *ttable*)
(defvar *cordic-scale*)

#+nil
(defun setup-cordic ()
  (let ((table (make-array 34))
	(ttable (make-array 34)))
    (setf (aref table 0) 1d0)
    (setf (aref table 1) 1d0)
    (setf (aref table 2) 1d0)
    (setf (aref ttable 0) (cl:/ pi 4))
    (setf (aref ttable 1) (cl:/ pi 4))
    (setf (aref ttable 2) (cl:/ pi 4))
    (loop for k from 3 below 34 do
	 (setf (aref table k) (cl:* 0.5d0 (aref table (cl:1- k))))
	 (setf (aref ttable k) (atan (aref table k))))
    (setf *table* table)
    (setf *ttable* ttable)))

(defun setup-cordic ()
  (let ((table (make-array 34))
	(ttable (make-array 34)))
    (setf (aref table 0) 4d0)
    (setf (aref table 1) 2d0)
    (setf (aref table 2) 1d0)
    (setf (aref ttable 0) (atan 4d0))
    (setf (aref ttable 1) (atan 2d0))
    (setf (aref ttable 2) (cl:/ pi 4))
    (loop for k from 3 below 34 do
	 (setf (aref table k) (cl:* 0.5d0 (aref table (cl:1- k))))
	 (setf (aref ttable k) (atan (aref table k))))
    (setf *table* table)
    (setf *ttable* ttable)))

(defun setup-cordic ()
  (let ((table (make-array 34))
	(ttable (make-array 34))
	(scale 1d0))
    (loop for k from 0 below 34 do
	 (setf (aref table k) (scale-float 1d0 (cl:- 2 k)))
	 (setf (aref ttable k) (atan (aref table k)))
	 (setf scale (cl:* scale (cos (aref ttable k)))))
    (setf *table* table)
    (setf *ttable* ttable)
    (setf *cordic-scale* scale)))


(defun cordic-rot (x y)
  (let ((z 0))
    (dotimes (k (length *table*))
      (cond ((plusp y)
	     (psetq x (cl:+ x (cl:* y (aref *table* k)))
		    y (cl:- y (cl:* x (aref *table* k))))
	     (incf z (aref *ttable* k)))
	    (t
	     (psetq x (cl:- x (cl:* y (aref *table* k)))
		    y (cl:+ y (cl:* x (aref *table* k))))
	     (decf z (aref *ttable* k)))
	    ))
    (values z x y)))

(defun cordic-vec (z)
  (let ((x 1d0)
	(y 0d0)
	(scale 1d0))
    (dotimes (k 12 (length *table*))
      (setf scale (cl:* scale (cos (aref *ttable* k))))
      (cond ((minusp z)
	     (psetq x (cl:+ x (cl:* y (aref *table* k)))
		    y (cl:- y (cl:* x (aref *table* k))))
	     (incf z (aref *ttable* k)))
	    (t
	     (psetq x (cl:- x (cl:* y (aref *table* k)))
		    y (cl:+ y (cl:* x (aref *table* k))))
	     (decf z (aref *ttable* k)))
	    ))
    (values x y z scale)))

(defun atan2-d (y x)
  (multiple-value-bind (z dx dy)
      (cordic-rot x y)
    (let ((theta (cl:/ dy dx)))
      (format t "theta = ~A~%" theta)
      (let ((corr (cl:+ theta
		     (cl:- (cl:/ (expt theta 3)
			   3))
		     (cl:/ (expt theta 5)
			5))))
	(format t "corr = ~A~%" corr)
	(cl:+ z corr)))))

(defun tan-d (r)
  (multiple-value-bind (x y z)
      (cordic-vec r)
    (setf x (cl:* x *cordic-scale*))
    (setf y (cl:* y *cordic-scale*))
    (format t "x = ~A~%" x)
    (format t "y = ~A~%" y)
    (format t "z = ~A~%" z)
    ;; Need to finish of the rotation
    (let ((st (sin z))
	  (ct (cos z)))
      (format t "st, ct = ~A ~A~%" st ct)
      (psetq x (cl:- (cl:* x ct) (cl:* y st))
	     y (cl:+ (cl:* y ct) (cl:* x st)))
      (format t "x = ~A~%" x)
      (format t "y = ~A~%" y)
      (cl:/ y x)
      )))

(defun sin-d (r)
  (declare (type double-float r))
  (multiple-value-bind (x y z s)
      (cordic-vec r)
    
    ;; Need to finish the rotation
    (let ((st (sin z))
	  (ct (cos z)))
      (psetq x (cl:- (cl:* x ct) (cl:* y st))
	     y (cl:+ (cl:* y ct) (cl:* x st)))
      (cl:* s y))))
||#

#||
;; Here is a function for clisp that can be used to create the atan2 table
;; that we need.

(defun make-atan-table-data ()
  (let ((scale 1l0))
    (dotimes (k 67)
      (let* ((x (scale-float 1L0 (- 2 k)))
	     (p (atan x)))
	(setf scale (* scale (cos p)))
	(multiple-value-bind (int exp sign)
	    (integer-decode-float p)
	  (let* ((len (integer-length int))
		 (wanted (ldb (byte 212 (- len 212)) int))
		 (bit (ldb (byte 1 (- len (* 4 53) 1)) int))
		 (roundp (not (zerop (ldb (byte (- len (* 4 53) 2) 0) int)))))
	    ;;(format t "~&~v,'0b~%" len int)
	    ;;(format t "~b~a~%" wanted (make-string (- len 212) :initial-element #\-))
	    ;;(format t "~v,'-b~%" len (ash bit (- len 212 1)))
	    ;;(format t "~v,'-b~%" len (ldb (byte (- len (* 4 53) 2) 0) int))
	    ;; See if we need to round up the answer.  
	    (when (= bit 1)
	      ;; Round to even
	      (cond (roundp
		     (incf wanted))
		    (t
		     ;; Round to even
		     (when (oddp wanted)
		       (incf wanted)))))
	    ;;(format t "~b~a~%" wanted (make-string (- len 212) :initial-element #\-))
	    
	    (let* ((i0 (ldb (byte 53 (* 3 53)) wanted))
		   (i1 (ldb (byte 53 (* 2 53)) wanted))
		   (i2 (ldb (byte 53 (* 1 53)) wanted))
		   (i3 (ldb (byte 53 0) wanted)))
	      (write `(make-qd-d
		       (scale-float (float ,i0 1d0) ,(+ exp (- len (* 1 53))))
		       (scale-float (float ,i1 1d0) ,(+ exp (- len (* 2 53))))
		       (scale-float (float ,i2 1d0) ,(+ exp (- len (* 3 53))))
		       (scale-float (float ,i3 1d0) ,(+ exp (- len (* 4 53)))))
		     :case :downcase))))))
    scale))
||#
	       
	
#+nil
(defconstant +atan-table+
  (make-array 66
	      :initial-contents
	      (list
	       +qd-pi/4+
	       +qd-pi/4+
	       +qd-pi/4+
	       ;; Do we need to make these values more accurate?  (The
	       ;; reader has quite a bit of roundoff.)
	       #.(qd-from-string "0.46364760900080611621425623146121440202853705428612026381093308872018q0") 
	       #.(qd-from-string "0.24497866312686415417208248121127581091414409838118406712737591466738q0")
	       #.(qd-from-string "0.12435499454676143503135484916387102557317019176980408991511411911572q0")
	       #.(qd-from-string "0.062418809995957348473979112985505113606273887797499194607527816898697q0")
	       #.(qd-from-string "0.031239833430268276253711744892490977032495663725400040255315586255793q0")
	       #.(qd-from-string "0.0156237286204768308028015212565703189111141398009054178814105073966645q0")
	       #.(qd-from-string "0.0078123410601011112964633918421992816212228117250147235574539022483893q0")
	       #.(qd-from-string "0.003906230131966971827628665311424387140357490115202856215213095149011q0")
	       #.(qd-from-string "0.00195312251647881868512148262507671393161074677723351033905753396043094q0")
	       #.(qd-from-string "9.7656218955931943040343019971729085163419701581008759004900725226773q-4")
	       #.(qd-from-string "4.8828121119489827546923962564484866619236113313500303710940335348752q-4")
	       #.(qd-from-string "2.4414062014936176401672294325965998621241779097061761180790046091019q-4") 
	       #.(qd-from-string "1.22070311893670204239058646117956300930829409015787498451939837846645q-4") 
	       #.(qd-from-string "6.1035156174208775021662569173829153785143536833346179337671134316588q-5") 
	       #.(qd-from-string "3.0517578115526096861825953438536019750949675119437837531021156883611q-5") 
	       #.(qd-from-string "1.5258789061315762107231935812697885137429238144575874846241186407446q-5") 
	       #.(qd-from-string "7.6293945311019702633884823401050905863507439184680771577638306965336q-6") 
	       #.(qd-from-string "3.8146972656064962829230756163729937228052573039688663101874392503939q-6") 
	       #.(qd-from-string "1.9073486328101870353653693059172441687143421654501533666700577234671q-6") 
	       #.(qd-from-string "9.53674316405960879420670689923112390019634124498790160133611802076q-7") 
	       #.(qd-from-string "4.7683715820308885992758382144924707587049404378664196740053215887142q-7") 
	       #.(qd-from-string "2.3841857910155798249094797721893269783096898769063155913766911372218q-7") 
	       #.(qd-from-string "1.19209289550780685311368497137922112645967587664586735576738225215437q-7") 
	       #.(qd-from-string "5.9604644775390554413921062141788874250030195782366297314294565710003q-8") 
	       #.(qd-from-string "2.9802322387695303676740132767709503349043907067445107249258477840843q-8") 
	       #.(qd-from-string "1.4901161193847655147092516595963247108248930025964720012170057805491q-8") 
	       #.(qd-from-string "7.4505805969238279871365645744953921132066925545665870075947601416172q-9") 
	       #.(qd-from-string "3.725290298461914045267070571811923583671948328737040524231998269239q-9") 
	       #.(qd-from-string "1.8626451492309570290958838214764904345065282835738863513491050124951q-9") 
	       #.(qd-from-string "9.3132257461547851535573547768456130389292649614929067394376854242196q-10") 
	       #.(qd-from-string "4.6566128730773925777884193471057016297347863891561617421323492554414q-10") 
	       #.(qd-from-string "2.32830643653869628902042741838821270371274293204981860525486662280605q-10") 
	       #.(qd-from-string "1.16415321826934814452599092729852658796396457380014290026584979170883q-10") 
	       #.(qd-from-string "5.8207660913467407226496761591231582349549156257795272423976206167147q-11") 
	       #.(qd-from-string "2.9103830456733703613273032698903947793693632003639830495829934525029q-11") 
	       #.(qd-from-string "1.4551915228366851806639597837362993474211703608936710732067270213307q-11") 
	       #.(qd-from-string "7.2759576141834259033201841046703741842764629388821429640111752890838q-12") 
	       #.(qd-from-string "3.6379788070917129516601402005837967730345578669779258118296083646486q-12") 
	       #.(qd-from-string "1.81898940354585647583007611882297459662931973336029253714520765350336q-12") 
	       #.(qd-from-string "9.094947017729282379150388117278718245786649666696631862264792881855q-13") 
	       #.(qd-from-string "4.5474735088646411895751949990348397807233312083369623012466392138249q-13") 
	       #.(qd-from-string "2.2737367544323205947875976170668549725904164010421166413578155299654q-13") 
	       #.(qd-from-string "1.1368683772161602973937988232271068715738020501302644662229139921281q-13") 
	       #.(qd-from-string "5.6843418860808014869689941345026335894672525626628305471702634435609q-14") 
	       #.(qd-from-string "2.8421709430404007434844970695472041986834065703328538172835210852389q-14") 
	       #.(qd-from-string "1.42108547152020037174224853506058802483542582129160672712566632799217q-14") 
	       #.(qd-from-string "7.1054273576010018587112426756616725310442822766145084088962160950957q-15") 
	       #.(qd-from-string "3.5527136788005009293556213378756778163805352845768135511116874239215q-15") 
	       #.(qd-from-string "1.7763568394002504646778106689434441020475669105721016938889503158663q-15") 
	       #.(qd-from-string "8.881784197001252323389053344724227002559458638215127117361184578544q-16") 
	       #.(qd-from-string "4.440892098500626161694526672362989312819932329776890889670147968684q-16") 
	       #.(qd-from-string "2.22044604925031308084726333618160413285249154122211136120876849284695q-16") 
	       #.(qd-from-string "1.11022302462515654042363166809081575098156144265276392015109606150467q-16") 
	       #.(qd-from-string "5.5511151231257827021181583404540958606019518033159549001888700768492q-17") 
	       #.(qd-from-string "2.7755575615628913510590791702270500685127439754144943625236087596052q-17") 
	       #.(qd-from-string "1.3877787807814456755295395851135253015328429969268117953154510949506q-17") 
	       #.(qd-from-string "6.9388939039072283776476979255676268417598037461585147441443138686883q-18") 
	       #.(qd-from-string "3.4694469519536141888238489627838134626418504682698143430180392335861q-18") 
	       #.(qd-from-string "1.7347234759768070944119244813919067365411688085337267928772549041983q-18") 
	       #.(qd-from-string "8.673617379884035472059622406959533689231148510667158491096568630248q-19") 
	       #.(qd-from-string "4.336808689942017736029811203479766845431237313833394811387071078781q-19") 
	       #.(qd-from-string "2.16840434497100886801490560173988342281757653922917435142338388484765q-19") 
	       #.(qd-from-string "1.08420217248550443400745280086994171142153300490364679392792298560597q-19") 

	       ))
  "Table of atan(2^(-k)) for k = 1 to 64.  But the first three entries are 1")

(defconstant +atan-table+
  (make-array 67
	      :initial-contents
	      (list
	       (%make-qd-d (scale-float (float 5970951936056572 1.0d0) -52)
			  (scale-float (float 5427585433121543 1.0d0) -105)
			  (scale-float (float 5608515294538868 1.0d0) -158)
			  (scale-float (float 445395631680583 1.0d0) -211))
	       (%make-qd-d (scale-float (float 4986154552901188 1.0d0) -52)
			  (scale-float (float 3814906810089799 1.0d0) -105)
			  (scale-float (float 1896417689773139 1.0d0) -158)
			  (scale-float (float 3393132800284032 1.0d0) -211))
	       (%make-qd-d (scale-float (float 7074237752028440 1.0d0) -53)
			  (scale-float (float 2483878800010755 1.0d0) -106)
			  (scale-float (float 3956492004828932 1.0d0) -159)
			  (scale-float (float 2434854662709436 1.0d0) -212))
	       (%make-qd-d (scale-float (float 8352332796509007 1.0d0) -54)
			  (scale-float (float 3683087214424816 1.0d0) -107)
			  (scale-float (float 8240297260223171 1.0d0) -160)
			  (scale-float (float 5174086704442609 1.0d0) -213))
	       (%make-qd-d (scale-float (float 8826286527774941 1.0d0) -55)
			  (scale-float (float 3471944699336670 1.0d0) -108)
			  (scale-float (float 4798212191802497 1.0d0) -161)
			  (scale-float (float 6908472993489831 1.0d0) -214))
	       (%make-qd-d (scale-float (float 8960721713639277 1.0d0) -56)
			  (scale-float (float 6978747913895162 1.0d0) -109)
			  (scale-float (float 1204496828771308 1.0d0) -162)
			  (scale-float (float 6150314016033077 1.0d0) -215))
	       (%make-qd-d (scale-float (float 8995498542038505 1.0d0) -57)
			  (scale-float (float 6996384121843768 1.0d0) -110)
			  (scale-float (float 6481245652257127 1.0d0) -163)
			  (scale-float (float 6083920726820778 1.0d0) -216))
	       (%make-qd-d (scale-float (float 9004268940523044 1.0d0) -58)
			  (scale-float (float 5921825575778154 1.0d0) -111)
			  (scale-float (float 1742767809528138 1.0d0) -164)
			  (scale-float (float 3392785816514584 1.0d0) -217))
	       (%make-qd-d (scale-float (float 9006466354344602 1.0d0) -59)
			  (scale-float (float 6455912199422039 1.0d0) -112)
			  (scale-float (float 7793493312778976 1.0d0) -165)
			  (scale-float (float 4748718880757240 1.0d0) -218))
	       (%make-qd-d (scale-float (float 9007016009513623 1.0d0) -60)
			  (scale-float (float 1583402193514233 1.0d0) -113)
			  (scale-float (float 4599960241393675 1.0d0) -166)
			  (scale-float (float 4964226307734805 1.0d0) -219))
	       (%make-qd-d (scale-float (float 9007153442175927 1.0d0) -61)
			  (scale-float (float 1458797116501429 1.0d0) -114)
			  (scale-float (float 2180379843517813 1.0d0) -167)
			  (scale-float (float 7244224576758923 1.0d0) -220))
	       (%make-qd-d (scale-float (float 9007187801521083 1.0d0) -62)
			  (scale-float (float 5961909987006481 1.0d0) -115)
			  (scale-float (float 1439161705865198 1.0d0) -168)
			  (scale-float (float 1250151122136839 1.0d0) -221))
	       (%make-qd-d (scale-float (float 9007196391431099 1.0d0) -63)
			  (scale-float (float 6595226783193595 1.0d0) -116)
			  (scale-float (float 7270788700276565 1.0d0) -169)
			  (scale-float (float 5212528258452836 1.0d0) -222))
	       (%make-qd-d (scale-float (float 9007198538913211 1.0d0) -64)
			  (scale-float (float 6605122380416172 1.0d0) -117)
			  (scale-float (float 2579496809882929 1.0d0) -170)
			  (scale-float (float 2545695100421145 1.0d0) -223))
	       (%make-qd-d (scale-float (float 9007199075784027 1.0d0) -65)
			  (scale-float (float 6605276999209814 1.0d0) -118)
			  (scale-float (float 8635423593413256 1.0d0) -171)
			  (scale-float (float 6747877897971029 1.0d0) -224))
	       (%make-qd-d (scale-float (float 9007199210001749 1.0d0) -66)
			  (scale-float (float 6605279415128805 1.0d0) -119)
			  (scale-float (float 5633073770825222 1.0d0) -172)
			  (scale-float (float 744251135568860 1.0d0) -225))
	       (%make-qd-d (scale-float (float 9007199243556181 1.0d0) -67)
			  (scale-float (float 3227579732349669 1.0d0) -120)
			  (scale-float (float 1645511649516378 1.0d0) -173)
			  (scale-float (float 7212311609477561 1.0d0) -226))
	       (%make-qd-d (scale-float (float 9007199251944789 1.0d0) -68)
			  (scale-float (float 3016473500406501 1.0d0) -121)
			  (scale-float (float 1629935234837168 1.0d0) -174)
			  (scale-float (float 1206159191623029 1.0d0) -227))
	       (%make-qd-d (scale-float (float 9007199254041941 1.0d0) -69)
			  (scale-float (float 3003279360882405 1.0d0) -122)
			  (scale-float (float 1629874389467187 1.0d0) -175)
			  (scale-float (float 8712158240272416 1.0d0) -228))
	       (%make-qd-d (scale-float (float 9007199254566229 1.0d0) -70)
			  (scale-float (float 3002454727161717 1.0d0) -123)
			  (scale-float (float 1629874151789961 1.0d0) -176)
			  (scale-float (float 3116377062563786 1.0d0) -229))
	       (%make-qd-d (scale-float (float 9007199254697301 1.0d0) -71)
			  (scale-float (float 3002403187554167 1.0d0) -124)
			  (scale-float (float 3881673964546782 1.0d0) -177)
			  (scale-float (float 6119176246102625 1.0d0) -230))
	       (%make-qd-d (scale-float (float 9007199254730069 1.0d0) -72)
			  (scale-float (float 3002399966328695 1.0d0) -125)
			  (scale-float (float 4198333313342644 1.0d0) -178)
			  (scale-float (float 114377133012236 1.0d0) -231))
	       (%make-qd-d (scale-float (float 9007199254738261 1.0d0) -73)
			  (scale-float (float 3002399765002103 1.0d0) -126)
			  (scale-float (float 4203281115667621 1.0d0) -179)
			  (scale-float (float 7620376512343991 1.0d0) -232))
	       (%make-qd-d (scale-float (float 9007199254740309 1.0d0) -74)
			  (scale-float (float 3002399752419191 1.0d0) -127)
			  (scale-float (float 4203358425078949 1.0d0) -180)
			  (scale-float (float 7121931241085909 1.0d0) -233))
	       (%make-qd-d (scale-float (float 9007199254740821 1.0d0) -75)
			  (scale-float (float 3002399751632759 1.0d0) -128)
			  (scale-float (float 4203359633038501 1.0d0) -181)
			  (scale-float (float 7119984189245056 1.0d0) -234))
	       (%make-qd-d (scale-float (float 9007199254740949 1.0d0) -76)
			  (scale-float (float 3002399751583607 1.0d0) -129)
			  (scale-float (float 4203359651912869 1.0d0) -182)
			  (scale-float (float 7119976583573803 1.0d0) -235))
	       (%make-qd-d (scale-float (float 9007199254740981 1.0d0) -77)
			  (scale-float (float 3002399751580535 1.0d0) -130)
			  (scale-float (float 4203359652207781 1.0d0) -183)
			  (scale-float (float 7119976553864150 1.0d0) -236))
	       (%make-qd-d (scale-float (float 9007199254740989 1.0d0) -78)
			  (scale-float (float 3002399751580343 1.0d0) -131)
			  (scale-float (float 4203359652212389 1.0d0) -184)
			  (scale-float (float 7119976553748096 1.0d0) -237))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -79)
			  (scale-float (float 3002399751580331 1.0d0) -132)
			  (scale-float (float 4203359652212461 1.0d0) -185)
			  (scale-float (float 7119976553747643 1.0d0) -238))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -80)
			  (scale-float (float 7505999378950826 1.0d0) -133)
			  (scale-float (float 6455159465897710 1.0d0) -186)
			  (scale-float (float 8245876460590265 1.0d0) -239))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -81)
			  (scale-float (float 8631899285793450 1.0d0) -134)
			  (scale-float (float 6032947000831726 1.0d0) -187)
			  (scale-float (float 8404206134990009 1.0d0) -240))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -82)
			  (scale-float (float 8913374262504106 1.0d0) -135)
			  (scale-float (float 6006558721765102 1.0d0) -188)
			  (scale-float (float 8406680036152505 1.0d0) -241))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -83)
			  (scale-float (float 8983743006681770 1.0d0) -136)
			  (scale-float (float 6004909454323438 1.0d0) -189)
			  (scale-float (float 8406718690858169 1.0d0) -242))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -84)
			  (scale-float (float 9001335192726186 1.0d0) -137)
			  (scale-float (float 6004806375108334 1.0d0) -190)
			  (scale-float (float 8406719294837945 1.0d0) -243))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -85)
			  (scale-float (float 9005733239237290 1.0d0) -138)
			  (scale-float (float 6004799932657390 1.0d0) -191)
			  (scale-float (float 8406719304275129 1.0d0) -244))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -86)
			  (scale-float (float 9006832750865066 1.0d0) -139)
			  (scale-float (float 6004799530004206 1.0d0) -192)
			  (scale-float (float 8406719304422585 1.0d0) -245))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -87)
			  (scale-float (float 9007107628772010 1.0d0) -140)
			  (scale-float (float 6004799504838382 1.0d0) -193)
			  (scale-float (float 8406719304424889 1.0d0) -246))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -88)
			  (scale-float (float 9007176348248746 1.0d0) -141)
			  (scale-float (float 6004799503265518 1.0d0) -194)
			  (scale-float (float 8406719304424925 1.0d0) -247))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -89)
			  (scale-float (float 9007193528117930 1.0d0) -142)
			  (scale-float (float 6004799503167214 1.0d0) -195)
			  (scale-float (float 8406719304424926 1.0d0) -248))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -90)
			  (scale-float (float 9007197823085226 1.0d0) -143)
			  (scale-float (float 6004799503161070 1.0d0) -196)
			  (scale-float (float 8406719304424926 1.0d0) -249))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -91)
			  (scale-float (float 9007198896827050 1.0d0) -144)
			  (scale-float (float 6004799503160686 1.0d0) -197)
			  (scale-float (float 8406719304424926 1.0d0) -250))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -92)
			  (scale-float (float 9007199165262506 1.0d0) -145)
			  (scale-float (float 6004799503160662 1.0d0) -198)
			  (scale-float (float 8406719304424926 1.0d0) -251))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -93)
			  (scale-float (float 9007199232371370 1.0d0) -146)
			  (scale-float (float 6004799503160661 1.0d0) -199)
			  (scale-float (float 3903119677054430 1.0d0) -252))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -94)
			  (scale-float (float 9007199249148586 1.0d0) -147)
			  (scale-float (float 6004799503160661 1.0d0) -200)
			  (scale-float (float 3058694746922462 1.0d0) -253))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -95)
			  (scale-float (float 9007199253342890 1.0d0) -148)
			  (scale-float (float 6004799503160661 1.0d0) -201)
			  (scale-float (float 3005918188789214 1.0d0) -254))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -96)
			  (scale-float (float 9007199254391466 1.0d0) -149)
			  (scale-float (float 6004799503160661 1.0d0) -202)
			  (scale-float (float 3002619653905886 1.0d0) -255))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -97)
			  (scale-float (float 9007199254653610 1.0d0) -150)
			  (scale-float (float 6004799503160661 1.0d0) -203)
			  (scale-float (float 3002413495475678 1.0d0) -256))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -98)
			  (scale-float (float 9007199254719146 1.0d0) -151)
			  (scale-float (float 6004799503160661 1.0d0) -204)
			  (scale-float (float 3002400610573790 1.0d0) -257))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -99)
			  (scale-float (float 9007199254735530 1.0d0) -152)
			  (scale-float (float 6004799503160661 1.0d0) -205)
			  (scale-float (float 3002399805267422 1.0d0) -258))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -100)
			  (scale-float (float 9007199254739626 1.0d0) -153)
			  (scale-float (float 6004799503160661 1.0d0) -206)
			  (scale-float (float 3002399754935774 1.0d0) -259))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -101)
			  (scale-float (float 9007199254740650 1.0d0) -154)
			  (scale-float (float 6004799503160661 1.0d0) -207)
			  (scale-float (float 3002399751790046 1.0d0) -260))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -102)
			  (scale-float (float 9007199254740906 1.0d0) -155)
			  (scale-float (float 6004799503160661 1.0d0) -208)
			  (scale-float (float 3002399751593438 1.0d0) -261))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -103)
			  (scale-float (float 9007199254740970 1.0d0) -156)
			  (scale-float (float 6004799503160661 1.0d0) -209)
			  (scale-float (float 3002399751581150 1.0d0) -262))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -104)
			  (scale-float (float 9007199254740986 1.0d0) -157)
			  (scale-float (float 6004799503160661 1.0d0) -210)
			  (scale-float (float 3002399751580382 1.0d0) -263))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -105)
			  (scale-float (float 9007199254740990 1.0d0) -158)
			  (scale-float (float 6004799503160661 1.0d0) -211)
			  (scale-float (float 3002399751580334 1.0d0) -264))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -106)
			  (scale-float (float 9007199254740991 1.0d0) -159)
			  (scale-float (float 6004799503160661 1.0d0) -212)
			  (scale-float (float 3002399751580331 1.0d0) -265))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -107)
			  (scale-float (float 9007199254740991 1.0d0) -160)
			  (scale-float (float 8256599316845909 1.0d0) -213)
			  (scale-float (float 3002399751580331 1.0d0) -266))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -108)
			  (scale-float (float 9007199254740991 1.0d0) -161)
			  (scale-float (float 8819549270267221 1.0d0) -214)
			  (scale-float (float 3002399751580331 1.0d0) -267))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -109)
			  (scale-float (float 9007199254740991 1.0d0) -162)
			  (scale-float (float 8960286758622549 1.0d0) -215)
			  (scale-float (float 3002399751580331 1.0d0) -268))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -110)
			  (scale-float (float 9007199254740991 1.0d0) -163)
			  (scale-float (float 8995471130711381 1.0d0) -216)
			  (scale-float (float 3002399751580331 1.0d0) -269))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -111)
			  (scale-float (float 9007199254740991 1.0d0) -164)
			  (scale-float (float 9004267223733589 1.0d0) -217)
			  (scale-float (float 3002399751580331 1.0d0) -270))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -112)
			  (scale-float (float 9007199254740991 1.0d0) -165)
			  (scale-float (float 9006466246989141 1.0d0) -218)
			  (scale-float (float 3002399751580331 1.0d0) -271))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -113)
			  (scale-float (float 9007199254740991 1.0d0) -166)
			  (scale-float (float 9007016002803029 1.0d0) -219)
			  (scale-float (float 3002399751580331 1.0d0) -272))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -114)
			  (scale-float (float 9007199254740991 1.0d0) -167)
			  (scale-float (float 9007153441756501 1.0d0) -220)
			  (scale-float (float 3002399751580331 1.0d0) -273))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -115)
			  (scale-float (float 9007199254740991 1.0d0) -168)
			  (scale-float (float 9007187801494869 1.0d0) -221)
			  (scale-float (float 3002399751580331 1.0d0) -274))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -116)
			  (scale-float (float 9007199254740991 1.0d0) -169)
			  (scale-float (float 9007196391429461 1.0d0) -222)
			  (scale-float (float 3002399751580331 1.0d0) -275))
	       (%make-qd-d (scale-float (float 9007199254740991 1.0d0) -117)
			  (scale-float (float 9007199254740991 1.0d0) -170)
			  (scale-float (float 9007198538913109 1.0d0) -223)
			  (scale-float (float 3002399751580331 1.0d0) -276))
	       ))
  "Table of atan(2^(-k)) for k = -2 to 64.  But the first three entries are 1")

(defconstant +atan-power-table+
  (make-array 67
	      :element-type 'double-float
	      :initial-contents
	       (loop for k from 0 below 67
		     collect (scale-float 1d0 (- 2 k)))
	       )
"Table of (2^(-k)) for k = -2 to 64.  But the first three entries are 1")

(defconstant +cordic-scale+
  #.(qd-from-string "0.065865828601599636584870082133151126045971796871364763285694473524426q0"))


;; This is the basic CORDIC rotation.  Based on code from
;; http://www.voidware.com/cordic.htm and
;; http://www.dspcsp.com/progs/cordic.c.txt.
;;
;; The only difference between this version and the typical CORDIC
;; implementation is that the first 3 rotations are all by pi/4.  This
;; makes sense.  If the angle is greater than pi/4, the rotations will
;; reduce it to at most pi/4.  If the angle is less than pi/4, the 3
;; rotations by pi/4 will cause us to end back at the same place.
;; (Should we try to be smarter?)
(defun cordic-rot-qd (x y)
  (declare (type %quad-double y x)
	   (optimize (speed 3)))
  (let* ((zero +qd-zero+)
	 (z zero))
    (declare (type %quad-double zero z))
    (dotimes (k (length +atan-table+))
      (declare (fixnum k))
      (cond ((qd-> y zero)
	     (psetq x (add-qd x (mul-qd-d y (aref +atan-power-table+ k)))
		    y (sub-qd y (mul-qd-d x (aref +atan-power-table+ k))))
	     (setf z (add-qd z (aref +atan-table+ k))))
	    (t
	     (psetq x (sub-qd x (mul-qd-d y (aref +atan-power-table+ k)))
		    y (add-qd y (mul-qd-d x (aref +atan-power-table+ k))))
	     (setf z (sub-qd z (aref +atan-table+ k))))))
    (values z x y)))

(defun atan2-qd/cordic (y x)
  (declare (type %quad-double y x))
  ;; Use the CORDIC rotation to get us to a small angle.  Then use the
  ;; Taylor series for atan to finish the computation.
  (multiple-value-bind (z dx dy)
      (cordic-rot-qd x y)
    ;; Use Taylor series to finish off the computation
    (let* ((arg (div-qd dy dx))
	   (sq (neg-qd (sqr-qd arg)))
	   (sum +qd-one+))
      ;; atan(x) = x - x^3/3 + x^5/5 - ...
      ;;         = x*(1-x^2/3+x^4/5-x^6/7+...)
      (do ((k 3d0 (cl:+ k 2d0))
	   (term sq))
	  ((< (abs (qd-0 term)) +qd-eps+))
	(setf sum (add-qd sum (div-qd-d term k)))
	(setf term (mul-qd term sq)))
      (setf sum (mul-qd arg sum))
      (add-qd z sum))))

(defun atan-qd/cordic (y)
  (declare (type %quad-double y))
  (atan2-qd/cordic y +qd-one+))

(defun atan-qd/duplication (y)
  (declare (type %quad-double y)
	   (optimize (speed 3) (space 0)))
  (cond ((< (abs (qd-0 y)) 1d-4)
	 ;; Series
	 (let* ((arg y)
		(sq (neg-qd (sqr-qd arg)))
		(sum +qd-one+))
	   ;; atan(x) = x - x^3/3 + x^5/5 - ...
	   ;;         = x*(1-x^2/3+x^4/5-x^6/7+...)
	   (do ((k 3d0 (cl:+ k 2d0))
		(term sq))
	       ((< (abs (qd-0 term)) +qd-eps+))
	     (setf sum (add-qd sum (div-qd-d term k)))
	     (setf term (mul-qd term sq)))
	   (mul-qd arg sum)))
	(t
	 ;; atan(x) = 2*atan(x/(1 + sqrt(1 + x^2)))
	 (let ((x (div-qd y
			  (add-qd-d (sqrt-qd (add-qd-d (sqr-qd y) 1d0))
				    1d0))))
	   (scale-float-qd (atan-qd/duplication x) 1)))))

(defun cordic-vec-qd (z)
  (declare (type %quad-double z)
	   (optimize (speed 3)))
  (let* ((x +qd-one+)
	 (y +qd-zero+)
	 (zero +qd-zero+))
    (declare (type %quad-double zero x y))
    (dotimes (k 30 (length +atan-table+))
      (declare (fixnum k)
	       (inline mul-qd-d sub-qd add-qd))
      (cond ((qd-> z zero)
	     (psetq x (sub-qd x (mul-qd-d y (aref +atan-power-table+ k)))
		    y (add-qd y (mul-qd-d x (aref +atan-power-table+ k))))
	     (setf z (sub-qd z (aref +atan-table+ k))))
	    (t
	     (psetq x (add-qd x (mul-qd-d y (aref +atan-power-table+ k)))
		    y (sub-qd y (mul-qd-d x (aref +atan-power-table+ k))))
	     (setf z (add-qd z (aref +atan-table+ k))))))
    (values z x y)))

(defun tan-qd/cordic (r)
  (declare (type %quad-double r))
  (multiple-value-bind (z x y)
      (cordic-vec-qd r)
    ;; Need to finish the rotation
    (multiple-value-bind (st ct)
	(sincos-taylor z)
      (psetq x (sub-qd (mul-qd x ct) (mul-qd y st))
	     y (add-qd (mul-qd y ct) (mul-qd x st)))
      (div-qd y x))))


(defun sin-qd/cordic (r)
  (declare (type %quad-double r))
  (multiple-value-bind (z x y)
      (cordic-vec-qd r)
    #+nil
    (progn
      (format t "~&x = ~/qd::qd-format/~%" x)
      (format t "~&y = ~/qd::qd-format/~%" y)
      (format t "~&z = ~/qd::qd-format/~%" z)
      (format t "~&s = ~/qd::qd-format/~%" s))
    ;; Need to finish the rotation
    (multiple-value-bind (st ct)
	(sincos-taylor z)
      #+nil
      (progn
	(format t "~&st = ~/qd::qd-format/~%" st)
	(format t "~&ct = ~/qd::qd-format/~%" ct)
	(format t "~&y  = ~/qd::qd-format/~%" (mul-qd +cordic-scale+ y)))

      (psetq x (sub-qd (mul-qd x ct) (mul-qd y st))
	     y (add-qd (mul-qd y ct) (mul-qd x st)))
      (mul-qd +cordic-scale+ y))))


;; Evaluate the polynomial poly at the point q.  The polynomial is
;; a list of the coefficients arranged in descending powers.
(defun poly-eval-qd (q poly)
  (let ((sum (%make-qd-d 0d0 0d0 0d0 0d0)))
    (dolist (c poly)
      (setf sum (add-qd (mul-qd q sum) c)))
    sum))

;; Like poly-eval-qd, except the polynomial is a list of double-floats
(defun poly-eval-qd-d (q poly)
  (let ((sum (%make-qd-d 0d0 0d0 0d0 0d0)))
    (dolist (c poly)
      (setf sum (add-qd-d (mul-qd q sum) c)))
    sum))


;; This idea is from Richard Fateman.  I've added a few additional
;; notes, but these are mostly from Richard.
;;
;; We can evaluate exp(x) for |x|< 0.01, say, by a pade approximation,
;; computed using Maxima:
;;
;; taylor(exp(x), x, 0, 20)$
;;
;; pade(%, 10, 10) ->
;;
;; [(x^10+110*x^9+5940*x^8+205920*x^7+5045040*x^6+90810720*x^5+1210809600*x^4
;;  +11762150400*x^3+79394515200*x^2+335221286400*x^+670442572800)
;;  /(x^10-110*x^9+
;;  5940*x^8-205920*x^7+5045040*x^6-90810720*x^5+1210809600*x^4-11762150400*x^3+
;;  79394515200*x^2-335221286400*x^+670442572800)]
;;
;; The numerator and denominator have the same coefficients with
;; different signs on odd terms.  so we note that num and den here can
;; be evaluated as f(x^2)+x*g(x^2) and f(x^2)-x*g(x^2) respectively using
;; half the number of multiplies.

(defconstant f-exp-pade
  '(1.0d0 5940.0d0 5045040.0d0  1.2108096d+9
    7.93945152d+10 6.704425728d+11))

(defconstant g-exp-pade
  '(110.0d0  205920.0d0  9.081072d+7 
    1.17621504d+10  3.352212864d+11 ))

(defun exp-qd/pade-small (x)
  ;; exp(x) = (f(x^2) + x*g(x^2))/(f(x^2) - x*g(x^2))
  (let* ((x^2 (sqr-qd x))
	 (fx (poly-eval-qd-d x^2 f-exp-pade))
	 (x*gx (mul-qd x (poly-eval-qd-d x^2 g-exp-pade))))
    (div-qd (add-qd fx x*gx)
	    (sub-qd fx x*gx))))

(defun exp-qd/pade (a)
  ;; Same idea as in exp-qd/reduce, except we use our Pade
  ;; approximation instead of a Taylor series.
  (let* ((k 256)
	 (z (truncate (qd-0 (nint-qd (div-qd a +qd-log2+)))))
	 (r (div-qd-d (sub-qd a (mul-qd-d +qd-log2+ (float z 1d0)))
		      (float k 1d0)))
  
	 (e (exp-qd/pade-small r)))
    (scale-float-qd (npow e k) z)))


;; Some timing and consing tests.
;;
;; The tests are run using the following:
;;
;; Sparc:	1.5 GHz Ultrasparc IIIi
;; Sparc2:	450 MHz Ultrasparc II
;; PPC:		1.42 GHz
;; x86:		866 MHz Pentium 3
;; PPC(fma):	1.42 GHz with cmucl with fused-multiply-add double-double.
;;

;; (time-exp #c(2w0 0) 50000)
;;
;; Time			Sparc	PPC	x86	PPC (fma)	Sparc2
;; exp-qd/reduce	2.06	 3.18	10.46	2.76		 6.12
;; expm1-qd/series	8.81	12.24	18.87	3.26		29.0
;; expm1-qd/dup		5.68	 4.34	18.47	3.64		 9.77
;; exp-qd/pade          1.53                                     4.51
;;
;; Consing (MB)		Sparc
;; exp-qd/reduce	 45   	 45   	 638   	44.4   		 45
;; expm1-qd/series	519   	519   	1201  	14.8   		519
;; expm1-qd/dup		 32   	 32   	1224   	32.0   		 32
;; exp-qd/pade           44                                      44
;;
;; Speeds seem to vary quite a bit between architectures.
;;
;; Timing without inlining all the basic functions everywhere.  (That
;; is, :qd-inline is not a feature.)
;;
;; (time-exp #c(2w0 0) 50000)
;;
;; Time			Sparc	PPC	x86	PPC (fma)
;; exp-qd/reduce	 5.83	0.67	10.67	0.98
;; expm1-qd/series	10.65	1.45	21.06	1.35
;; expm1-qd/dup		11.17	1.36	24.01	1.25
;;
;; Consing		Sparc
;; exp-qd/reduce	 638   	 93	 638	 93
;; expm1-qd/series	1203   	120	1201	120
;; expm1-qd/dup		1224   	122	1224	122
;;
;; So inlining speeds things up by a factor of about 3 for sparc,
;; 1.5-4 for ppc.  Strangely, x86 slows down on some but speeds up on
;; others.
(defun time-exp (x n)
  (declare (type %quad-double x)
	   (fixnum n))
  (let ((y +qd-zero+))
    (declare (type %quad-double y))
    #+cmu (ext:gc :full t)
    (format t "exp-qd/reduce~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (exp-qd/reduce x))))
    #+cmu (ext:gc :full t)
    (format t "exp-qd/pade~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (exp-qd/pade x))))
    #+cmu (ext:gc :full t)
    (format t "expm1-qd/series~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (expm1-qd/series x))))
    #+cmu (ext:gc :full t)
    (format t "expm1-qd/duplication~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (expm1-qd/duplication x))))
  
    ))

#||

;; This bit of code is meant to be run with Clisp, which has arbitrary
;; precision long floats.
;;
;; We use this to compare the accuracy of exp-qd/reduce and
;; exp-qd/pade against Clisp's exp implementation.
;;
;; Some results:
;;
;; k = 159000
;; pade min   =       4.733L-68
;; pade max   =       5.153L-61
;; pade sumsq =      7.899L-119
;; red min    =       3.838L-69
;; red max    =       4.318L-62
;; red sumsq  =      6.076L-120
;;
;; So, worst case error is:
;;
;; exp-qd/reduce: 4.318L-62 (203.8 bits)
;; exp-qd/pade:   5.153L-61 (200.3 bits)
;;
;; We lose 3 bits for Pade.  exp-qd/reduce is "missing" 8 bits of
;; accuracy.  Bummer.

(in-package #:oct)

;; 240 bit fractions for long-floats.  I hope this is accurate enough
;; compared against our 212 bit fractions for quad-doubles.
(setf (ext:long-float-digits) 240)
(defun compare-exp (n &optional verbose)
  (let ((pade-max -1L0)
	(pade-min 1L100)
	(pade-sum 0L0)
	(red-max -1L0)
	(red-min 1L100)
	(red-sum 0L0))
    (dotimes (k n)
      (let* ((f (random (make-qd 1/256)))
	     (pade-exp (octi::exp-qd/pade (qd-value f)))
	     (red-exp (octi::exp-qd/reduce (qd-value f))))
	(flet ((qd-to-lf (qd)
		 (octi:with-qd-parts (q0 q1 q2 q3)
		     qd
		   (+ (float q0 1L0)
		      (float q1 1L0)
		      (float q2 1L0)
		      (float q3 1L0)))))
	  (let* ((lf (qd-to-lf (qd-value f)))
		 (l-exp (exp lf))
		 (pade-exp-lf (qd-to-lf pade-exp))
		 (red-exp-lf (qd-to-lf red-exp))
		 (pade-diff (abs (- l-exp pade-exp-lf)))
		 (red-diff (abs (- l-exp red-exp-lf))))
	    (when verbose
	      (format t "f     = ~S~%" f)
	      (format t "pade-exp = ~A~%" (make-instance 'qd-real :value pade-exp))
	      (format t "l-exp =   ~A~%" l-exp)
	      (format t "diff  = ~A~%" pade-diff))
	    (when (and (plusp k) (zerop (mod k 1000)))
	      (format t "k = ~A~%" k)
	      (format t "pade min   = ~15,3g~%" pade-min)
	      (format t "pade max   = ~15,3g~%" pade-max)
	      (format t "pade sumsq = ~15,3g~%" pade-sum)
	      (format t "red min    = ~15,3g~%" red-min)
	      (format t "red max    = ~15,3g~%" red-max)
	      (format t "red sumsq  = ~15,3g~%" red-sum))
	    (setf pade-max (max pade-max pade-diff))
	    (setf pade-min (min pade-min pade-diff))
	    (incf pade-sum (* pade-diff pade-diff))
	    (setf red-max (max red-max red-diff))
	    (setf red-min (min red-min red-diff))
	    (incf red-sum (* red-diff red-diff)))
	  )))
  (values pade-max pade-min pade-sum
	  red-max red-min red-sum
	  )))
	  
||#

;; (time-log #c(3w0 0) 50000)
;;
;; Time (s)		Sparc	PPC	x86	PPC (fma)	Sparc2
;; log-qd/newton	7.08	10.23	35.74	8.82		21.77
;; log1p-qd/dup		5.87	 8.41	27.32	6.65		20.73
;; log-qd/agm		6.58	 8.0	27.2	6.87		24.62
;; log-qd/agm2		5.8	 6.93	22.89	6.07		18.44
;; log-qd/agm3		5.45	 6.57	20.97	6.18		20.34
;; log-qd/halley	4.96	 6.8	25.11	7.01		16.13
;;
;; Consing (MB)		Sparc	PPC	x86	PPC (fma)
;; log-qd/newton	150   	150   	2194   	148   		150
;; log1p-qd/dup		 56   	 56   	1564   	 56   		 56
;; log-qd/agm		 81   	 11	1434   	 81		 81
;; log-qd/agm2		 87   	 35   	1184   	 87		 87
;; log-qd/agm3		 82   	 36   	1091   	 81   		 82
;; log-qd/halley	101   	101   	1568   	100		101
;;
;; Based on these results, it's not really clear what is the fastest.
;; But Halley's iteration is probably a good tradeoff for log.
;;
;; However, consider log(1+2^(-100)).  Use log1p as a reference:
;;  7.88860905221011805411728565282475078909313378023665801567590088088481830649115711502410110281q-31
;;
;; We have
;; log-qd
;;  7.88860905221011805411728565282475078909313378023665801567590088088481830649133878797727478488q-31
;; log-agm
;;  7.88860905221011805411728565282514580471135738786455290255431302193794546609432q-31
;; log-agm2
;;  7.88860905221011805411728565282474926980229445866885841995713611460718519856111q-31
;; log-agm3
;;  7.88860905221011805411728565282474926980229445866885841995713611460718519856111q-31
;; log-halley
;;  7.88860905221011805411728565282475078909313378023665801567590088088481830649120253326239452326q-31
;;
;; We can see that the AGM methods are grossly inaccurate, but log-qd
;; and log-halley are quite good.
;;
;; Timing results without inlining everything:
;;
;; Time			Sparc	PPC	x86	PPC (fma)
;; log-qd/newton	21.37	0.87	41.49	0.62
;; log1p-qd/dup		12.58	0.41	31.86	0.28
;; log-qd/agm		 7.17	0.23	34.86	0.16
;; log-qd/agm2		 6.35	0.22	27.53	0.15
;; log-qd/agm3		 7.49	0.17	24.92	0.14
;; log-qd/halley	14.38	0.56	30.2	0.65
;;
;; Consing
;;			Sparc	PPC	x86	PPC (fma)
;; log-qd/newton	2194   	60.7	2194	61
;; log1p-qd/dup		1114   	22.6	1564	23
;; log-qd/agm		 371   	 7.9	1434	 7.9
;; log-qd/agm2		 371   	 7.8	1185	 7.8
;; log-qd/agm3		 373   	 7.8	1091	 7.8
;; log-qd/halley	1554   	42.3	1567	42.3

(defun time-log (x n)
  (declare (type %quad-double x)
	   (fixnum n))
  (let ((y +qd-zero+))
    (declare (type %quad-double y))
    #+cmu (ext:gc :full t)
    (format t "log-qd/newton~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (log-qd/newton x))))
    #+cmu (ext:gc :full t)
    (format t "log1p-qd/duplication~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (log1p-qd/duplication x))))
    #+cmu (ext:gc :full t)
    (format t "log-qd/agm~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (log-qd/agm x))))
  
    #+cmu (ext:gc :full t)
    (format t "log-qd/agm2~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (log-qd/agm2 x))))
    #+cmu (ext:gc :full t)
    (format t "log-qd/agm3~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (log-qd/agm3 x))))
    #+cmu (ext:gc :full t)
    (format t "log-qd/halley~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (log-qd/halley x))))
    ))
  

;; (time-atan2 #c(10w0 0) 10000)
;;
;; Time
;;			PPC	Sparc	x86	PPC (fma)	Sparc2
;; atan2-qd/newton     	2.91	 1.91	 8.06	2.16		7.55
;; atan2-qd/cordic	1.22	 0.89	 6.68	1.43		2.47
;; atan-qd/duplication	2.51	 2.14	 5.63	1.76		5.94
;;
;; Consing
;; atan2-qd/newton     	44.4   	44.4   	481   	44.4   		44.4
;; atan2-qd/cordic	 1.6   	 1.6   	482   	 1.6   		 1.6
;; atan-qd/duplication	17.2   	 6.0   	281   	 6.0		 6.0
;;
;; Don't know why x86 is 10 times slower than sparc/ppc for
;; atan2-qd/newton.  Consing is much more too.  Not enough registers?
;;
;; atan2-qd/cordic is by far the fastest on all archs.
;;
;; Timing results without inlining everything:
;; Time
;;			PPC	Sparc	x86	PPC (fma)
;; atan2-qd/newton     	6.56	 4.48	9.75	6.15
;; atan2-qd/cordic	6.02	 4.24	7.06	5.01
;; atan-qd/duplication	3.28	 1.94	5.72	2.46
;;
;; Consing
;; atan2-qd/newton     	443	441   	482	443
;; atan2-qd/cordic	482	482   	482	482
;; atan-qd/duplication	 87	 81   	281	87
;;

(defun time-atan2 (x n)
  (declare (type %quad-double x)
	   (fixnum n))
  (let ((y +qd-zero+)
	(one +qd-one+))
    #+cmu (ext:gc :full t)
    (format t "atan2-qd/newton~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (atan2-qd/newton x one))))
    #+cmu (ext:gc :full t)
    (format t "atan2-qd/cordic~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (atan2-qd/cordic x one))))
    #+cmu (ext:gc :full t)
    (format t "atan-qd/duplication~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (atan-qd/duplication x))))
    ))
	  
;; (time-tan #c(10w0 0) 10000)
;;
;; Time
;;			PPC	Sparc	x86	PPC (fma)	Sparc2
;; tan-qd/cordic     	2.12	 1.51	 8.26	1.77		4.61
;; tan-qd/sincos	0.68	 0.57	 2.39	0.54		2.56
;;
;; Consing
;; tan-qd/cordic     	23.0   	23.0   	473   	23.0		23.0
;; tan-qd/sincos	14.8   	14.8   	147   	14.8		14.8
;;
;; Don't know why x86 is so much slower for tan-qd/cordic.
;;
;; Without inlining everything
;;			PPC	Sparc	x86	PPC (fma)
;; tan-qd/cordic     	7.72	4.56	17.08	5.96
;; tan-qd/sincos	2.32	1.4	 4.91	1.87
;;
;; Consing
;; tan-qd/cordic     	463	463	472	463
;; tan-qd/sincos	137	136	146	137

(defun time-tan (x n)
  (declare (type %quad-double x)
	   (fixnum n))
  (let ((y +qd-zero+))
    #+cmu (ext:gc :full t)
    (format t "tan-qd/cordic~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (tan-qd/cordic x))))
    #+cmu (ext:gc :full t)
    (format t "tan-qd/sincos~%")
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf y (tan-qd/sincos x))))))
    
(defun dump-qd (qd)
  (flet ((dump-d (d)
	   (multiple-value-bind (int exp sign)
	       (integer-decode-float d)
	     `(scale-float (float ,(* sign int) 1d0) ,exp))))
  (multiple-value-bind (q0 q1 q2 q3)
      (qd-parts qd)
    `(%make-qd-d ,(dump-d q0)
		 ,(dump-d q1)
		 ,(dump-d q2)
		 ,(dump-d q3)))))

