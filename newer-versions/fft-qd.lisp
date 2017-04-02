;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: qd; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;FFT!!!;;;;;;;;;;;;;;;;;

;; this is part of qd.lisp now.

(in-package :qd)
(eval-when (compile)
  (load "ga.fasl")
    (load "qd.fasl"))

(eval-when (compile load eval)
; Fourier Transform Spectral Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;Routines translated with permission by Kevin A. Broughan from ;;;;;;;;;;;
;;Numerical Recipies in Fortran Copyright (c) Numerical Recipies 1986, 1989;;;;
;;;;;;;;;;;;;;;Modified by Ken Olum for Common Lisp, April 1996;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; notes 4/27/05, RJF.  
;; more notes, 2/28/06 RJF. Converting h:/lisp/fft.lisp to QD arithmetic.
;; see comments at fft.lisp.
;; We use four1 only
;; the input data will be over-written by the result.
;; the inverse fft is indicated by :isign -1, though it must be
;; multiplied by  1/nn to work.
; functions:
;	four1: fourier transform (FFT) in one dimension

(defun v2dfa(a &optional (m (length a))	)
  ;;coerce a vector of QD numbers of length m to a QD array of length
  ;; 2m, since here complex numbers are stored in 2 adjacent QD
  ;; locations.
  (let* ((k (length a))
	 (ans (make-array (cl::* 2 m) ))
	 (zz (qd::into 0)))
    (dotimes (i k)			;just copy the actual numbers, or convert?
      (setf (aref ans (cl::* 2 i))(qd::into (aref a i))) ;; here we convert.
      (setf (aref ans (cl::1+ (cl::* 2 i))) (copy zz)))
    (loop for i from (* 2 k) to (1-(* 2 m)) do (setf  (aref ans i) (copy zz)))
    ans))

;; (v2dfa #(30 40 50) 4)
;;  --> #(0.3Q2 0.Q0 0.4Q2 0.Q0 0.5Q2 0.Q0 0.Q0 0.Q0)

(defparameter *zz* (qd::into 0))
(defparameter *one* (qd::into 1))

(defun dfa2v(a &optional (m (/ (length a)2)))
  ;; Coerce real parts back to integers, more or less.  a is an an
  ;; array of even length.  If you know that there are trailing zeros,
  ;; set the actual length with the optional second parameter m.
  (let* ((k (/ (length a) 2))
	 (ans (make-array m)))
    (dotimes (i m ans)
      (setf (aref ans i)(round (ga::outof (aref a (* 2 i))) k)))))

;;(dfa2v  (v2dfa #(256 256 256) 4))
;; -->  #(64 64 64 0)  is correct.

;; this works!
(defun polymultfftqd(r s)
  ;;compute the size Z of the answer. Z is a power of 2.
  ;; compute complex array r, increased to size S
  ;; compute complex array r, increased to size S
  ;; compute two FFTs
  ;; multiply pointwise
  ;; compute inverse FFT  * 1/n
  ;; convert back to array and return answer
  (let* ((lr (length r))
	 (ls (length s))
	 (lans (+ lr ls -1))
	 (z (ash 1 (ceiling (log lans 2)))) ; round up to power of 2
	 (rfft (four1 (v2dfa r z) z))
	 (sfft (four1 (v2dfa s z) z))
	 (prod (prodarray rfft sfft z ))) 
    (dfa2v(four1 prod z :isign -1) lans)))

;; Utility to copy an array because this fft will clobber input.
;; We don't use it here, but here's a way to write it.
;; (defun copyarray (a)(make-array (length a):initial-contents a))

(defun prodarray(r s len)
  ;; r and s are the same length arrays
  ;; compute, for i=0, 2, ..., len-2
  ;; ans[i]:=  r[i]*s[i]-r[i+1]*s[i+1] ;; real part
  ;; ans[i+1]:=r[i]*s[i+1]+s[i]*r[i+1] ;; imag part
  (let ((ans (make-array (* 2 len))))
  (dotimes (i len ans)
    (let* ((ind (* 2 i))
	   (ind1 (1+ ind))
	   (a (aref r ind))
	   (b (aref r ind1))
	   (c (aref s ind))
	   (d (aref s ind1)))
      (declare ;(type aqd a b c d)
	       (fixnum ind ind1))
      (setf (aref ans ind)(copy(with-temps (- (* a c)(* b d)))))
      (setf (aref ans ind1)(copy(with-temps (+ (* a d)(* b c)))))))))

(defparameter qdtwopi
    (ga::/ (qd::into  165424160919322423196824703508232170249081435635340508251270944637 )
     (qd::into  26328072917139296674479506920917608079723773850137277813577744384))
  )
(defparameter onehalf (/ (qd::into 1) (qd::into 2)))


;;0.62831853071795864769252867665590057683943387987502116419498891846Q1

;;; this is a fairly generic FFT that works, but not optimized much.
#+ignore
(defun four1 (data nn &key (isign 1))

 ;;
  (declare (type fixnum nn isign))
 (prog ((wr (copy *zz*)) 
	(wi (copy *zz*)) 
	(wpr (copy *zz*))
	(wpi (copy *zz*))
	(wtemp (copy *zz*)) 
        (theta (copy *zz*)) 
	(tempr (copy *zz*)) 
	(tempi (copy *zz*))
	(j 0) (n 0) (m 0) (mmax 0) (istep 0))
   (declare
	;;(type aqd wr wi wpr wpi wtemp theta tempr tempi)
    (fixnum j n m mmax istep))
   
  (setf n (* 2 nn)) 
  (setf j 1) 
  (do ((i 1 (+ i 2)))
      ((> i n) t)
      (declare (fixnum i))
    (when (> j i) 
     (setf tempr (aref data (1- j)))
     (setf tempi (aref data j)) 
     (setf (aref data (1- j)) (aref data (1- i)))
     (setf (aref data j) (aref data i)) 
     (setf (aref data (1- i)) tempr)
     (setf (aref data i) tempi))
    (setf m (floor (/ n 2)))
 label1
    (when (and (>= m 2) (> j m))
     (setf j (- j m)) (setf m (floor (/ m 2)))
     (go label1))
    (setf j (+ j m))) 
  (setf mmax 2) 
 label2 
  (when (> n mmax)
    (setf istep (cl::* 2 mmax))
    (setf theta  (/ qdtwopi (* isign mmax)))
    (setf wpr  (identity(* -2 (expt (sin (* 1/2 theta)) 2))))
    (setf wpi (sin theta)) (setf wr (copy *one*)) (setf wi (copy *zz*))
    (do ((m 1 (+ m 2)))
	((cl::> m mmax) t)
      (declare (fixnum  m))
      (do ((i m (+ i istep)))
	  ((> i n) t)
	(declare (type fixnum i))
	(setf j (+ i mmax))
	(setf tempr (identity (- (* wr (aref data (1- j)))
				    (* wi (aref data j)))))
	(setf tempi (identity (+ (* wr (aref data j))
				    (* wi (aref data (1- j))))))
	(setf (aref data (1- j)) (identity (- (aref data (1- i)) tempr)))
	(setf (aref data j) (identity (- (aref data i) tempi)))
	(setf (aref data (1- i)) (identity (+ (aref data (1- i)) tempr)))
	(setf (aref data i) (identity (+ (aref data i) tempi))))
      (setf wtemp wr)
      (setf wr (identity (+ (+ (* wr wpr) (* (* -1 wi) wpi)) wr)))
      (setf wi (identity(+  (+ (* wi wpr) (* wtemp wpi)) wi))))
    (setf mmax istep)
    (go label2)) 
   (return data)))

;; hacking away at it to make it faster for QD....
(defun four1 (data nn &key (isign 1))

  (declare (type fixnum nn isign))
  (prog ((wr (copy *zz*)) 
	 (wi (copy *zz*)) 
	 (wpr (copy *zz*))
	 (wpi (copy *zz*))
	 (wtemp 0) 
	 (theta (copy *zz*)) 
	 (tempr 0) 
	 (tempi 0)
	 (j 0) (n 0) (m 0) (mmax 0) (istep 0))
	(declare  (fixnum j n m mmax istep))
	(setf n (cl::* 2 nn)) 
	(setf j 1) 
	(do ((i 1 (cl::+ i 2)))
	    ((cl::> i n) t)
	  (declare (fixnum i))
	  (when (cl::> j i) 
	    (setf tempr (aref data (cl::1- j)))
	    (setf tempi (aref data j)) 
	    (setf (aref data (cl::1- j)) (aref data (cl::1- i)))
	    (setf (aref data j) (aref data i)) 
	    (setf (aref data (cl::1- i)) tempr)
	    (setf (aref data i) tempi))
	  (setf m (cl::floor  n 2))
  label1
	  (when (and (cl::>= m 2) (cl::> j m))
	    (setf j (cl::- j m)) (setf m (cl::floor  m 2))
	    (go label1))
	  (setf j (cl::+ j m))) 
	(setf mmax 2) 
  label2 
	(when (> n mmax)
	  (setf istep (cl::* 2 mmax))
	  (dsetv theta (into (cl::* isign mmax)))
	  (setf theta (with-temps (/ qdtwopi theta)))
	  (dsetv wpr (with-temps  (sin (* 1/2 theta))))
	  (dsetv wpr  (with-temps (* -2 (* wpr wpr))))
	  (dsetv wpi (sin theta))
	  (setf wr (copy *one*))
	  (setf wi (copy *zz*))
	  (do ((m 1 (cl::+ m 2)))
	      ((cl::> m mmax) t)
	    (declare (fixnum m))
	    (do ((i m (cl::+ i istep)))
		((cl::> i n) t)
	      (declare (fixnum i))
	      (setf j (cl::+ i mmax))
	      ;; next line works, but not with dsetv
	      (setf tempr (with-temps (- (* wr (aref data (1- j)))
					 (* wi (aref data j)))))
	      (setf tempi (with-temps (+ (* wr (aref data j))
					 (* wi (aref data (1- j))))))

	      (setf (aref data (cl::1- j))  (- (aref data (cl::1- i)) tempr))
	      (setf (aref data j)   (- (aref data i) tempi))
	      (setf (aref data (cl::1- i))   (+ (aref data (cl::1- i)) tempr))
	      (setf (aref data i) (+  (aref data i) tempi)))
	      
	    (setf wtemp wr)
	    (setf wr (copy (with-temps (+ (+ (* wr wpr) (* (* -1 wi) wpi)) wr))))
	    (setf wi (copy (with-temps (+ (+ (* wi wpr) (* wtemp wpi)) wi)))))
	  (setf mmax istep)
	  (go label2)) 
	(return data)))
)

#|  what the answer should be ...
(defun t1()(polymultfftqd #(1  2 3 4 5 6) #(7 8 9)));; test
(t1)
#(7 22 46 70 94 118 93 54)
(four1 (v2dfa #(1 2 3 4 5 6) 16) 16) ;; except higher precision
#(21.0d0 0.0d0 4.203712543852026d0 17.12548253340269d0
  -9.656854249492387d0 2.999999999999995d0 1.4918055861931159d0
  -4.857754915068683d0 2.999999999999997d0 4.0d0 ...)

(defun randpol(n m)
  (let ((ans (make-array n))
	(lim (expt 2 m)))
    (dotimes (i n ans)(setf (aref ans i)(random lim)))))

|#








