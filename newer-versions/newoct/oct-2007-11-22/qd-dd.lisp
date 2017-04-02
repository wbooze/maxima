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

(in-package #:octi)

;;; double-double float routines needed for quad-double.
;;;
;;; Not needed for CMUCL.
;;;
;;; These routines were taken directly from CMUCL.

#||
(declaim (inline quick-two-sum))
(defun quick-two-sum (a b)
  "Computes fl(a+b) and err(a+b), assuming |a| >= |b|"
  (declare (double-float a b)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((s (+ a b))
	 (e (- b (- s a))))
    (declare (double-float s e))
    (values s e)))
||#

(defmacro quick-two-sum (s e x y)
  (let ((a (gensym))
	(b (gensym)))
    `(let* ((,a ,x)
	    (,b ,y))
      (declare (double-float ,a ,b ,s ,e))
      (setf ,s (+ ,a ,b))
      (setf ,e (- ,b (- ,s ,a))))))

#||
(declaim (inline two-sum))
(defun two-sum (a b)
  "Computes fl(a+b) and err(a+b)"
  (declare (double-float a b)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((s (+ a b))
	 (v (- s a))
	 (e (+ (- a (- s v))
	       (- b v))))
    (declare (double-float s v e))
    (values s e)))
||#

(defmacro two-sum (s e x y)
  "Computes fl(a+b) and err(a+b)"
  (let ((a (gensym))
	(b (gensym))
	(v (gensym)))
    `(let ((,a ,x)
	   (,b ,y))
      (declare (double-float ,a ,b))
      (setf ,s (+ ,a ,b))
      (let ((,v (- ,s ,a)))
	(declare (double-float ,v))
	(setf ,e (+ (- ,a (- ,s ,v))
		    (- ,b ,v)))))))

(declaim (inline two-prod))
(declaim (inline split))
;; This algorithm is the version given by Yozo Hida.  It has problems
;; with overflow because we multiply by 1+2^27.
;;
;; But be very careful about replacing this with a new algorithm.  The
;; values computed here are very important to get the rounding right.
;; If you change this, the rounding may be different, which will
;; affect other parts of the algorithm.
;;
;; I (rtoy) tried a different algorithm that split the number in two
;; as described, but without overflow.  However, that caused
;; -9.4294948327242751340284975915175w0/1w14 to return a value that
;; wasn't really close to -9.4294948327242751340284975915175w-14.
;;
;; This also means we can't print numbers like 1w308 with the current
;; printing algorithm, or even divide 1w308 by 10.
#+nil
(defun split (a)
  "Split the double-float number a into a-hi and a-lo such that a =
  a-hi + a-lo and a-hi contains the upper 26 significant bits of a and
  a-lo contains the lower 26 bits."
  (declare (double-float a))
  (let* ((tmp (* a (+ 1 (expt 2 27))))
	 (a-hi (- tmp (- tmp a)))
	 (a-lo (- a a-hi)))
    (values a-hi a-lo)))


;; Here is my limited understanding of what SPLIT is really supposed
;; to do.
;;
;; Let A be a double-float number.  We want to split the fraction bits
;; into 2 parts of 26 bits each.  This is best explained by example.
;; Let use use A = 1d50.  Use INTEGER-DECODE-FLOAT to display the bits
;; of A:
;;
;; (write (integer-decode-float 1d50) :base 2) ->
;;   10001000110110000111011000101011111100110010010011010
;;
;; Break this into 2 parts with the lower part having 27 bits and the
;; upper having 26 (because of the extra "hidden" bit):
;;
;;   10001000110110000111011000 101011111100110010010011010
;;
;; But this is not enough.  Note that the bottom half has a leading 1.
;; We want to round up the upper part.  Then we need to account for
;; this in the lower part:
;;
;;   10001000110110000111011001 -10100000011001101101100110
;;
;; This is the answer we want.  Convert these back to floats with the
;; appropriate exponents, and we get:
;;
;; 1.0000000087331024d50 and -8.733102285997912d41
;;
;; While this example worked out nicely, we should note that the
;; rounding operation above should be done in an IEEE round-to-even
;; fashion.  So if the lower part of the bits is exactly "half", we
;; round the upper part to even.  Thus,
;;
;; (float #b10001000110110000111011000100000000000000000000000000 1d0)
;; should be split into two parts:
;;
;;   10001000110110000111011000 100000000000000000000000000
;;
;; but
;;
;; (float #b10001000110110000111011001100000000000000000000000000 1d0)
;; is
;;
;;   10001000110110000111011010 -100000000000000000000000000

(defun split (a)
  "Split the double-float number a into a-hi and a-lo such that a =
  a-hi + a-lo and a-hi contains the upper 26 significant bits of a and
  a-lo contains the lower 26 bits."
  (declare (double-float a)
	   (optimize (speed 3)))
  ;; This splits the number a into 2 halves of 26 bits each, but the
  ;; halves are, I think, supposed to be properly rounded in an IEEE
  ;; fashion.
  ;;
  ;; For numbers that are very large, we use a different algorithm.
  ;; For smaller numbers, we can use the original algorithm of Yozo
  ;; Hida.
  (if (> (abs a) #.(scale-float 1d0 (- 1023 27)))
      ;; I've tested this algorithm against Yozo's method for 1
      ;; billion randomly generated double-floats between 2^(-995) and
      ;; 2^996, and identical results are obtained.  For numbers that
      ;; are very small, this algorithm produces different numbers
      ;; because of underflow.  For very large numbers, we, of course
      ;; produce different results because Yozo's method causes
      ;; overflow.
      (let* ((tmp (* a #.(+ 1 (scale-float 1d0 -27))))
	     (as (* a #.(scale-float 1d0 -27)))
	     (a-hi (* (- tmp (- tmp as)) #.(scale-float 1d0 27)))
	     (a-lo (- a a-hi)))
	(declare (double-float tmp as a-hi a-lo))
	(values a-hi a-lo))
      ;; Yozo's algorithm.
      (let* ((tmp (* a #.(float (+ 1 (expt 2 27)) 1d0)))
	     (a-hi (- tmp (- tmp a)))
	     (a-lo (- a a-hi)))
	(declare (double-float tmp a-hi a-lo))
	(values a-hi a-lo))))


;; Note that if you have an architecture that has a fused
;; multiply-subtract instruction that computes a*b-c with exactly one
;; rounding operation, you can use that instead of the complicated
;; routine below.  Power PC chips have such an instruction.
;;
;; Here is the code to do that, where (fused-multiply-subtract a b p)
;; computes a*b-p.
;;
;; (defun two-prod (a b)
;;   "Compute fl(a*b) and err(a*b)"
;;   (declare (double-float a b))
;;   (let* ((p (* a b))
;; 	 (err (fused-multiply-subtract a b p)))
;;     (values p err)))

(defun two-prod (a b)
  "Compute fl(a*b) and err(a*b)"
  (declare (double-float a b)
	   (optimize (speed 3) (safety #-allegro 0 #+allegro 1) (debug 0)))
  (let ((p (* a b)))
    (declare (double-float p))
    (multiple-value-bind (a-hi a-lo)
	(split a)
      (declare (double-float a-hi a-lo))
      (multiple-value-bind (b-hi b-lo)
	  (split b)
	(declare (double-float b-hi b-lo))
	(let ((e (+ (+ (- (* a-hi b-hi) p)
		       (* a-hi b-lo)
		       (* a-lo b-hi))
		    (* a-lo b-lo))))
	  (declare (double-float e))
	  (values p e))))))

(declaim (inline two-sqr))
(defun two-sqr (a)
  "Compute fl(a*a) and err(a*b).  This is a more efficient
  implementation of two-prod"
  (declare (double-float a)
	   (optimize (speed 3) (safety #-allegro 0 #+allegro 1) (debug 0)))
  (let ((q (* a a)))
    (declare (double-float q))
    (multiple-value-bind (a-hi a-lo)
	(split a)
      (declare (double-float a-hi a-lo))
      (values q (+ (+ (- (* a-hi a-hi) q)
		      (* 2 a-hi a-lo))
		   (* a-lo a-lo))))))
