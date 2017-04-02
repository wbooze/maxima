;; Lisp decode-float does not work for inf or nan. At least in ACL
;; Let's see if we can make this happen.
;; thanks to Duane Rettig for suggesting I should just
;; violate the type abstraction as a relatively clean hack.

;; First some utility programs, the (only) ones that hack
;; on the representation.

;; Given a double float, convert to an array of 64 bits.
;; this must be compiled with type checking disabled.

(defun dfloat2bits(x &aux 
		     (ans (make-array 64 :element-type '(unsigned-byte 1))))
  (declare(optimize (speed 3)(safety 0))
	  (type (simple-array (unsigned-byte 1)(64)) x))
  (dotimes (i 64 ans) (declare (fixnum i))(setf (aref ans i) (aref x i))))

;; given a 64bit array, convert to double float
;; this must be compiled with type checking disabled.

(defun bits2dfloat(x)  
  (declare(optimize (speed 3)(safety 0))
	  (double-float x))
  (+ 0.0d0 x))

;;example  (bits2dfloat(dfloat2bits -543.21d0))

;; The numbering of bits in the float is the same as
;; the numbering of bits in this bit array.
;; It is reversed from the way they are often pictured, however,
;; with the exponent on the left.

(defun b2i (x ans) ;; bits to integer, reverse order
  (let ((L (1-(length x))))
    (dotimes (i (1+ L) ans)
      (setf ans (+ (ash ans 1) (aref x (- L i)))))))

(defun parts(x) ;; separate a double-float into parts.
  (assert (double-float-p x )) ; for now.
  (let* ((ba (dfloat2bits x))
	 (sign (aref ba 63))
	 (exp (subseq ba 52 63))
	 (frac(subseq ba 0 52)))
;;    (format t "~%raw values sign=~s exp=~s frac=~s" sign exp frac)
    (setf exp (- (b2i exp 0) 1023)) ; for normalized, anyway
    (setf frac ;check for zero
      (cond((and (= exp -1023)(every #'zerop frac)) 0) ; zero
	   ((and (= exp 1024) (every #'zerop frac)) 0) ; infinity
	    
    ;; next line is only for normalized; put in leading bit
	   (t(b2i frac 1))))
    (values sign exp frac 
	    ;; you may not want this last part, which essentially
	    ;; is the same as (rational x), but it is here for
	    ;; a sanity check.
	    (* (expt -1 sign)  ;; this is the float EXACTLY as rational
	       (expt 2 (+ exp -52))
	       frac)
	    )))

;; decode-float is sort of like taking the sign, exp, frac
;; from parts, and returning instead 
;; (* frac #.(expt 2.0d0 -52))
;; (1+ exp)
;; (expt -1.0d0 sign)

#+ignore
(defun my-decode-float(x)
  (assert (double-float-p x)) ;; rewrite for single if needed.
  
  (multiple-value-bind
      (sign exp frac)
      (parts x)
    (values   (* frac #.(expt 2.0d0 -53))
	      (1+ exp)
	      (expt -1.0d0 sign))))

;; note. This version of my-decode-float does this:
;; (my-decode-float #.excl::*nan-double*)
;; returns 0.5000000000000001d0, 1025, 1.0d0
;; which is better for my purposes than "error"

;; this could be the end of the file.  BUT...

;; if you want to encode "stuff" in a nan, try this

(defvar nan-template  (dfloat2bits #.excl::*nan-double*))

;; Our way to set funny nan bits:
;; start with the bit-string for a general NaN
;; and put in some particular fraction.

(defun makenan(c) ;; c is a char string, up to 7 chars. 
 (let ((h (copy-seq nan-template)))
   (setf (subseq h 0 51) (n2b (chars2num c)))
   (bits2dfloat h)))

;; hm. we can pack 7 chars in 7-bit ascii 7X7=49 < 52.
;; Feed this next program character string. get a number
;; to put in the NaN fraction.

(defun chars2num(c &aux (ans 0))
  ;; c is probably no more than 7 chars for this application.
  (dotimes (i (length c) ans)
    (setf ans(ash ans 7)) ; only first 128, up to #\~ = 126
    (incf ans (char-code (aref c i)))))

(defun num2chars(n &aux (ans nil)) ;reverse transformation from above
  (declare (special letter-lookup))
  (loop while (> n 31) do
    (push (aref letter-lookup (logand n 127)) ans)
    (setf n (ash n -7)))
  (coerce ans 'string))

;; there used to be int-char for this..
(defparameter letter-lookup 
    (make-array 127 :element-type 'character :initial-element #\space))
(loop for i in 
      (coerce "abcdefghijklmnopqrstuvwxyz 
ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-+
,.:[]{}:;\"\\|!@#$%^&*()_+~`<>?/" 'list) 
    do (setf (aref letter-lookup  (char-code i))i))
;; second version

(defun my-decode-float(x)
  (assert (double-float-p x)) ;; rewrite for single if needed.
  (multiple-value-bind
      (sign exp frac)
      (parts x)
    ;; not clear what we want to return here for a NaN, but at least for
    ;; testing try this..
    (if (= exp 1024) ;; a nan or inf!!
	(values			;;	(num2chars (ash frac -1))
	 (if (= frac 0) "Inf" (num2chars frac))
		(1+ exp)
		(expt -1.0d0 sign))
      
	;;(format nil "A double-NaN with code = ~a" (num2chars frac))
    (values   (* frac #.(expt 2.0d0 -53))
	      (1+ exp)
	      (expt -1.0d0 sign)))))

;;pos integer number to bitarray and back
(defun n2b(x)
  (let*
      ((L (integer-length x))
       (ans (make-array L :element-type '(unsigned-byte 1))))
    (dotimes (j L ans)
      (setf (aref ans j) (logand 1 x))
      (setf x (ash x -1)))))

;; reverse direction is (b2i b 0)
;; example.  try (my-decode-float  (makenan "HiDuane"))


