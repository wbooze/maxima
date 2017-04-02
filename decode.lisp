; A small package of programs to help use IEEE Float NaNs and IEEE
;; Infinity from within (Allegro CL) Lisp.  The basic strategy is to
;; allow a programmer access to the encoding of the fraction part of
;; the NaN, and with additional utility programs, make progress toward
;; integrating these special values into other programs. Included are
;; routines showing how to use a NaN fraction for encoding short
;; messages. 3 letters in a single, 7 in a double. This could be
;; used for diagnostic information.

;;We do this because the built-in program in Lisp, decode-float,
;;does not work for inf or nan. At least in ACL.
;; This package defines my-decode-float  which should model
;; decode-float in most ways, except for treatment of NaN and Inf.

;; Thanks to Duane Rettig for suggesting I should just violate the
;; type abstraction (twice) as a relatively simple way of getting
;; access to the encodings, and for helping get the programs to work.

;; First some utility programs, the (only) ones that hack
;; on the representation. These could be written in some other
;; Lisp implementation, or in C.

;; Given a double float, convert to an array of 64 bits.
;; this must be compiled with type checking disabled. And the
;; reverse.

;; The corresponding programs, sfloatbits  and bits2sfloat, for single
;; floats appear later.

#+ignore ;; faster one is next
(defun dfloat2bits(x &aux 
		     (ans (make-array 64 :element-type '(unsigned-byte 1))))
  (declare(optimize (speed 3)(safety 0))
	  (type (simple-array (unsigned-byte 1)(64)) x))
  (dotimes (i 64 ans) (declare (fixnum i))(setf (aref ans i) (aref x i))))

(defun dfloat2bits(x) ;; x is a double float
  (let ((ans (make-array 64 :element-type '(unsigned-byte 1))))
    (dfloat2bytes x ans) ; this call will alias a double-float to bit array.
    ans))

(defun dfloat2bytes(x ans) 
  ;; auxilary program, 2 phoney aliases
  ;; x is a double-float, ans is a bit array.
  ;; this program treats them both as byte arrays.
  (declare(optimize (speed 3)(safety 0))
	  (type (simple-array (unsigned-byte 8)(8)) x ans))
  (dotimes (i 8 ans) (declare (fixnum i))(setf (aref ans i) (aref x i))))

;; Given a 64bit array, convert to double float
;; this must be compiled with type checking disabled.

(defun bits2dfloat(x)  
  (declare(optimize (speed 3)(safety 0))
	  (double-float x))
  (+ 0.0d0 x))

(defun bytes2dfloat(x)  
  (declare(optimize (speed 3)(safety 0))
	  (double-float x))
  (+ 0.0d0 x))

;;example  (bits2dfloat (dfloat2bits -543.21d0))  returns -543.21d0


;; The numbering of bits in the float is the same as the numbering of
;; bits in this bit array.  It is reversed from the way they are often
;; pictured, however.

(defun b2i (x ans) ;; bits to integer, reverse order
  (let ((L (1-(length x))))
    (dotimes (i (1+ L) ans)
      (setf ans (+ (ash ans 1) (aref x (- L i)))))))

;;pos integer number to bitarray and back
(defun n2b(x)
  (let*
      ((L (integer-length x))
       (ans (make-array L :element-type '(unsigned-byte 1))))
    (dotimes (j L ans)
      (setf (aref ans j) (logand 1 x))
      (setf x (ash x -1)))))

;; reverse direction is (b2i b 0)

(defun dparts(x) ;; separate a double-float into parts.
  (assert (typep x 'double-float)) ; for now.
  (let* ((ba (dfloat2bits x))
	 (sign (aref ba 63))
	 (exp (subseq ba 52 63))
	 (frac(subseq ba 0 52)))
   ;; (format t "~%raw values sign=~s exp=~s frac=~s" sign exp frac)
    (setf exp (- (b2i exp 0) 1023)) ; for normalized, anyway
    (setf frac ;check for zero, inf
      (cond((and (= exp -1023)(every #'zerop frac)) 0) ; zero
	   ((and (= exp 1024) (every #'zerop frac)) 0) ; infinity
	   ;;should check for "denormalized" or subnormal here.
	   ;; else put in hidden normalized bit; put in leading bit
	   (t(b2i frac 1))))
    (values sign exp frac
	    ;; this last value should be
	    ;; the same as (rational x), but it is here for
	    ;; a sanity check.
	    #+ignore
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
(defun my-decode-dfloat(x)
  (assert (typep x 'double-float)) ;; rewrite for single if needed.
  
  (multiple-value-bind
      (sign exp frac)
      (dparts x)
    (values   (* frac #.(expt 2.0d0 -53))
	      (1+ exp)
	      (expt -1.0d0 sign))))

;; note. This version of my-decode-dfloat does this:
;; (my-decode-dfloat #.excl::*nan-double*)
;; returns 0.5000000000000001d0, 1025, 1.0d0
;; which is better for my purposes than "error".

;; This could be the end of the file if all we needed was
;; double float decode for *nan* BUT...
;; if we want to encode "stuff" in a nan, we can do this

(defvar nan-template  (dfloat2bits #.excl::*nan-double*))

;; Our way to set funny nan bits:
;; start with the bit-string for a general NaN
;; and put in some particular fraction.

(defun makenan(c) ;; c is a char string, up to 7 chars. 
 (let ((h (copy-seq nan-template)))
   (setf (subseq h 0 51) (n2b (chars2num c)))
   (bytes2dfloat h)))

;; Note that we can pack 7 chars in 7-bit ascii 7X7=49 < 52.
;; Feed this next program a character string. You get a number
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

;; There used to be int-char for this..
(defparameter letter-lookup 
    (make-array 127 :element-type 'character :initial-element #\space))
(loop for i in 
      (coerce "abcdefghijklmnopqrstuvwxyz 
ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-+
,.:[]{}:;\"\\|!@#$%^&*()_+~`<>?/" 'list) 
    do (setf (aref letter-lookup  (char-code i))i))

;; Second version of my-decode-dfloat
(defun my-decode-dfloat(x)
  (assert (typep x 'double-float)) ;; rewrite for single if needed.
  (multiple-value-bind
      (sign exp frac)
      (dparts x)
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

(defun my-decode-float(r)
  (typecase r (single-float (my-decode-sfloat r))
	    (double-float (my-decode-dfloat r))
	    (real (my-decode-dfloat (* 1.0d0 r)))
	    (t (error "decode-float called on ~s, not a number" r))))

;; example.  try (my-decode-float  (makenan "HiDuane"))

;; An appendix to decode.lisp,
;; which is small package of programs to help use IEEE Float NaNs and IEEE
;; Infinity from within (Allegro CL) Lisp.  

;; This is supposed to work for SINGLE FLOAT format.
;; convert a single-float to an array of bits.

(defun sfloat2bits(x &aux 
		     (ans (make-array 32 :element-type 'bit :short t)))
  (declare(optimize (speed 3)(safety 0))
	  (type (short-simple-array bit(32)) x))
  (dotimes (i 32 ans) (declare (fixnum i))(setf (aref ans i) (aref x i))))

(defun sfloat2bytes(x &aux  
		     (ans (make-array 4 :element-type '(unsigned-byte 8) :short t)))
  (declare(optimize (speed 3)(safety 0))
	  (type (short-simple-array (unsigned-byte 8)(4)) x))
  (dotimes (i 4 ans) (declare (fixnum i))(setf (aref ans i) (aref x i))))

;; Given a 32bit array, convert to double float
;; this must be compiled with type checking disabled.

(defun bits2sfloat(x)  
  (declare(optimize (speed 3)(safety 0))
	  (single-float x))
  (+ 0.0 x))

(defun bytes2sfloat(x)  
  (declare(optimize (speed 3)(safety 0))
	  (single-float x))
  (+ 0.0 x))

;;example  (bits2sfloat(sfloat2bits -543.21))
;;example  (bytes2sfloat(sfloat2bytes -543.21)) 

(defun sparts(x) ;; separate a double-float into parts.
  ;(assert (typep x 'single-float)) ; for now.
  (let* ((ba (sfloat2bits x))
	 (sign (aref ba 31))
	 (exp (subseq ba 23 31))
	 (frac (subseq ba 0 23))
	 )
;;    (format t "~%raw values sign=~s exp=~s frac=~s" sign exp frac)
    ;;(setf exp (- (b2i exp 0) 1023)) ; for normalized, anyway
    (setf exp (- (b2i exp 0) 127))
    (setf frac ;check for zero, inf
      (cond((and (= exp -127)(every #'zerop frac))
	    0) 				; zero
	   ((and (= exp 128) (every #'zerop frac))
	    0)				; infinity
	   ;;should check for denormalized here.
	   ;; else put in hidden normalized bit; put in leading bit
	   (t(b2i frac 1))))
    (values sign 
	    exp
	    ;;(1- exp)
	    ;;(ash frac -1)
	    frac
	    ;; this last value should be
	    ;; the same as (rational x), but it is here for
	    ;; a sanity check.
	    ;;#+ignore
	    (* (expt -1 sign)  ;; this is the float EXACTLY as rational
	       (expt 2 (+ exp -23))
	       frac)
	    )))

;; decode-float is sort of like taking the sign, exp, frac
;; from parts, and returning instead 
;; (* frac #.(expt 2.0d0 -52))
;; (1+ exp)
;; (expt -1.0d0 sign)

(defvar snan-template  (sfloat2bits #.excl::*nan-single*))

;; Our way to set funny nan bits:
;; start with the bit-string for a general NaN
;; and put in some particular fraction.

(defun makesnan(c) ;; c is a char string, up to 7 chars. 
 (let ((h (copy-seq snan-template)))
   (setf (subseq h 0 23) (n2b (chars2num c)))
   (bits2sfloat h)))

;; Note that we can pack 3 chars in 7-bit ascii 3X7=21 < 23.
;; Feed this next program a character string. You get a number
;; to put in the NaN fraction.

(defun my-decode-sfloat(x) ;; single float version
  (multiple-value-bind
      (sign exp frac)
      (sparts x)
    ;; not clear what we want to return here for a NaN, but at least for
    ;; testing try this..
    (if (= exp 128) ;; a nan or inf!!
	(values			;;	(num2chars (ash frac -1))
	 (if (= frac 0) "Inf" (num2chars frac))
		(1+ exp)
		(expt -1.0 sign))
      
	;;(format nil "A single-NaN with code = ~a" (num2chars frac))
    (values   (* frac #.(expt 2.0 -24))
	      (1+ exp)
	      (expt -1.0 sign)))))

;; example.  try (my-decode-sfloat  (makesnan "HiD"))



