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

;;; Here are some simple timing tests, based on Yozo Hida's qd_timer
;;; test code. I've tried to make these versions time the same
;;; operations as Yozo's.

(in-package #:oct)

(defun time-add (&optional (n 100000))
  (declare (fixnum n)
	   (optimize (speed 3)))
  (let ((a (/ #q7))
	(b #q0))
    (declare (type qd-real a b))
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (incf b a)))
    (format t "n = ~D~%" n)
    (format t "b = ~W~%" b)))

(defun time-mul (&optional (n 100000))
  (declare (fixnum n)
	   (optimize (speed 3)))
  (let ((a (+ 1 (/ (float n #q1))))
	(b #q1))
    (declare (type qd-real a b))
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf b (* a b))))
    (format t "n = ~D~%" n)
    (format t "b = ~W~%" b)))
  
(defun time-div (&optional (n 100000))
  (declare (fixnum n)
	   (optimize (speed 3)))
  (let ((a (+ 1 (/ (float n #q1))))
	(b #q1))
    (declare (type qd-real a b))
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf b (/ b a))))
    (format t "n = ~D~%" n)
    (format t "b = ~W~%" b)))

(defun time-sqrt (&optional (n 100000))
  (declare (fixnum n)
	   (optimize (speed 3)))
  (let ((a #q0)
	(b (+ 2 +pi+)))
    (declare (type qd-real a b))
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (setf a (sqrt (+ a b)))))
    (format t "n = ~D~%" n)
    (format t "a = ~W~%" a)))

(defun time-sin (&optional (n 2000))
  (declare (fixnum n)
	   (optimize (speed 3)))
  (let ((a #q0)
	(b (/ +pi+ n))
	(c #q0))
    (declare (type qd-real a b c))
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (incf a b)
	    (incf c (sin a))))
    (format t "n = ~D~%" n)
    (format t "a = ~W~%" a)))

(defun time-log (&optional (n 1000))
  (declare (fixnum n)
	   (optimize (speed 3)))
  (let ((a #q0)
	(c (exp #q-50.1))
	(d (exp (/ #q100.2 n))))
    (declare (type qd-real a c d))
    (time (dotimes (k n)
	    (declare (fixnum k))
	    (incf a (log c))
	    (setf c (* c d))))
    (format t "n = ~D~%" n)
    (format t "a = ~W~%" a)))

#||

Some test results.  These were all run on a Sun Blade 1500 using a 1.5
GHz Ultrasparc III.  I used the default configuration when compiling
qd, and used Sun's C++ compiler with -O.  For the Lisp timing, I used
CMUCL.

Executive summary:

Test	    Time
	qd	oct
----	-----------
add	  0.23	  1.16
mul	  0.749	  1.54
div	  3.00	  3.11
sqrt	 10.57	 12.2
sin	 57.33	 64.5
log	194	119

Times are in microsec/operation for the test.  The default number of
iterations were used.  Most of the timings match my expectations,
including the log test.  Oct uses a different algorithm (Halley's
method) which is faster (in Lisp) than the algorithm used in qd
(Newtwon iteration).

Not also that these times include the 3-arg versions of the routines.

-------------------------------------------------------------------------------
The raw data:

The output from qd_timer -qd -v:

Timing qd_real
--------------

Timing addition...
n = 1000000   t = 0.236154
b = 142857.142857
1000000 operations in 0.236154 s.
  0.236154 us

Timing multiplication ...
n = 1000000   t = 0.748933
b = 2.718280
1000000 operations in 0.748933 s.
  0.748933 us

Timing division ...
n = 1000000   t = 3.004328
b = 0.367880
1000000 operations in 3.004328 s.
  3.004328 us

Timing square root ...
n = 100000   t = 1.057170
a = 2.821980
100000 operations in 1.057170 s.
 10.571696 us

Timing sin ...
n = 20000   t = 1.146667
a = 3.141593
20000 operations in 1.146667 s.
 57.333335 us

Timing log ...
n = 10000   t = 1.939869
a = -50.100000
10000 operations in 1.939869 s.
193.986900 us

--------------------------------------------------
CMUCL results:

CL-USER> (oct::time-add 1000000)

; Evaluation took:
;   1.16 seconds of real time
;   0.98 seconds of user run time
;   0.18 seconds of system run time
;   1,845,637,904 CPU cycles
;   0 page faults and
;   72,000,248 bytes consed.
; 
n = 1000000
b = #q142857.142857142857142857142857142857142857142857142857142857142854q0

CL-USER> (oct::time-mul 1000000)

; Evaluation took:
;   1.53 seconds of real time
;   1.27 seconds of user run time
;   0.25 seconds of system run time
;   2,430,859,732 CPU cycles
;   0 page faults and
;   72,000,248 bytes consed.
; 
n = 1000000
b = #q2.71828046931937688381979970845435639275164502668250771294016782123q0

CL-USER> (oct::time-div 1000000)

; Evaluation took:
;   3.11 seconds of real time
;   2.94 seconds of user run time
;   0.16 seconds of system run time
;   4,957,512,968 CPU cycles
;   0 page faults and
;   72,000,248 bytes consed.
; 
n = 1000000
b = #q0.367879625111086265804761271038216553876450599098470428879260437304q0
CL-USER> (oct::time-sqrt 100000)

; Evaluation took:
;   1.22 seconds of real time
;   1.1 seconds of user run time
;   0.1 seconds of system run time
;   1,938,798,996 CPU cycles
;   0 page faults and
;   24,000,128 bytes consed.
; 
n = 100000
a = #q2.82198033014704783016853125515542796898998765943212617578596649019q0

CL-USER> (oct::time-sin 20000)

; Evaluation took:
;   1.29 seconds of real time
;   1.24 seconds of user run time
;   0.05 seconds of system run time
;   2,053,157,408 CPU cycles
;   0 page faults and
;   27,751,144 bytes consed.
; 
n = 20000
a = #q3.14159265358979323846264338327950288419716939937510582097494458294q0

CL-USER> (oct::time-log 10000)

; Evaluation took:
;   1.19 seconds of real time
;   1.13 seconds of user run time
;   0.04 seconds of system run time
;   1,890,677,952 CPU cycles
;   0 page faults and
;   16,197,536 bytes consed.
; 
n = 10000
a = #q-50.100000000000000000000000000000000000000000000000000000552824575q0

||#
