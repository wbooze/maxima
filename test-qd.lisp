;;; compile inside qd, some tests to see what the qd allocation
;;; overhead might be

(in-package :qd)

(eval-when (compile) (load "qd.fasl")) ; need the macros 

(defun time-sin(n)
  (let ((m1 (into 1)))
  (dotimes (i n)(declare (fixnum n))  ;; just use lisp
    (sin (the aqd m1))))) ;; this uses the OO defmethod for sine

(defun time-sin2(n)    ;; avoid the allocation
  (let* ((m1 (into 1))
	 (ans (make-aqd))
	 (in (aqd-q m1))
	 (ina(aqd-q ans)))
      (declare (optimize speed)
	       (type (simple-array double-float (4)) in inat))
      (dotimes (i n ans)(declare (fixnum n))
	(qd_sin in ina))))  ;;this uses the raw c entry point directly


(defun time-empty(n);; a program to use to time the empty loop, compiled
  (let* ((m1 (into 1))
	 (ans (make-aqd))
	 (in (aqd-q m1))
	 (ina(aqd-q ans)))
    (declare (optimize speed) (ignore in ina)
	     (type (simple-array double-float (4)) in inat))
    (dotimes (i n ans)(declare (fixnum n))  ;; just counting
      (identity 1))))

(defun time-make-aqd(n);; a program to use to time  make-aqd
  (let* ((m1 (into 1))
	 (ans (make-aqd))
	 (in (aqd-q m1))
	 (ina(aqd-q ans)))
    (declare (optimize speed) (ignore in ina)
	     (type (simple-array double-float (4)) in inat))
    (dotimes (i n ans)(declare (fixnum n))  ;; just counting
       (make-aqd))))

(defun time-sin3(n)  ;; use our destructive set operation
  (let* ((m1 (into 1))
	 (ans (make-aqd)))
    (dotimes (i n ans)(declare (fixnum n)) ;; this is just as fast as time-sin2
      (dsetv ans (sin m1)))))

(defun time-sin4(n)
  (let* ((m1 (into 1))
	 (ans (make-aqd)))
    (dotimes (i n ans)(declare (fixnum n)) ; use our with-temps hack
	(setf ans (with-temps (sin m1))))))


#| fundamentally, for any of the time-sin* programs
compiled correctly [careful: must set up macros at compile time]
the cost for any of these sin(1) loops is about the same, though the storage
allocation / deallocation varies wildly among them. For example, to
run the loop 10,000 times,  
time-sin4 allocates 10      cons cells, 168      other bytes, 0 static bytes
time-sin  allocates 100,054 cons cells, 721,424  other bytes, 0 static bytes

And compiled wrong, with macroexpansion at runtime, this same program takes 12% more time..
time-sin4 allocates  770,076 cons cells, 3,601,816 other bytes, 0 static bytes


How can it be that such a big difference in what is apparently going on
between correctly compiled code doesn't seem to affect the run time much?

Here's why:  the empty loop running  (time-empty 1,000,000) takes 15 ms.
the loop doing 1,000,000 allocations of aqd takes 267+124(GC)=391 ms.
So in the loop running 10,000 times, the aqd allocation is about 4 ms.
The loop actually computing sin(1) 10,000 times is about 3,500 ms. So
the extra 4ms doesn't show up in 3500+4.

How does the sin(1) time here compare to sin(1.0d0)?  
The double-float version seems to be about 350 times faster. 
uses 1 cons cell, 480,000 other bytes.

|#




