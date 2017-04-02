;;;; -*- Mode: lisp -*-
;; rjf's compile of oct, should be obsolete when rjf learns to use defsystem
(defparameter files
 ' ((:file "qd-package")
   (:file "qd-rep" :depends-on ("qd-package"))
   #-cmu
   (:file "qd-dd" :depends-on ("qd-package"))
   (:file "qd"
	  :depends-on ("qd-rep"))
   (:file "qd-io"
	  :depends-on ("qd"))
   (:file "qd-const"
	  :depends-on ("qd-io"))
   (:file "qd-fun"
	  :depends-on ("qd" "qd-const"))
   (:file "qd-class"
	  :depends-on ("qd-fun"))
   (:file "qd-methods"
	  :depends-on ("qd-class"))
   (:file "qd-format"
	  :depends-on ("qd-methods"))
   (:file "qd-complex"
	  :depends-on ("qd-methods"))
   ))

(defun doit() 
  (mapcar #'(lambda(r)(delete-file (concatenate 'string (cadr r) ".fasl"))) files) ;; load so you can compile
  (mapcar #'(lambda(r)(load (cadr r))) files) ;; load so you can compile
  (mapcar #'(lambda(r)(compile-file (cadr r))) files) ;; compile
  (mapcar #'(lambda(r)(load (cadr r))) files) ;; load the compiled files
)




	   



