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

;;; This is the asdf definition for oct.  I don't normally use this,
;;; so it might be out of date.  Use at your own risk.

;; If you want all core functions to be inline (like the C++ code
;; does), add :qd-inline to *features* by enabling the following line.
;; This makes compilation much, much slower, but the resulting code
;; conses much less and is significantly faster.
#+(not (and cmu x86))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :qd-inline *features*))

;; To be able to inline all the functions, we need to make
;; *inline-expansion-limit* much larger.
;;
;; Not sure we really want to inline everything, but the QD C++ code
;; inlines all of the functions so we do the same.  This makes CMUCL
;; take a very long time to compile the code, and the resulting
;; functions are huge.  (I think div-qd is 8 KB, and sqrt-qd is a
;; whopping 30 KB!)
;;
#+(and cmu qd-inline)
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf ext:*inline-expansion-limit* 1600))

(defpackage #:oct-system
  (:use #:cl))

(in-package #:oct-system)

(asdf:defsystem oct
  :description "A portable implementation of quad-double arithmetic.  See <http://www.common-lisp.net/project/oct>."
  :author "Raymond Toy"
  :maintainer "See <http://www.common-lisp.net/project/oct"
  :licence "MIT"
  :version "0.0"			; No real version yet
  :components
  ((:file "qd-package")
   (:file "qd-rep" :depends-on ("qd-package"))
   #-cmu
   (:file "qd-dd" :depends-on ("qd-package" "qd-rep"))
   (:file "qd"
	  :depends-on ("qd-rep" #-cmu "qd-dd"))
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
