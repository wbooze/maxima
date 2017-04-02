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

(defpackage #:oct-internal
  (:use #:cl)
  (:nicknames #:octi)
  (:export #:%quad-double
	   #:read-qd
	   #:add-qd
	   #:add-qd-d
	   #:add-d-qd
	   #:sub-qd
	   #:sub-qd-d
	   #:sub-d-qd
	   #:neg-qd
	   #:mul-qd
	   #:mul-qd-d
	   #:sqr-qd
	   #:div-qd
	   #:div-qd-d
	   #:make-qd-d
	   #:integer-decode-qd
	   #:npow
	   #:qd-0
	   #:qd-1
	   #:qd-2
	   #:qd-3
	   #:qd-parts
	   #:+qd-one+
	   #:+qd-zero+
	   #:+qd-pi+
	   #:+qd-pi/2+
	   #:+qd-pi/4+
	   #:+qd-2pi+
	   #:+qd-log2+
	   ;; Functions
	   #:hypot-qd
	   #:abs-qd
	   #:sqrt-qd
	   #:log-qd
	   #:log1p-qd
	   #:exp-qd
	   #:sin-qd
	   #:cos-qd
	   #:tan-qd
	   #:sincos-qd
	   #:asin-qd
	   #:acos-qd
	   #:atan-qd
	   #:atan2-qd
	   #:sinh-qd
	   #:cosh-qd
	   #:tanh-qd
	   #:asinh-qd
	   #:acosh-qd
	   #:atanh-qd
	   #:qd-=
	   #:qd->
	   #:qd-<
	   #:qd->=
	   #:qd-<=
	   #:zerop-qd
	   #:plusp-qd
	   #:minusp-qd
	   #:integer-decode-qd
	   #:decode-float-qd
	   #:scale-float-qd
	   #:ffloor-qd
	   #:random-qd
	   #:with-qd-parts
	   #:rational-to-qd
	   #:float-infinity-p
	   #:float-nan-p
	   )
  #+cmu
  (:export #:add-qd-dd
	   #:sub-qd-dd
	   #:div-qd-dd
	   #:make-qd-dd)
  #+cmu
  (:import-from #:c
		#:two-prod
		#:two-sqr)
  #+cmu
  (:import-from #:ext
		#:float-infinity-p
		#:float-nan-p
		#:float-trapping-nan-p
		#:double-double-float))

(defpackage #:net.common-lisp.oct
  (:use #:cl #:oct-internal)
  (:nicknames #:oct)
  (:shadow #:+
	   #:-
	   #:*
	   #:/
	   #:1+
	   #:1-
	   #:zerop
	   #:plusp
	   #:minusp
	   #:abs
	   #:sqrt
	   #:log
	   #:exp
	   #:sin
	   #:cos
	   #:tan
	   #:asin
	   #:acos
	   #:atan
	   #:sinh
	   #:cosh
	   #:tanh
	   #:asinh
	   #:acosh
	   #:atanh
	   #:expt
	   #:=
	   #:/=
	   #:<
	   #:>
	   #:<=
	   #:>=
	   #:complex
	   #:integer-decode-float
	   #:decode-float
	   #:scale-float
	   #:float
	   #:floor
	   #:ffloor
	   #:ceiling
	   #:fceiling
	   #:truncate
	   #:ftruncate
	   #:round
	   #:fround
	   #:realpart
	   #:imagpart
	   #:conjugate
	   #:float-sign
	   #:qd-format-exp
	   #:max
	   #:min
	   #:cis
	   #:phase
	   #:signum
	   #:coerce
	   #:random
	   #:realp
	   #:complexp
	   #:numberp
	   #:incf
	   #:decf
	   #:float-digits
	   #:rational
	   #:rationalize
	   )
  ;; Export types
  (:export #:qd-real
	   #:qd-complex)
  ;; Export functions
  (:export #:+
	   #:-
	   #:*
	   #:/
	   #:1+
	   #:1-
	   #:zerop
	   #:plusp
	   #:minusp
	   #:abs
	   #:sqrt
	   #:log
	   #:exp
	   #:sin
	   #:cos
	   #:tan
	   #:asin
	   #:acos
	   #:atan
	   #:sinh
	   #:cosh
	   #:tanh
	   #:asinh
	   #:acosh
	   #:atanh
	   #:expt
	   #:=
	   #:/=
	   #:<
	   #:>
	   #:<=
	   #:>=
	   #:complex
	   #:integer-decode-float
	   #:decode-float
	   #:scale-float
	   #:float
	   #:floor
	   #:ffloor
	   #:ceiling
	   #:fceiling
	   #:truncate
	   #:ftruncate
	   #:round
	   #:fround
	   #:realpart
	   #:imagpart
	   #:conjugate
	   #:float-sign
	   #:qd-format-exp
	   #:max
	   #:min
	   #:cis
	   #:phase
	   #:signum
	   #:coerce
	   #:random
	   #:realp
	   #:complexp
	   #:numberp
	   #:incf
	   #:decf
	   #:float-digits
	   #:rational
	   #:rationalize
	   #:make-qd
	   )
  ;; Constants
  (:export #:+pi+
	   #:+pi/2+
	   #:+pi/4+
	   #:+2pi+
	   #:+log2+)
  ;; CMUCL supports infinities.
  #+cmu
  (:export #:+quad-double-float-positive-infinity+
	   #:+quad-double-float-negative-infinity+))
