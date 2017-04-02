;; this should be packaged up in asdf or quicklisp or something.
;; 9/22/2011 RJF
(load "packs")
(load "ga")				;generic arithmetic
(load "p2i")				;prefix to infix formatter
(load "df")				;auto diff "AD"
(load "poly")				;canonical polynomial arith
(load "simpsimp")			;simple algebraic simplifier for math
(load "ma")				;generic math

(load "ninterval")
;; the next 4 taken from MockMMA to provide a better polynomial simplifier
(load "ucons")				;utility for unique conses used below

(load "rat1")				;rational function arithmetic
(load "simprat")			;rational function simplifier
;;(load "proj-rat") ;projective rationals with 1/0, 0/0
					;(load "aml-small")
;;(load "gmp.dll") ;; or a more specific version
;;(load "gmp")
;;(load "gc-mpfr.dll") ;; if you want to do the mpfr load below
;;(load "mpfr")
(load "qd.dll")
(load "qd")


(defpackage :ms	
  (:use :ga :ma :cl :df)
    (:shadowing-import-from 
   :ga
   "+" "-" "/" "*" "expt"		;binary arith
   "=" "/=" ">" "<" "<=" ">="		;binary comparisons
   expt log exp sqrt
   sin cos tan
   atan asin acos atan2
   sinh cosh tanh
   asinh acosh atanh
   "1-" "1+" "abs"
   "tocl" "re-intern"
   "incf" "decf"
    "numerator" "denominator"
   max min
   
   )
;;  (:import-from :df  "df")
;;  (:import-from :ma  "ma" "simp" "ratsimp" "ratexpand"
;;		"p" "ex"    ;; some fun examples
;;    )
)

;; put your code in here..








