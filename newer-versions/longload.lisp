(load "packs")
(load "ga")				;generic arithmetic
(load "df")				;auto diff "AD"
(load "simpsimp")			;simple algebraic simplifier for math
(load "ma")				;generic math
(load "p2i")				;prefix to infix formatter
; the next 4 taken from MockMMA to provide a better polynomial simplifier
(load "ucons")				;utility for unique conses used below
(load "poly")				;canonical polynomial arith
(load "rat1")				;rational function arithmetic
(load "simprat")			;rational function simplifier
(load "proj-rat") ;projective rationals with 1/0, 0/0
(load "aml-small")
(load "gmp")
(load "mpfr")
(load "qd")


(defpackage :ms	
  (:use :ga :ma :cl :df)
    (:shadowing-import-from 
   :ga
   "+" "-" "/" "*" "expt"		;binary arith
   "=" "/=" ">" "<" "<=" ">="		;binary comparisons
   "sin" "cos" "tan"			;... more trig
   "atan" "asin" "acos"			;... more inverse trig
   "sinh" "cosh" "atanh"		;... more hyperbolic
   "expt" "log" "exp" "sqrt"		;... more exponential, powers
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








