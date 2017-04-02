;;;generic complex
(defpackage :comp				;uses generic arithmetic
  (:use  :common-lisp :ga )
  (:shadowing-import-from 
   :ga
   "+" "-" "/" "*" "expt"		;binary arith
   "=" "/=" ">" "<" "<=" ">="		;binary comparisons
   "sin" "cos" "tan"			;... more trig
   "atan" "asin" "acos"			;... more inverse trig
   "sinh" "cosh" "atanh"		;... more hyperbolic
   "expt" "log" "exp" "sqrt"		;... more exponential, powers
   "1-" "1+" "abs" "incf" "decf"
   "tocl" "re-intern"
   "numerator" "denominator"
   "realpart" "complex" "imagpart"
   )
  (:export    "comp" )
)

(provide "complex" )
(in-package :comp)


(defstruct comp r i)			;real and imaginary parts

(defmethod print-object ((a comp) stream)  (format stream "~a+~a*i"  (comp-r a)(comp-i a)))
    
;;; to do:  redefine two-arg-*  of complex objects in terms of two-arg-* of pieces.
;;; 

;;; forbid comparison of complex objects except for = and /=

(defcomparison >)
(defcomparison =)
(defcomparison /=)
(defcomparison <)
(defcomparison <=)
(defcomparison >=)

			  
