(load "packs")
(load "ga")				;generic arithmetic

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
)
(load "gc-mpfr.dll")
(load "mpfr")
(load "quad-fast")

;; put your code in here..








