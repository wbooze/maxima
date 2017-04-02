(defpackage :ga				;generic arithmetic
  (:use :cl)
  (:shadow
   + - / * expt		;... n-ary arith
   = /= > < <= >=		;... n-ary comparisons
   sin cos tan
   atan asin acos atan2
   sinh cosh tanh
   asinh acosh atanh
   expt log exp sqrt	   
   min max
   1- 1+ abs incf decf
   numerator denominator	
   ffloor fceiling ftruncate fround
   ash evenp oddp
   scale-float
   ;;true false 
	   )
  (:export re-intern tocl
      sin cos tan
   atan asin acos atan2
   sinh cosh tanh
   asinh acosh atanh
   expt log exp sqrt	   
   min max
   1- 1+ abs incf decf
   numerator denominator	
   combmul  simp-numb-or-nop
   neg erf erfc
   simp )
  (:use :common-lisp))

(defpackage :gmp				;uses generic arithmetic
  (:use :ga :cl)
  (:shadowing-import-from  
   :ga    
   + - / * expt		;... n-ary arith
   = /= > < <= >=		;... n-ary comparisons
   1- 1+ abs incf decf
   sin cos tan
   atan asin acos atan2
   sinh cosh tanh
   asinh acosh atanh
   expt log exp sqrt	   
   min max
   tocl  re-intern 
   numerator denominator
   ash evenp oddp
   ) )



(defpackage :amlparser ;; ambigmath language parser
  (:use :common-lisp #+allegro :excl :cl-user)
  (:nicknames aml)
  (:export meanings parse-from-string mreadlist)
  (:shadowing-import-from  
   :ga    
   
   + - / * 
   = /= > < <= >=	
   sin cos tan
   atan asin acos atan2
   sinh cosh tanh
   asinh acosh atanh
   expt log exp sqrt	   
   min max
   1- 1+ abs incf decf
   numerator denominator	
   tocl re-intern 
 ;;  true false			;used by simpsimp
   )
  (:import-from :ga simp combmul simp-numb-or-nop ;meval
		) )

(defpackage :df				;derivative and function package
  (:use  :cl)
  (:shadowing-import-from  
   :ga    
   
   + - / * 
   = /= > < <= >=	
   sin cos tan
   atan asin acos atan2
   sinh cosh tanh
   asinh acosh atanh
   expt log exp sqrt	   
   min max
   1- 1+ abs incf decf
   numerator denominator	
   tocl re-intern 
 ;;  true false			;used by simpsimp
   )
   (:export df)
   )

(defpackage :ma				;uses generic arithmetic
  (:use :ga :cl)
(:shadowing-import-from  
   :ga    
   + - / * 
   = /= > < <= >=	
   sin cos tan
   atan asin acos atan2
   sinh cosh tanh
   asinh acosh atanh
   expt log exp sqrt	   
   min max
   1- 1+ abs incf decf
   numerator denominator	
   tocl re-intern 
 ;;  true false			;used by simpsimp
   )
  (:import-from :aml meanings mreadlist)
  (:export "ma" "simp" "%" "ratsimp" "ratexpand"
	   ))

;;; put more package definitions in here when we
;;; believe them.



(defpackage :dec
    (:use :ga :cl)
    
  (:shadowing-import-from  
   :ga    
   + - / * 
   = /= > < <= >=	
   sin cos tan
   atan asin acos atan2
   sinh cosh tanh
   asinh acosh atanh
   expt log exp sqrt	   
   min max
   1- 1+ abs incf decf
   numerator denominator	
   tocl re-intern 
 ;;  true false			;used by simpsimp
   )
  
  (:export "dec" ))

(defpackage :octi
  (:use  :cl)
  (:export into lisp2oct oct2lisp))

(defpackage :mpfr				;uses generic arithmetic
  (:use :ga :cl)
  (:shadowing-import-from  
   :ga    
   "+" "-" "/" "*" "expt"		;... n-ary arith
   "=" "/=" ">" "<" "<=" ">="		;... n-ary comparisons
   "1-" "1+" abs incf decf
   min max
   sin cos tan 
   asin acos 
   atan log   ;; these are trickier, 1 or 2 args.
   ;; must add all these like
   sinh cosh tanh 
   asinh acosh atanh sqrt exp
   numerator denominator
   neg erf erfc
   scale-float
  ; besselj
   ))