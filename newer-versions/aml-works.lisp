;; this is a parser for a language that includes most
;; conventional infix math, but also allows ambiguous stuff
;; like a(x). Is it a*x? 

;; 1/2p  comes out as 1/(2p).  1/2*p  is  p/2
;; b^2-4ac works as b^2-4*a*c
;; r(x,y):x+y  is a lisp defun.

;; the parser also includes stuff from matlab and random
;; other usages. it should probably be cleaned up to remove
;; stuff left over from Tilu that does not make particular sense.
;; this has to do with phrases like dx/x  for integrands.
;; and '$[:integral 0, 10:]sin x dx
;; also note onechartoks flag, and the necessity of putting
;; new functions on a list by (newop "foo").

(defpackage :amlparser;; ambigmath language parser
  (:use :common-lisp :excl :cl-user)
  (:nicknames "aml")
  (:export meanings parse-from-string mreadlist)
  (:shadowing-import-from  
   :ga    
   "+" "-" "/" "*" "expt"		;... n-ary arith
   "=" "/=" ">" "<" "<=" ">="		;... n-ary comparisons
   "sin" "cos" "tan"			;... more trig
   "atan" "asin" "acos"			;... more inverse trig
   "sinh" "cosh" "atanh"		;... more hyperbolic
   "expt" "log" "exp" "sqrt"		;... more exponential, powers
   "1-" "1+" "abs" "incf" "decf"
   ;;"numerator" "denominator"	
   "tocl" "re-intern" 
   "true" "false"			;used by simpsimp
   )
  (:import-from :ga simp combmul simp-numb-or-nop;meval
		)
  
  )

(in-package :amlparser)
(use-package :cl-user)

;; this is what I think we need.
;; You can do this $sin 3 to get 0.14112.  you can do '$sin x to get (sin x)
;; or you can do %$sin x to get sin(x).


(set-macro-character ;; this makes LISP stuff, not ma stuff. IN "this" package
 #\$ 
  #'(lambda(st char)
     (declare (ignore char))
     (let* ((theline (read-line st nil t))
	    (ans(aml::parse-from-string theline)))
              (cond ((=(length ans) 0)
		     (error "I can't parse the line ~a" theline))
		    ((=(length ans) 1) ;; put answer in THIS package
		     (ga::re-intern (car ans) *package*))
		    (t(let* 
			  ((alist
			    (format nil "~{~a~^ --or--  ~}"
				    ans))
			   (lal (length ans)))
		 ;; ask user for choice from list??
		 (format t "I found ~s parses are possible, ~a ~%Pick one from 1-~s. ~%->" 
			 lal alist lal)
		 (let ((h (read))) ;; read an integer from user
		   (if (and (integerp h)(<= 1 h lal))
		       (re-intern(nth (1- h) ans) *package*)nil))))))))


;;; utilities from auxfuns/ PAIP

(defun first-or-nil (x)
  "The first element of x if it is a list; else nil."
  (if (consp x) (first x) nil))

(defun first-or-self (x)
  "The first element of x, if it is a list; else x itself."
  (if (consp x) (first x) x))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence 
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))


;;;; The Memoization facility:

(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))


;;(in-package :aml)
;; TO DO:

;; special function names, special variable names, 

;; Sample inputs and correct output:

;; (x^2sin2x)/(p^2-sinx)^2
;;  => ((/ (* (^ |x| 2) (|sin| (* |2| |x|))) (^ (- (^ |p| 2) (|sin| |x|)) 2)))

;; x^2ysinaxcosby
;; => ((* (* (* (^ |x| 2) |y|) (|sin| (* |a| |x|))) (|cos| (* |b| |y|))))

;; Equations that WILL NOT parse correctly:
;;
;;                    [:integral:](dx/x)sqrt(x)
;; (the differential must be in the rightmost factor in the integrand,
;; i.e. this could be written [:integral:]sqrt(x)(dx/x)

(defvar *grammar* "The grammar used by GENERATE.")
(defparameter *ambigmath*
    
    '((s -> (s equals expr) infix-qfuncall)
      ;; added
      (s -> (s defines expr) defines)
      (s -> (s lt expr) infix-qfuncall)
      (s -> (s gt expr) infix-qfuncall)
      (s -> (s le expr) infix-qfuncall)
      (s -> (s ge expr) infix-qfuncall)
      (s -> (expr) identity)
      
      (expr -> (expr plus term) infix-qfuncall)
      (expr -> (expr minus term) infix-qfuncall)
      (expr -> (term) identity)
            
      ;; terms are products of the various subspecies of term.
      ;; note that multiplication without the * has higher precedence than 
      ;; either multiplication with * or division.
      (term -> (term times any_term) infix-qfuncall)
      (term -> (term div any_term) infix-qfuncall)
      (term -> (any_term) identity)
      
      ;; any_term rewrites to any one of the various term types
      (any_term -> (lara_term) identity)
      (any_term -> (larf_term) identity)
      (any_term -> (larn_term) identity)
      (any_term -> (minus any_term) negate)
      
      ;; lara_terms can be multiplied on the left or on the right by anything
      ;; (vars or other sorts of terms)
      
      (lara_term -> (lara_term lara_factor) makeprod)
      (lara_term -> (lara_factor) identity)
      ;; if we multiply a larf_term on the right by a factor, the result
      ;; is a lara_term because it can now be multiplied by var on the right
      (lara_term -> (larf_term delimited_nonvar_factor) makeprod)
      

      
      ;; factors in products. 
      (lara_factor -> (lara_factor exp left_delimited_item) make-expt)
      (lara_factor -> (lara_factor factorial) factorialfn)
      (lara_factor -> (delimited_factor) identity)
      
      
      ;; these are the factors in a product that are well-delimited, by 
      ;; parentheses or otherwise.  I split it up into these two nonterminals
      ;; because this is needed to make funapp_noparens work.
      
      (delimited_factor -> (delimited_nonvar_factor) identity)
      (delimited_factor -> (var) identity)
      (delimited_factor -> (number) identity)
      
      ;; doesn't include variables and numbers, which are also delimited
      (delimited_nonvar_factor -> (lparen expr rparen) parenfact)
      (delimited_nonvar_factor -> (funapp_parens) identity)
      (delimited_nonvar_factor -> (sqrt lparen expr rparen) funapp-parens-onearg)
      (delimited_nonvar_factor -> (delimited_nonvar_factor factorial) factorialfn)

      ;; these are the items where the left side is 
      ;; well-delimited, but the right side may not be.  for example,
      ;; functions applications where the argument is not enclosed in 
      ;; parentheses, like sinx.  i say this is not right-delimited
      ;; because we cannot see where the function ends without processing
      ;; the expression to the right (eg, in sinxyf(x), what is inside the 
      ;; sine?)
      ;; note that all factors in the language are, in fact, left-delimited
      (left_delimited_item -> (delimited_nonvar_factor) identity)
      (left_delimited_item -> (larf_factor) identity)
      (left_delimited_item -> (var) identity)
      (left_delimited_item -> (number) identity)
      (left_delimited_item -> (minus left_delimited_item) negate)
      

      
      ;; larf_term can be multiplied by anything on the left but only
      ;; by other larfs (produces another larf) and factors
      ;; (produces a factor) on the right unless there is an asterisk.
      ;; this is because, for example, sinxy does not mean (sinx)*y
      ;; but sinxcosx does mean (sinx)*(cosx)
      
      (larf_term -> (lara_term larf_factor) makeprod)
      (larf_term -> (larf_term larf_factor) makeprod)
      (larf_term -> (larf_factor) identity)
      
      (larf_factor -> (funapp_noparens) identity)
      
      ;; i could do this more simply by allowing exprlists of one element;
      ;; however, this makes ever expression reduce to both expr and (expr), which 
      ;; is annoying (more ambiguity)
      (funapp_parens -> (name lparen exprlist rparen) funapp-parens)
      (funapp_parens -> (name lparen expr rparen) funapp-parens-onearg)
      
      (exprlist -> (expr comma expr) make-1-3-exprlist)
      (exprlist -> (expr comma expr comma exprlist) extend-1-3-exprlist-5)
      (exprlist -> (expr comma exprlist) extend-1-exprlist-3)
      
      (funapp_noparens -> (fun funarg_noparens) makeoneargfun)
      (funapp_noparens -> (fun exp left_delimited_item funarg_noparens) expfun)

      (funarg_noparens -> (var_prod) identity)
      (funarg_noparens -> (lara_factor) identity)
      (funarg_noparens -> (left_delimited_item) identity)
      (funarg_noparens -> (minus funarg_noparens) negate)
      
      
      ;; var-prod is a product of several variables, with no asterisk
      (var_prod -> (var_prod var_power) makeprod)
      (var_prod -> (var_power) identity)
      
      ;; this is a variable or number, possibly raised to a power
      (var_power -> (var exp left_delimited_item) make-expt)
      (var_power -> (number exp left_delimtied_item) make-expt)
      (var_power -> (var) identity)
      (var_power -> (number) identity)
       

      (larn_term -> (larf_term larn_factor) makeprod)
      (larn_term -> (lara_term larn_factor) makeprod)
      (larn_term -> (larn_factor) identity)
      
      (larn_factor -> (summation) identity)
      (larn_factor -> (integration) identity)
      
      ;; this handles expressions like integral x+f(x)dx
      (integration -> (integral expr differential) integrate)
      ;; this handles expressions like integral dx/x
      (integration -> (integral integrand) integrate-integrand)
      
      ;; parsing the integrand:
      ;; we want to include only terms divisible by a differential (i.e., 
      ;; (xdx)/(sinx) is legal but sinx + dx is not).  this could be done by
      ;; allowing any term in the integrand then using sematics to extract a 
      ;; differential if possible.  instead, i allow only terms whose rightmost 
      ;; factor is a fraction with a differential in the numerator
      
      ;; note that this hack results in ambiguous parses with respect to the
      ;; associativity of multiplication.  i rely on the simplifier to discard
      ;; the unwanted parses by grouping multiplications together.
      ;;(integrand -> (integrand div any_term) quotient-make-integrand)
      ;;(integrand -> (term integrand) product-make-integrand)
      ;;(integrand -> (differential) differential-make-integrand)
      ;;(integrand -> (lparen integrand rparen) parenfact)
      
      
      
      (integrand -> (integrand div any_term) quotient-make-integrand)
      (integrand -> (term differential) product-make-integrand)
      (integrand -> (differential) differential-make-integrand)
      (integrand -> (term diffntl_factor) product-extend-integrand)
      (integrand -> (diffntl_factor) identity)
      
      (diffntl_factor -> (lparen integrand rparen) parenfact)
      
      
      ;; notation for definite integral sign: [:integral lbound, ubound:]
      (integral -> (lbrack colon sym_integral expr comma expr colon rbrack) list)
      ;; indefinite integral sign
      (integral -> (lbrack colon sym_integral colon rbrack) list)
      
      ;; differentials, such as dx
      (differential -> (diffop var) list)
      
      ;; sigma notation for sums
      (summation -> (sigma term) make-summation)

      ;; this is the notaion i use for the big-sigma summation notation:
      ;; [: sum, lowerbound, upperbound :]
      (sigma -> (lbrack colon sym_sum s comma s colon rbrack) list)
      
      ;; might need these later
      ;;(string -> (string name) makestring)
      ;;(string -> (name) symbol-name)
      
            
      ;; var can be any symbol that is not a known function or a number.  this
      ;; is built into the parser (see the lexical-rules function below)
      
      ;; notation for subscripted variables: a[i]
      (var -> (var lbrack expr rbrack) make-subscripted-var)
      
      ;; square roots of numbers should be treated as numbers
      (number -> (sqrt lparen number rparen) funapp-parens-onearg)
      
      (lparen -> \( \()
      (rparen -> \) \))
      (lbrack -> \[ \[)
      (rbrack -> \] \])
      (colon -> \: \:)
      (plus -> + +)
      (times -> * *)
      (minus -> - -)
      (div -> / /)
      (comma -> \, \,)
      (factorial -> ! !)
      (exp -> ^ ^)
      (diffop -> |d| |d|)
      ;; need this rule to make sure d can be a variable as well as the diffop
      (var -> |d| |d|)
      (equals -> = =)
            (equals -> == ==)
      ;; added
      (defines -> |:| |:|)
      (lt -> < <)
      (gt -> > >)
      (le -> (lt equals) lesign)
      (ge -> (gt equals) gesign)
      (sym_sum -> |sum| |sum|)
      (sym_integral -> |integral| |integral|)
      (sqrt -> |sqrt| |sqrt|)
      ))

(defun factorialfn (arg fact)
  (list fact arg))

(defun negate (minus arg)
  (list minus arg))

(defun make-expt (a caret b)
  (list 'expt a b))

(defun funapp-parens (f lparen args rparen)
  (cons f args))

(defun funapp-parens-onearg (f lparen arg rparen)
  (list f arg))

(defun integrate (integral expr differential)
  (if (= (length integral) 8)
      (let ((lbound (nth 3 integral))
	    (ubound (nth 5 integral)))
	;; definite integral
	(list 'integrate expr (cadr differential) lbound ubound)) 
    (list 'integrate expr (cadr differential)))) ;; indefinite integral

;; for the less-nice integrals, where the differential is not on the far right of
;; the expression
(defun integrate-integrand (integral integrand)
  (integrate integral (car integrand) (list '|d| (cadr integrand))))

(defun make-summation (sigma-list exp)
  (let ((lbound (nth 3 sigma-list))
	(ubound (nth 5 sigma-list)))
    (list 'sum exp lbound ubound)))

(defun expfun (fun caret exponent arg)
  (list 'expt (list fun arg) exponent))

(defun makestring (str name)
  (concatenate 'string str (symbol-name name)))

(defun make-subscripted-var (var lbrack expr rbrack)
  (list 'subscript var expr))

(defun make-1-3-exprlist (e1 comma e2)
  (list e1 e2))

(defun extend-1-3-exprlist-5 (e1 comma1 e2 comma2 elist)
  (cons e1 (cons e2 elist)))

(defun extend-1-exprlist-3 (e comma elist)
  (cons e elist))

(defun differential-make-integrand (differential)
  (list 1 (cadr differential)))
  
(defun quotient-make-integrand (integrand div term)
  (list (list div (car integrand) term) (cadr integrand)))

(defun product-extend-integrand (term integrand)
  (list (list '* term (car integrand)) (cadr integrand)))

(defun product-make-integrand (term differential)
  (list term (cadr differential)))

(defun lesign (lt eq) '<=)
(defun gesign (gt eq) '>=)

(defun lexical-rules (word)
  "return a list of rules with word on the right hand side."
  ;; specially hacked for *mathgram* so that if "foo" is on oplist then
  ;; fun -> foo  is a legit rule.

  ;; if "foo" is not on oplist then
  ;; var -> is a legit rule.  
  ;; no overlaps except if you put specifics into the grammar!
  
  ;; changed so name -> foo is legitimate for anything
  ;; var -> foo is legitimate for anything not in oplist
  ;; fun -> foo is legitimate for anything in oplist
  ;; 10/18/99
  
  (or (find-all word *grammar* :key #'rule-rhs :test #'equal)
      (cond ((numberp word)
	     `((number -> ,word ,word)))
	    ((not (isopsym word))
	     `((name -> ,word ,word) (var -> ,word ,word)))
	    ;; add rules fun -> op
	    (t
	     `((name -> ,word ,word) (fun -> ,word ,word))))
      ))
;;      (mapcar #'(lambda (cat) `(,cat -> ,word ,word)) *open-categories*)	;***rjf 5/10/99


;; changed to simplify expressions before removing duplicates
;; 11/8/99 (rw)
#+ignore (defun meanings (&optional words)
  (remove-duplicates (mapcar #'simp (mapcar #'tree-sem (parser words))) 
		     :test #'equal))


(defun meanings (&optional words)
  (remove-duplicates (mapcar #'tree-sem (parser words)) 
		     :test #'equal))
  
  "Return all possible meanings of a phrase.  Throw away the syntactic part."
;;*******
  ;; was...  (tmap #'unpack *)), but now the unwrapping is done when
  ;; we return the answer


(defparameter onechartoks t)

;; the following list should include all operators known. they should
;; be sorted so that the longest prefix comes first. that is, cosh before cos.
;; in here's a few

(defparameter oplist '("cosh" "cos" "sinh" "sin" "tanh"  "tan" 
		       "acosh" "acos" "arccosh" "arccos"
		       "asinh" "asin" "arcsinh" "arcsin"
		       "atanh" "atan" "arctanh" "arctan"
		       "sum" "sqrt" "integral" "ln" "log" "cot" "exp"
		       "phase" "arg" "abs" "alpha" "beta" "gamma" "delta"
	       ;; etc for sech, csch, cosech, cotanh ?, also capitalized?
))


;; put symbols where we can get them fast.

(defparameter
    oplistsyms (mapc #'(lambda (z)(setf (get (intern z :ga) 'mathop)  t)) oplist))

(defun newop (a)
  (assert (stringp a))
  (let ((z (intern a :ga)))
    (setf (get z 'mathop) t)
    (setf oplist (nconc oplist (list a)))
    (setf oplistsyms (nconc oplistsyms  ;; why not use oplist?
			    (list a)))
    a))



;; string-downcase added to ensure that uppercase operators are recognized
;; rw 12/2/99
;; changed to string-upcase, so operators are always in uppercase.
;; grw 2/14/2000

;; changed to downcase 5/15/01, rjf using "mlisp" setting for allegro.

(defun breakup (s2 oplist)
  (dolist (s oplist (values (subseq s2 0 1)(subseq s2 1)))
    (if (equal 0 (search s (string-downcase s2) :start1 0 :start2 0))
	(return (values s (subseq s2 (length s)))))))

(defun parse-from-string (str)
  (with-input-from-string (*standard-input* str) (meanings)))

;; simpsimp functions, extras
;; some helper functions for dealing with fractions:

;; is x a fraction? (i.e., (/ a b))
(defun fraction? (x)
  (and (listp x) (eq (car x) '/)))

;; returns the numerator/denominator from (/ x y)
(defun numerat (x)
  (cadr x))
(defun denominat (x)
  (caddr x))

(defun /-simp (xpr)
  ;; xpr must have 2 items.  this is different from the original
  ;; implementation.
  ;; if the denominat is equal to 1, remove it
  (if (eq (cadr xpr) 1)
      (car xpr)
    (simp-numb-or-nop '/ #'/ xpr)))

(defun *-simp (xpr)
  (let ((x (combine-fractions xpr)))
    (if (fraction? x)
	(let ((n (simp (numerat x)))
	      (d (simp (denominat x))))
	  (/-simp (list n d)))
        (combmul xpr))))
      
;; combines fractions that are multiplied together.  for example,
;; (* (/ a x) (/ b y)) becomes (/ (* a b) (* x y)).  this facilitates
;; cancelling terms.

(defun combine-fractions (xpr)
  (if (null (cdr xpr)) 
      (car xpr)
      (let ((factor (car xpr))
	    (rest (combine-fractions (cdr xpr))))
	(cond ((and (fraction? factor) (fraction? rest))
	       (list '/
		     (list '* (numerat factor) (numerat rest))
		     (list '* (denominat factor) (denominat rest))))
	      ((and (fraction? factor) (not (fraction? rest)))
	       (list '/
		     (list '* (numerat factor) rest)
		     (denominat factor)))
	      ((and (not (fraction? factor)) (fraction? rest))
	       (list '/
		     (list '* factor (numerat  rest))
		     (denominat rest)))
	      (t ;; neither is a fraction
	       (list '* factor rest)))))
	)

(setf (get '* 'simp) #'*-simp)
(setf (get '/ 'simp) #'/-simp)


;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Some of this code is from Paradigms of AI Programming
;;;; Those portions copyright (c) 1991 Peter Norvig

;;;; CFG parser based on Norvig's syntax2.lisp: The PSG-based natural language parser.
;;;; This version handles semantics as described in Section 19.5.
;;;; Includes *grammar5*  *grammar6* and *mathgram*. USE one of these.;
;;;;  Must also read in auxmacs.lisp auxfns.lisp  to get memoization.
;(load "auxmacs") ;; necessary pieces in this file
;(eval-when (compile load) (load "auxfns"))


;; Lisp-matlab (Lmat) parser (just the lexical part, for now)
;;(c) copyright 1990, 1999 by Richard J. Fateman

;; This started as Lmath, a  Mathematica parser written in Lisp.
;; it was converted to a MATLAB language parser, starting 5/6/99 by RJF
;; it can also be used as a lexical analyzer for tokens read by
;; the CFG parser based on Norvig's syntax2.cl file


(eval-when (compile)(declaim (optimize (speed 3)(safety 0))))

;;(eval-when (compile) (load "mat")); ;; get all the symbols from this file
;;(in-package :mat)

(defvar matbuffer nil) 
(defvar stream t) ;; if needed

;; The first section consists of readtable hacking for matlab parser.
;; We set up a separate readtable for
;; matlab input, and utilize it when scanning.
;; We use lisp atoms to store information on tokens.

(defvar matrt (copy-readtable nil))
;;(setf (readtable-case matrt) :preserve) ;preserve the case of symbols read in

;; I don't like the following choice, but the common lisp
;; standards committee majority make it very difficult
;; to have both cases work.

(setf (readtable-case matrt) :downcase)	;don't preserve the case of symbols read in

;; debugging flags 
 
(setq *print-level* nil *print-length* nil *print-pretty* t)

(defun pc()(peek-char nil stream nil #\newline))
(defun rc()(read-char stream))

(defun char-to-int (c)  ;; return the integer 0-9 corresponding to
			;; the character c, #\0 - #\9
  ;; will not work in larger bases though..
  (let ((h (char-int c)))
    (cond ((< h 48)(- h 7))  ;; #\A=17
	  ((< h 58)  (- h  48)) ; #\0 is 48 in ascii.
	  (t (- h 87)) ; #\a=97
	  )))

(defun collect-integer (val r)
  (cond ((eql (pc) #\newline) val)
	((digit-char-p (pc) r)	;r is radix. Matlab is always 10, I guess
	 (collect-integer (+ (char-to-int (rc))(* r val)) r))
	(t val)))

;; to test scanner, try typing
;;  (mreadl)  

(defun single-macro-character (stream char)
  (declare (ignore stream))
  (intern (string char) :amlparser ;;; WARNING
	  ))

(mapc #'(lambda(r)(set-macro-character r #'single-macro-character nil matrt))
      (coerce "+-*/\^&[]()=,%!:{}" 'list))

(set-macro-character #\<
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc) (#\newline '<) (#\= (rc) '<=) (t '<)))
  nil matrt)
      
;(set-syntax-from-char #\' #\" matrt nil)  ;matlab uses ' instead of "
;; the apostrophe is used for string delimiter and transpose. to
;; figure out its use we look at the previous token and if it is
;; an operator the apostrophe is an open-quote.

(set-macro-character #\'
  #'(lambda (stream char)
      (declare (ignore char))
      (cond ((or (null prev) (mattop prev))
	     (readquote nil))  ; ='Hello a String'
	    (t 'transpose))		; A'
)
  nil matrt)

(defun mattop(x) (get x 'matop))

;;; read a string until a "'" or eof

(defun readquote (s)
  (let ((r (read-char stream nil 'eof)))
    (cond ((eql r 'eof) (coerce (nreverse (cons r s)) 'string))
	  ((eql r #\' )(coerce (nreverse s) 'string))
	  (t (readquote (cons r s))))))


(set-macro-character #\~
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc) (#\newline '~) (#\= (rc) '~=)  (t '~)))
  nil matrt)

(set-macro-character #\>
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc)
		  (#\newline '>)
		  (#\= (rc) '>=)
		 
		  (t '>)))
  nil matrt) 


(set-macro-character #\= 
  #'(lambda(stream char)
      (declare (ignore char))
      (case (pc)
		 (#\newline '|=|)
		 (#\= (rc)  '|==|)
		 (t '|=|))) nil matrt)

;; 3 dots, ... means throw out rest of line and #\newline. read from next line


(set-macro-character 
 #\. 
 #'(lambda (stream char)
     (declare (ignore char))
;;     (format t "got a .")
      (case (pc)
	(#\newline '|.|)
	(#\. (rc) 
;;	     (format t "got another .")
	     (case (pc)
	       (#\newline '|..|) 
	       (#\.  ; ...
		;;(format t "got a third .")
		(loop (if (eql (rc) #\newline)
			  (return (rt)))) ;; get rid of rest of line 
		)
	       (t '|..|)))
	(#\/ (rc) 'array-rightdiv)
	(#\* (rc) 'array-mult)
	(#\\ (rc) 'array-leftdiv)
	(#\^ (rc) 'array-power)
	(#\' (rc) 'array-transpose)
	(t '|.|)))
  nil matrt)


 
(set-macro-character #\newline #'(lambda(stream char)  (declare (ignore char))
					'e-o-l) nil matrt)


(defun makemt(x)(setf (get x 'matop) t))

(makemt '\.)
(makemt '\;)
(mapc #'makemt '(= * ^	&  +  -  [  ]   > >=  < <= ! ~ ~= / ))
(mapc #'makemt '(|.| |..| |(| |)|))
;;(mapc #'makemt '( |(| |)|))

(makemt '|\\|)
(makemt '|'|)
(makemt '|,|)

#+ignore
(defun commentskip (stream char)
  (declare (ignore c))
  (loop
    (if (eql(rc) #\newline) (return(mread1)))))

(defun commentskip (stream char)
  (declare (ignore stream c)) 'e-o-l)

(set-macro-character #\% #'commentskip nil matrt)  ;)
 
;;; end of the lexical analysis part
;;----------------------------------------------------------
;;; 			The Parser
;; You can use (p)  to try out the parser by typing in from the
;; keyboard. It sets up the readtable and calls parse-comp.

;; Reading from lines is set up so that if a sentence ends at
;; an end-of-line, the parse is completed. Otherwise, the e-o-l
;; is absorbed and the reading continued.  A continuation line
;; can be forced by a \.  (This is Mathematica's usual operation)

(defvar interactive t) ; t means 2 eol's ends a command. not for files.

;;mreadl is a debugging loop that just reads lexemes until it reads eol
(defvar next nil)
(defvar prev nil)

(defun mreadl(&aux (stream *standard-input* )
		   next (*readtable* matrt)
		   (prev nil))

  (loop 
   (setq next (mread1))
    (when (eq  next 'e-o-l) (return 'done))
    (setf prev next)
    (print next)))


(defun mreadlist(&optional (stream *standard-input*))
  (let ((next nil)
	(*readtable* matrt)
	(prev nil)
	(moretoks nil)
	(tokens nil))
      (declare (special moretoks))

  (loop 
    (setq next (mread1))
    	
    (if  (eq  next 'e-o-l) (return (nreverse tokens)))
    (cond
     ((and onechartoks ;; NIL if  we want to allow xcoshx as just one var
      (setf moretoks (stringmathdecomp next))) ;; xcoshx = x cosh x ?
      (dolist (i moretoks)
	
	(push i tokens)

	(setf prev i)))
     (t (setf prev next)
	
	(push next tokens))))
  ))

(defun stringmathdecomp(s)
  (cond ((null (symbolp s))nil) ; numbers
	(t(setf s (smd(symbol-name s)))))) ;  s= |xcoshx| ==> "xcoshx"

(defun smd(s)
  (cond ((= 0 (length s))nil)
	(t(multiple-value-bind
	      (first rest)
	      (breakup s oplist)
	    (cons (intern first :amlparser ;;; WARNING
			  )(smd rest))))))

;; the following list should include all operators known. They should
;; be sorted so that the longest prefix comes first. That is, cosh before cos.
;; in Here's a few

;; put symbols where we can get them fast.

(setf oplistsyms (mapc #'(lambda (z)(setf (get (intern z :amlparser
						       ) ;;;WARNING
					       
					       'mathop)  t)) oplist))

(defun isopsym(r)(if (symbolp r)(get r 'mathop) nil))

;; same except this reads from file named filename

(defun mreadf (filename &aux
		   next (*readtable* matrt)
		   (prev nil))
  (declare (special moretoks))
      
  (with-open-file (stream filename :direction :input)
    (declare (special stream))
	 (loop 
	   (setq next (mread1))
	   (when (eq (peek-char t stream nil 'eof ) 'eof) (return 'done))
	   
	   (cond
	    (onechartoks ;; NIL if  we want to allow xcoshx as just one var
	     (setf moretoks (stringmathdecomp next)) ;; xcoshx = x cosh x ?
	     (dolist (i moretoks)
	       
	(print i)

	(setf prev i)))
     (t (setf prev next)
	
	(print next ))))))


(defmacro rt()`(cond((null matbuffer)(mread1))
		(t (prog1 matbuffer (setq matbuffer nil )))))

(defmacro eolp(end) ;;used all over to see if we've reached an end of line
  `(and ,end (eq 'e-o-l (peek-token))))

;; this function reads a token. Although it looks like it
;; just reads a lisp s-expression or number, it uses a different
;; read-table. If mread1 encounters a #\newline, it returns the
;; atom e-o-l, as specified in the read-table.

(defun mread1()
  ;;  (format t "~% next char = ~s" (pc))
  (cond ((member (pc)'( #\space #\tab #\page) :test #'char=)
	 (rc)(mread1)) 
	((digit-char-p (pc));; next character is a digit 0-9
	 ;; can't just use lisp reader to try to read a number
	 ;;(let ((*readtable* greadtable))	   (read stream ))
	 ;;(collect-integer  (char-to-int(read-char stream)) 10) ;radix 10 default
	 
      (collect-number  (char-to-int(read-char stream)) 10) ;radix
	  )
	;; should collect floats too. 
	(t (intern
	    (or(read-preserving-whitespace stream nil 'e-o-l)
	       'False)
	    
	    :amlparser)  ;;;;;;; WARNING. TRICKY...
	   ;; nil reads as False
	   )))

(defun collect-fraction (mult)
  (cond((not (characterp (pc))) 0)
       ((digit-char-p (pc) 10)
	(+ (* (char-to-int (rc)) mult)
	   (collect-fraction (/ mult 10))))
       (t 0)))
(defun collect-exponent ();; given d23 returns 1.0*10^23
  (let ((sign 1)			;  sign of exponent
	(marker (pc)))
    (cond((member marker '(#\e #\E #\d #\D) :test #'char=)
	  ;; this program could overflow/underflow
	  ;; we ignore distinction between e [single]
	  ;; and d [double], until final conversion.
	  
	  ;; First, we read past the e,E,d,D character
	  (rc) 
	  ;; Read the (optional) sign
	  (cond ((char= (pc) #\-) (rc)(setf sign -1))
		((char= (pc) #\+) (rc))) ;sign is +1
	  ;; Do exact arithmetic, convert to float at the end
	  (* (if (char= #\e (char-downcase marker)) 1.0e0 1.0d0)
	      (expt 10 (* sign  (collect-integer 0 10))))
	  ;; Why is this better than (expt 10.0 blah) or (expt 10.0d0 blah)?
	  ;; This way is more accurate, and lisp gives us perfect
	  ;; rational arithmetic to make the program simple.
	  )
	 (t 1.0e0 ))))

(defun collect-number (val radix)
  ;; we don't allow .123, but require 0.123
  (let ((m (collect-integer val radix))) ; before decimal
    (cond((not (characterp (pc)))
	  m)
	 ((char= (pc) #\.)
	  (rc)				;remove the "."
	  ;; this next operation, on fp numbers, could over/underflow
	  
		(or (ignore-errors
		     (* (+ m (collect-fraction 1/10))(collect-exponent))) 
		    
		    'overflowed))
	
	 ((member (pc) '(#\e #\E #\d #\D) :test #'char=)
	   ;; case of 123e04 for example, no decimal point
	  
		(or (ignore-errors
		     (*  m (collect-exponent)))
		    'overflowed)
		  )
	 (t m))))

#+ignore
(defun p (&optional(stream *standard-input*) 
		  &aux (interactive t)
		       res
		       (*readtable* matrt)
		       (matbuffer nil)
		       (prev nil))
;  (rt) ;;get something in matbuffer
  (setq res (catch 'endofparse (parse-comp t)))  ;; end=t means a #\newline will end expr.
  (cond((eq matbuffer 'e-o-l)  (if res res 'Null)) ;; proper ending
       (t (format t "~%Unexpected token at end of expression:~s~%" matbuffer)
	  res)))


#+ignore
(defun pt ( parseprog &optional(stream *standard-input*) ;parse test
		  &aux (interactive t)
		       res
		       (*readtable* matrt)
		       (matbuffer nil)
		       (prev nil))
;  (rt) ;; get something in matbuffer
  (setq res (catch 'endofparse (funcall parseprog t)))  ;; end=t means a #\newline will end expr.
  (cond((eq matbuffer 'e-o-l)  (if res res 'Null)) ;; proper ending
       (t (format t "~%Unexpected token at end of expression:~s~%" matbuffer)
	  res)))

(defun peek-token() (cond(matbuffer)
			 (t (setq matbuffer(mread1)))))

;; Here is where Norvig's code begins

(defstruct (rule (:type list)) lhs -> rhs sem)

(defstruct (tree (:type list) (:include rule) (:copier nil)
                 (:constructor new-tree (lhs sem rhs))))

(defstruct (parse) "A parse tree and a remainder." tree rem)

(defun parse-lhs (parse) (tree-lhs (parse-tree parse)))


(defun rules-starting-with (cat)
  "Return a list of rules where cat starts the rhs."
  (find-all cat *grammar* 
            :key #'(lambda (rule) (first-or-nil (rule-rhs rule)))))


(defun complete-parses (parses)
  "Those parses that are complete (have no remainder)."
  (find-all-if #'null parses :key #'parse-rem))

(defun append1 (items item)
  "Add item to end of list of items."
  (append items (list item)))


(defun parser (&optional words)
  "Return all complete parses of a list of words."
  (clear-memoize 'parse)		;***
  (if (null words)(setf words (mreadlist))) ;; read from input
  (mapcar #'parse-tree (complete-parses (parse words))))

(defun use (grammar)
  "Switch to a new grammar."
  (clear-memoize 'rules-starting-with)
  (clear-memoize 'lexical-rules)
  (length (setf *grammar* grammar)))

(defparameter *open-categories* '(N V A	;Name
				  )
  "Categories to consider for unknown words")

(defun parse (words)
  "Bottom-up parse, returning all parses of any prefix of words.
  This version has semantics."
  (unless (null words)
    (mapcan #'(lambda (rule)
                (extend-parse (rule-lhs rule) (rule-sem rule) ;***
                              (list (first words)) (rest words) nil))
            (lexical-rules (first words)))))

(defun extend-parse (lhs sem rhs rem needed) ;***
  "Look for the categories needed to complete the parse.
  This version has semantics."
  (if (null needed)
      ;; If nothing is needed, return this parse and upward extensions,
      ;; unless the semantics fails
      (let ((parse (make-parse :tree (new-tree lhs sem rhs) :rem rem)))
        (unless (null (apply-semantics (parse-tree parse))) ;***
          (cons parse
                (mapcan
                  #'(lambda (rule)
                      (extend-parse (rule-lhs rule) (rule-sem rule) ;***
                                    (list (parse-tree parse)) rem
                                    (rest (rule-rhs rule))))
                  (rules-starting-with lhs)))))
      ;; otherwise try to extend rightward
      (mapcan
        #'(lambda (p)
            (if (eq (parse-lhs p) (first needed))
                (extend-parse lhs sem (append1 rhs (parse-tree p)) ;***
                              (parse-rem p) (rest needed))))
        (parse rem))))

(defun apply-semantics (tree)
  "For terminal nodes, just fetch the semantics.
  Otherwise, apply the sem function to its constituents."
  (if (terminal-tree-p tree)
      (tree-sem tree)
      (setf (tree-sem tree)
            (apply (tree-sem tree)
                   (mapcar #'tree-sem (tree-rhs tree))))))

(defun terminal-tree-p (tree)
  "Does this tree have a single word on the rhs?"
  (and (length=1 (tree-rhs tree))
       (atom (first (tree-rhs tree)))))

(defun length=1 (x) 
  (and (consp x) (null (cdr x))))

(memoize 'lexical-rules)
(memoize 'rules-starting-with)
(memoize 'parse :test #'eq)

;;;; Grammars



(defun integers (start end)
  "A list of all the integers in the range [start...end] inclusive."
  (if (> start end) nil
      (cons start (integers (+ start 1) end))))

(defun infix-funcall (arg1 function arg2)
  "Apply the function to the two arguments"
  (funcall function arg1 arg2))
(defun infix-qfuncall (arg1 function arg2)
  "Apply the function to the two arguments"
  (list function arg1 arg2))


(defun union* (x y) (if (null (intersection x y)) (append x y)))
(defun set-diff (x y) (if (subsetp y x) (set-difference x y)))

;; end of Norvig's code

;;added
(defun defines(x def y)`(defun ,(car x),(cdr x) ,y))
(defun equalsign(x e y)(list 'setf x y))
(defun comma(x e y)(cons x y))
(defun \, (x  y)(cons x y))
(defun aster(x e y)(list e x y))
(defun plu(x e y)(list e x y))
(defun makeprod(x y)(list '* x y))
(defun makepower(x arrow  y)(list 'expt x y))
(defun brackfun(a b c d)`(aref ,a ,@c))
(defun parenfact(a b c)b)
(defun makeoneargfun (f x)(list f x))

; (function ->(FUN LPAR exprlist RPAR ) funapp) ;;function call

(defun funapp(a b c d) (cons a c ))

;; b(c)^c  could be b * c^c  or (b(c))^c  or b (c^c) if we don't know if b is fun or var.

;;[1] USER(314): (m)
;;a=sin(c)^c ;; is this ambiguous??
;;((SETF |a| (|sin| (EXPT |c| |c|))) 
;;  or ...   (SETF |a| (EXPT (|sin| |c|) |c|))) answer 7/21/99 RJF

;; but a=b(c)^c is not. 
;; nor is
;;     a=sin c^c


(defun tmap(f x) ;; map f over the tree x
  (cond ((null x)nil)
	((consp x)(cons (tmap f (car x)) (tmap f (cdr x))))
	(t (funcall f x))))

(defun unpack(x) (if (symbolp x) (intern (symbol-name x) ;;:ga
					 ) x)) ;; warning
;; to remove ucky package qualifiers from an answer, try
;; (tmap #'unpack answer)

#|examples
q(a,b,c):b^2-4ac  

|#

(use *ambigmath*)



