;;

;; this is a parser for a language that includes most
;; conventional infix math, but also allows ambiguous stuff
;; like a(x). Is it a*x? 

;; 1/2p  comes out as 1/(2p).  1/2*p  is  p/2
;; b^2-4ac works as b^2-4*a*c
;; r(x,y):x+y  is a lisp defun.

;; this version cleaned up some from (aml-works)
;; parser which also includes stuff from matlab and random
;; other usages.

;; Note onechartoks flag, which if default true parses letters
;; as separate names, e.g. ab is a*b.  Exceptions to this one-char 
;; rule are the built in functions; you can add to them by
;; by calling (newop "foo").
;; if you want to add a new NAME, e.g. "pi", try 
;; (newvar "pi")
;; preferred mode now is onechartoks TRUE. The parser 

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
   "1-" "1+" "abs" 
   ;;"incf" "decf"
   ;;"numerator" "denominator"	
   "tocl" "re-intern" 
   "true" "false"			;used by simpsimp
   )
  (:import-from :ga simp combmul simp-numb-or-nop;meval
		)
  
  )

(in-package :amlparser)
(use-package :cl-user)

;; This is what I think we need for a single-character "escape to infix".
;; We use the dollar-sign for this purpos.
;; You can do this $sin 3 to get 0.14112.  That is, you
;; parse to infix and then execute.  If you want to parse but
;; not execute, use the ordinarily Lisp quote: you can do 
;; '$sin x to get (sin x)

;; in the generic arithmetic ":ma" package there is a %escape to
;; make a math object (not a lisp symbolic object) which displays
;; in infix.  This is %. So with :ma 
;; you can do %$sin x to get sin(x).

(set-macro-character ;; this makes LISP stuff out of infix
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

;;; much of this code is based on Peter Norvig's Paradigms of AI Programming.
;;; (PAIP).

;;; First we use a few utilities from auxfuns/ PAIP

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
;; remaining suggestions for additions:
;; add more special function names, special variable names. 

;; Sample inputs and correct output:

;; (x^2sin2x)/(p^2-sinx)^2
;; ==>(/ (* (expt x 2) (sin (* 2 x))) (expt (- (expt p 2) (sin x)) 2))


;; x^2ysinaxcosby
;; => (* (* (* (expt x 2) y) (sin (* a x))) (cos (* b y)))

(defvar *grammar* "The grammar used by GENERATE.")
(defparameter *ambigmath*
    
    '(
      (s -> (s assign expr) infix-qfuncall)
      (s -> (s colon expr) infix-qfuncall) ; 
      (s -> (s def expr) defines)	; 
      (s -> (expr) identity)
      (expr -> (expr plus term) infix-qfuncall)
      (expr -> (expr minus term) infix-qfuncall)
      (expr -> (term) identity)
      (expr -> (expr le term) infix-qfuncall)
      (expr -> (expr ge term) infix-qfuncall)
      (expr -> (expr gt term) infix-qfuncall)
      (expr -> (expr lt term) infix-qfuncall)
      (expr -> (expr ee expr) infix-qfuncall) ;;==
      (gt -> > >)
      (ge -> < <)
      (le -> <= <=)
      (ge -> >= >=)
      (ee -> == ==)
            
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
      ;; however, this makes every expression reduce to both expr and (expr), which 
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
      (def -> \:= defines)
      (plus -> + +)
      (times -> * *)
      (minus -> - -)
      (div -> / /)
      (comma -> \, \,)
      (factorial -> ! !)
      (exp -> ^ ^)
      (assign -> = setf)
      
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

(defun make-summation (sigma-list exp)
  (let ((lbound (nth 3 sigma-list))
	(ubound (nth 5 sigma-list)))
    (list 'sum exp lbound ubound)))

(defun expfun (fun caret exponent arg)
  (list 'expt (list fun arg) exponent))

(defun makestring (str name)
  (concatenate 'string str (symbol-name name)))

(defun make-subscripted-var (var lbrack expr rbrack)
  (list 'aref var expr))

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
(defun eesign (gt eq) '=)

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
  
  #+ignore
  (or (find-all word *grammar* :key #'rule-rhs :test #'equal)
      (cond ((numberp word)
	     `((number -> ,word ,word)))
	    ((or (isvarsym word)(not (isopsym word)))
	     `((name -> ,word ,word) (var -> ,word ,word)))
	    ;; add rules fun -> op
	    (t
	     `((name -> ,word ,word) (fun -> ,word ,word))))
      )
  
  (or (find-all word *grammar* :key #'rule-rhs :test #'equal)
      (cond ((numberp word)
	     `((number -> ,word ,word)))
	      ((isopsym word)
	     `((name -> ,word ,word) (fun -> ,word ,word)))
	    (t
	     `((name -> ,word ,word) (var -> ,word ,word)))
	    ;; add rules fun -> op
	  )
      )
  )

(defun meanings (&optional words)
  (remove-duplicates (mapcar #'tree-sem (parser words)) 
		     :test #'equal))
  
  "Return all possible meanings of a phrase.  Throw away the syntactic part."

(defparameter onechartoks t);; t means only 1-char names. xy parses as x*y

;; the following list should include all operators known. they should
;; be sorted so that the longest prefix comes first. that is, cosh before cos.
;; We also put all 2 char operators in the language

(defparameter oplist '("cosh" "cos" "sinh" "sin" "tanh"  "tan" 
		       "acosh" "acos" "arccosh" "arccos"
		       "asinh" "asin" "arcsinh" "arcsin"
		       "atanh" "atan" "arctanh" "arctan"
		       "sum" "sqrt" "integral" "ln" "log" "cot" "exp"
		       "complex"
		       "phase" "arg" "abs" "alpha" "beta" "gamma" "delta"
		       "===" "==" ">=" "<=" ":="
	       ;; etc for sech, csch, cosech, cotanh ?, also capitalized?
		       ))


;; the following list should include all operators known. They should
;; be sorted so that the longest prefix comes first. That is, cosh before cos.
;; also, put symbols where we can get them fast.

(eval-when (load eval)
  (mapc #'(lambda (z)(setf (get (intern z :amlparser ) ;;;WARNING
				'mathop)  t)) oplist))

(defun newop (a)
  (assert (stringp a))
  (let ((z (intern a :amlparser))) ;;;WARNING
    (setf (get z 'mathop) t)
    (unless (member a oplist :test #'string=)
      (setf oplist (nconc oplist (list a)))  )
    z))

(defvar varlist '("pi" ))

(defun newvar (a)  ; way to declare new variables, e.g. pi, euler
  (assert (stringp a))
  (let ((z (intern a :ga)))
    (setf (get z 'mathvar) t)
    (push a varlist)
    (push a oplist)
    a))

(defun breakup (s2 oplist)
  (dolist (s oplist (values (subseq s2 0 1)(subseq s2 1)))
    (cond ((digit-char-p (aref s2 0)) 	   ;; try to read integer
	   (multiple-value-bind (val count)
	       (parse-integer s2 :start 0 :junk-allowed t)
	       (return (values val (subseq s2 count)))))
	  ((equal 0 (search s s2  :start1 0 :start2 0)) ;found keyword!
	   (return (values s (subseq s2 (length s)))))))
  )

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
;;;; also contains lexical pieces of a parser for Matlab,
;;(c) copyright 1990, 1999 by Richard J. Fateman

(eval-when (compile)(declaim (optimize (speed 3)(safety 0))))

(defvar matbuffer nil) 
(defvar stream t) ;; if needed

;; The first section consists of readtable hacking 
;; We use lisp atoms to store information on tokens.

(defvar matrt (copy-readtable nil))
(setf (readtable-case matrt) :preserve) ;preserve the case of symbols read in

;; I don't like the following choice, but the common lisp
;; standards committee majority make it very difficult
;; to have both cases work. Allegro CL (modern) and SBCL seem to do the right thing now
;;(setf (readtable-case matrt) :downcase)
;; debugging flags 
 
(setq *print-level* nil *print-length* nil *print-pretty* t)

(defun pc()(peek-char nil stream nil #\newline))
(defparameter *charct* 0);how many chars have been read? in case we need it
(defun rc();(incf *charct*)
       (read-char stream) )

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
	((digit-char-p (pc) r)	;r is radix. for us, 5+5.
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

(defun mattop(x) (get x 'matop))

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


(set-macro-character #\: 
  #'(lambda(stream char)
      (declare (ignore char))
      (case (pc)
		 (#\newline '|:|)
		 (#\= (rc)  '|:=|)
		 (t '|:|))) nil matrt)

 
(set-macro-character #\newline #'(lambda(stream char)  (declare (ignore char))
					'e-o-l) nil matrt)

(defun makemt(x)(setf (get x 'matop) t))

(makemt '\.)
(makemt '\;)
(mapc #'makemt '(= * ^	&  +  -  [  ]   > >=  < <= ! ~ ~= / ))
(mapc #'makemt '( |(| |)|))

(makemt '|\\|)
(makemt '|'|)
(makemt '|,|)

 
;;; end of the lexical analysis part
;; putting pieces together

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
  (cond ((null (symbolp s))nil) ; numbers perhaps
	(t(setf s (smd(symbol-name s)))))) ;  s= |xcoshx| ==> "xcoshx"
;; this program also breaks apart ==  into = =  etc.

(defun smd(s)
  (cond ((= 0 (length s))nil)
	((eq s 'E-O-L) nil)
	(t(multiple-value-bind
	      (first rest)
	      (breakup s oplist)
	    (cons (if (stringp first)(read-from-string first) first)
		  (smd rest))))))
(defun isopsym(r)(if (symbolp r)(get r 'mathop) nil))
(defun isvarsym(r)(if (symbolp r)(get r 'mathvar) nil))

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
      (collect-number  (char-to-int(read-char stream)) 10) ;radix
	  )
	(t (intern
	    (or(read-preserving-whitespace stream nil 'e-o-l)
	       'False)
	    
	    :amlparser)  ;;;;;;; WARNING. TRICKY...
	   ;; nil reads as False
	   )))
;; parts of the floating-point reader
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

(defun peek-token() (cond(matbuffer)
			 (t (setq matbuffer(mread1)))))

;; Here is where Norvig's parser code begins

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
(defun defines(x def y)(if (listp x) `(defun ,(car x),(cdr x) ,y)
			 `(setf ,x ,y)))
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



#|examples
'$ q(a,b,c):b^2-4ac  
(defun q (a b c) (- (expt b 2) (* (* 4 a) c)))

;;(newvar "pi") ;; the double-float for pi is part of CL.
$pi					;works
$cos pi					;works. -1.0d0
'$a=sin(c)^c ;; is this ambiguous??
I found 2 parses are possible, (= a (expt (sin c) c)) --or--  (= a (sin (expt c c))) 
 but
a=sin c^c
is not
;; b(c)^c  could be b * c^c  or (b(c))^c  or b (c^c) if we don't know if b is fun or var.

|#

(use *ambigmath*)







