;;; -*- Mode:Common-Lisp; Package:ari; Base:10 -*-

;; Affine Real Interval (ARI arithmetic)
;; Based on ninterval.lisp in this directory,
;; See ../../papers/interval.tex for info on AAI.

(defpackage :ari			;uses generic arithmetic for endpoints
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
    (:shadowing-import-from 
     :cl
     union intersection)
  (:export ari ri union intersection))

(eval-when (compile eval load)(require "ga") (provide "ari") (in-package :ari)

(in-package :ari)
(eval-when (compile eval load)	   
  
(defstruct ri mid exp))		;structure for affine real interval

(defun ri(lo hi &key  exp)
 ;; (format t "~% lo=~s, hi=~s, index=~s exp=~s" lo hi index exp)
  (cond ((< hi lo)(error "attempt to create invalid affine real interval [~s, ~s]" lo hi))
	(t (let* ((mid (/ (+ lo hi) 2))
		 (del (- hi mid)))
	     (setf exp (cons (cons (gensym) del) nil))
	     (make-ri :mid (/ (+ lo hi) 2):exp exp)))))

;;or, widen interval.

(defun wri(lo hi &key  exp)
  (cond ((< hi lo)(error "attempt to create invalid affine real interval [~s, ~s]" lo hi))
	(t (let* ((mid (/ (+ lo hi) 2))
		 (del (ri::bu (- hi mid)))) ; bump up.
	     (setf exp (cons (cons (gensym) del) nil))
	     (make-ri :mid (/ (+ lo hi) 2):exp exp)))))

(defun into(lo hi)(ri lo hi))  ;; standard name would be ari::into

(defmethod print-object ((a ri) stream)
  (format stream "[~a~a~a]"  (ri-mid a) #.(code-char #x00b1) ;; unicode +-
	  (reduce #'cl::+ (mapcar #'(lambda(q)(abs(cdr q))) (ri-exp a))))
  )

#+ignore ;; alternative
(defmethod print-object ((a ri) stream)
  (let ((delta (reduce #'cl::+ (mapcar #'(lambda(q)(abs(cdr q))) (ri-exp a)))))
    (if (> delta 0)
	(format stream "[~a,~a]"  (- (ri-mid a) delta)(+ (ri-mid a) delta ))
      (format stream "[~a,~a]"  (+ (ri-mid a) delta ) (- (ri-mid a) delta)))))


(defun mkstr (&rest args)
  (with-output-to-string (s)(dolist (a args) (princ a s))))

(defun symb (&rest args) (values (intern (apply #'mkstr args))))

(defmacro with-ri2 (struct1 struct2 names1 names2 &body body)
  (let ((gs1 (gensym))
	(gs2 (gensym)))
    `(let ((,gs1 ,struct1)
	   (,gs2 ,struct2))
       (let ,(append 
	      (mapcar #'(lambda (f field)
			  `(,f (,(symb "ri-" field) ,gs1)))
		      names1
		      '(mid  exp))
	      (mapcar #'(lambda (f field)
			  `(,f (,(symb "ri-" field) ,gs2)))
		      names2
		      '(mid exp)))
	 ,@body))))

(defmacro with-ri (struct1 names1  &body body)
  (let ((gs1 (gensym)))
    `(let ((,gs1 ,struct1))
       (let  ,(mapcar #'(lambda (f field)
			  `(,f (,(symb "ri-" field) ,gs1)))
		      names1
		      '(mid  exp))
	 ,@body))))

)					; end eval-when

(defmethod ga::two-arg-+ (r (s ari::ri)) ;adding num+interval
  (with-ri s (mid exp) 
	   (make-ri :mid  (+ r mid) :exp exp)))

(defmethod ga::two-arg-+ ((s ari::ri) r) ;adding interval + num
  (+ r s))

(defmethod ga::two-arg-- (r (s ari::ri)) 
  (with-ri s (mid exp)
	   (make-ri :mid  (- r mid) :exp exp)))

(defmethod ga::two-arg-- ( (s ari::ri) r) 
  (with-ri s (mid exp) 
	   (make-ri :mid  (- mid r) :exp exp)))

(defmethod ga::two-arg-- ((ri1 ri)(ri2 ri))
  (+ ri1 (negate ri2)))

(defmethod negate ((r ri))
  (make-ri :mid (- (ri-mid r))
	   :exp (mapcar #'(lambda(h)(cons (car h)(- (cdr h)))) (ri-exp r))))

(defmethod ga::two-arg-* ((s ari::ri) r) ;multiply interval * num
      (with-ri s (mid exp)
	       (make-ri :mid (* r mid)
			:exp (mapcar #'(lambda(q)(cons (car q)(* r (cdr q)))) exp))))
    

(defmethod ga::two-arg-* ( r(s ari::ri)) (ga::two-arg-* s r)) ;reverse the args

(defvar intindex 0)

;;new
(defmethod ga::two-arg-+ ((ri1 ri)(ri2 ri))
  (with-ri2 ri1 ri2 (mid1 exp1)(mid2 exp2)
	    (let ((m (+ mid1 mid2))
		  ;; implement a kind of sort-merge
		  (e (mapcar #'copy-list exp1)))
	      (loop for q in exp2 do 
		    (let ((h (assoc (car q) e)))
		    ;  (format t "~%q,h=~s,~s" q h)
		      (if (null h)(setf e (cons q e))
			(let ((sum (+ (cdr h)(cdr q))))
			  (if (= sum 0) (setf e (delete h e))
			       (setf (cdr h) sum))))))
	  ;;    (describe e)
	      (make-ri :mid m :exp e))))
	      

;;#+ignore
(defmethod ga::two-arg-* ((r ri)(s ri))
  ;; a crude version.
  
  ;; (x0 + a1*e1 + ... +an*en) *( y0 + b1*e1 + ... +bm*em)
  ;; let A=|a1|+...+|an|
  ;; let B=|b1|+...+|bm|
  ;; then a bound could be  (x0+ A*e_xx)*(y0+B*e_yy)
  ;; x0*y0+ (x0*B*e_yy)+(y0*A*e_xx)+ AB*e_zz

  ;; so we bound the result by
  ;; --> (x0*y0 + (A*y0+B*x0+A*B)e_new.

  (with-ri2 r s (mid1 exp1)(mid2 exp2)
	    (let* ((m (* mid1 mid2))
		  (A  (reduce #'cl::+ (mapcar #'(lambda(q)(abs(cdr q)))
					      exp1)))
		  (B  (reduce #'cl::+ (mapcar #'(lambda(q)(abs(cdr q)))
					      exp2)))   )
	      (make-ri :mid m 
		       :exp `( (,(gensym) .  ,(+ (abs(* mid1 B))(abs(* mid2 A))(* A B))))))))

;; this works, but by translating to conventional interval arithmetic.
;;not doing anything clever with dependencies
#+ignore
(defmethod ga::two-arg-* ((r ri)(s ri))
  (with-ri2 r s (mid1 exp1)(mid2 exp2)
	    (let* ((A  (reduce #'cl::+ (mapcar #'(lambda(q)(abs(cdr q)))
					      exp1)))
		   (B  (reduce #'cl::+ (mapcar #'(lambda(q)(abs(cdr q)))
					       exp2)))
		   (lo1 (- mid1 A))
		   (hi1 (+ mid1 A))
		   (lo2 (- mid2 B))
		   (hi2 (+ mid2 B))		 )
		   (if (and (> lo1 0)(> lo2 0))
		       (ri (* lo1 lo2)(* hi1 hi2))
		     (let ((all (list (* lo1 lo2)(* lo1 hi2)
				      (* hi1 lo2)(* hi1 hi2))))
		  ;;     (format t "~%A=~s, B=~s, all=" A B all)
		       (ri (apply #'cl::min all)(apply #'cl::max all)))))))

;; I think this is right, but not necessarily as tight as regular intervals sometimes.
;; Sometimes it is tighter, as shown by Stolfi.
(defmethod ga::two-arg-* ((r ri)(s ri))
  ;; maybe we can be tighter here...
  
  ;; since ei*ej is also in [-1,1], and ei*ei is in [0,1], we can do
  ;; this..  partial terms like a1*e1 * b1*e1 ==> (a1*b1)[0,1] ==> add a new
  ;; item with mid=(a1*b1)/2, del= (a1*b1)/2.  We add to the middle
  ;; term, a1*b1/2, and introduce a variable (a1*b1)/2*e_new.
  ;; otherwise, accumulate A and B as above, if terms don't match.
  ;;
  ;;   a1*b2 --> (a1*b1)*e_new

  (with-ri2 r s (mid1 exp1)(mid2 exp2)
	    (let ((m  (* (- mid1) mid2))
		  (e nil)
		  (e1 0)
		  (v1 0)
		  (leftover 0))
	      (loop for p in exp1 do	; compute quadratic terms
		    (setf e1 (car p) v1 (cdr p))
		    (loop for q in exp2 do 
			  (let ((e2 (car q))
				(v2 (cdr q)))
;			    (format t "~%v1=~s, v2=~s" v1 v2)
			  (cond ((eq e1 e2)
				 (let ((vv (* 1/2 (* v1 v2))))
;				   (format t "~%vv=~s, m=~s" vv m)
				     (setf e `( (,(gensym). ,vv),@ e))
				     (setf m (+ m vv))))
				(t
				 (incf leftover (abs (* v1 v2)))))
			  )))
  (unless (= leftover 0)     (setf e `( (,(gensym). ,leftover),@ e)))
  (+ ;; finally, add these intervals
   (make-ri :mid m :exp e)
   (* mid1 s)				;linear terms
   (* mid2 r)				;linear terms
   ))));; works for squaring (ri -1/2 1/2).  not (ri 1 3)

#|

[1,3] =   2 +- 1.  to square it, compute
m= -4
e1=xx
v1=1
e2=xx
v2=1
vv=1/2
e = (( yy . 1/2))
m = -4+1/2 = -7/2
leftover=0
(* mid1 s) = (* mid2 r) = 2* [1,3] = [2,6] or 4+-1
add these two together to make [4,12] or  8 +-4
final result is
mid = -7/2 + 8 = -7/2+16/2 = 9/2
exp = ((yy . 1/2) (zz . 4))
By this computation,
low end should be 9/2 -1/2 -4 =0.
hi end should be 9/2+1/2+4 = 9.
A tighter bound would be  [1,9].
Why can't we make it better??
We know that when zz =-1, yy=1, so low end would be 9/2+1/2-4 = 1.
But the representation has lost that information.


|#
;;hm.. from text on stolfi's site, he suggests
;;  x_0*y_0 + sum( x_0*y_i+y_0*x_i)*e_i  +rad(X)*rad(Y)e_new.
;; rad is the half-width.  let's try it.

;; program below has bug? or maybe almost as much work and not as tight as above.
#+ignore 
(defmethod ga::two-arg-* ((r ri)(s ri))
  (with-ri2 r s (mid1 exp1)(mid2 exp2)
	    (let ((m  (*  mid1 mid2))
		  (e nil)
		  (e1 0)
		  (v1 0)
		  (rad1 (reduce #'cl::+ (mapcar #'(lambda(q)(abs(cdr q)))
					       exp1)))
		  (rad2 (reduce #'cl::+ (mapcar #'(lambda(q)(abs(cdr q)))
					       exp2)))
		  (mv 0))
	      (loop for p in exp1 do 
		    (setf e1 (car p) v1 (cdr p) mv (* v1 mid2))
		  ;;  (setf e `((,e1 . ,(* v1 mid2)),@ e))
		    (loop for q in exp2 do 
			  (let ((e2 (car q))
				(v2 (cdr q)))

			  ;  (format t "~%v1=~s, v2=~s" v1 v2)
			  (cond ((eq e1 e2)
				 (let ((vv (+ (* mid1 v2) mv)))
				   (format t "~%vv=~s, m=~s" vv m)
				   (setf e `( (,e1 . ,vv),@ e)) ))))))
	      	   (format t "~%rad1=~s, rad2=~s" rad1 rad2)

	      (setf e `( (,(gensym) . ,(* rad1 rad2
					  )),@ e))
		  (make-ri :mid m :exp e)
		  )))

(defmethod ga::two-arg-expt ((s ri) (r fixnum)) 
;; can we do better than this?  
  (with-ri s (mid exp)
	   (let* ((del (reduce #'cl::+ (mapcar #'cdr exp)))
		  (lo (- mid del))
		  (hi (+ mid del)))
	       (if (and (evenp r)(<= lo 0 hi))
		   (ri 0 (expt (ga::two-arg-max (abs lo) (abs hi)) r))

		 (let ((e1 (expt lo r))
		       (e2 (expt hi r)))
		   (if (< e1 e2)(ri e1 e2 )
		     (ri e2 e1)))))))

;; make an affine into ordinary interval.
(defmethod aa2ia((a ri))
  (let ((delta (reduce #'cl::+ (mapcar #'(lambda(q)(abs(cdr q))) (ri-exp a)))))
    (if (> delta 0)
	(ri::into (- (ri-mid a) delta)(+ (ri-mid a) delta )) (ri-mid a))))
(defmethod aa2ia(b) b)

;; make an ordinary interval into an affine interval

(defmethod ia2aa((a ri::ri))
  (ari::ri (ri::ri-lo a)(ri::ri-hi a)))
(defmethod ia2aa(b) b)

(defmethod ga::invert((k ri))
  (ia2aa(ri::invert (aa2ia k))))

(defmethod ga::two-arg-/((n ri) d)  (* (/ 1 d) n))
(defmethod ga::two-arg-/(n (d ri))  (* (ga::invert d) n))
;;(defmethod ga::two-arg-/((n ri) (d ri))  (ia2aa (/ (aa2ia n)(aa2ia d))))

;; Every other method for IA may be transferred to AA by this technique,
;; though there may be other ways that involve less calculation or shorter program
;; as the version below.

(defmethod ga::two-arg-/((n ri) (d ri)) (* (ga::invert d) n))







