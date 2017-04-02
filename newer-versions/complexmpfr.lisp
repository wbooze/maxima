;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: mpfr; Base: 10 -*- 
;; Author: Richard Fateman, Jan, 2006
;; last edit, April 17, 2006
;; look at mpfr.lisp

(in-package :mpfr)
;; try to do stuff with pairs of reals.  So a polynomial is now
;; complex coefficients   (3+4i)*x + 5+6i)  looks like
;; (3 4 5 6);



;; multiply a+bi  * c+di  --> ac-bd  +  (ad+bc) i
;; not the numerically most stable forms.


(defmacro rp*(a b c d)			;real part of product
  `(- (* ,a ,c)(* ,b ,d)))

(defmacro ip*(a b c d)			;imag part of product
  `(+ (* ,a ,d)(* ,b ,c)))

(defun rp/(a b c d)
  (/ (+ (* b d) (* a c)) (+ (* d d)(* c c))))


(defun ip/(a b c d)
  (/ (- (* b c) (* a d)) (+ (* d d)(* c c))))




(defun onecroot (f df xr xi threshold iters &aux denom fr fi)
  (dotimes  (i iters 
	      (error "complex rootfinder failed to converge. 
Residual at ~s+~si is ~s+~si after ~s iterations." 
			    xr xi  fr fi i))
    (multiple-value-bind
	(gr gi)
	(funcall f xr xi)
      (setf fr gr fi gi)
    (if (< (cabs fr fi) threshold) 
	(return (values xr xi  fr fi i))	;return x, residual and iteration count
      (multiple-value-bind
	  (dr di)
	  (funcall df xr xi)
      ;;  (decf xr (rp/ fr fi dr di))
      ;;  (decf xi (ip/ fr fi dr di))
	(setf denom (+ (* dr dr)(* di di))) ; what if denom = 0 ?
	(decf xr (/ (ip* fr fi di dr) denom))
	(decf xi (/ (rp* fr fi (- di) (- dr)) denom)))))))
      

(defun cderiv(coefs)			;given coefs of a complex polynomial, return coefs of derivative.
  (let ((ans nil))
    (do
	((c (cddr (reverse coefs)) (cddr c))
	 (i 1 (1+ i)))
	((null c) ans)
      (push (* i (car c)) ans)
      (push (* i (cadr c)) ans))))

;; [slow, generic] COMPLEX version of horner's rule.
;; compare to version polyeval in mpfr.lisp file
;; works, 4/17/06. Don't mess with this copy.
(defun polycceval (calist zreal zimag)  ;; complex numbers in alist.
  (let ((sumr (into 0))
	(sumi (into 0))
	(tr (into 0)); temps
	(ti (into 0)))
    (do* ((thelist calist (cddr thelist))
	  (areal (car thelist)(car thelist))
	  (aimag (cadr thelist)(cadr thelist)))
	((null thelist) (values sumr sumi))
      ;;      (format t "~%r=~s i=~s" areal aimag)
      (setf tr (rp* sumr sumi zreal zimag))
      (setf ti (ip* sumr sumi zreal zimag))
      ;;     (format t "~%tr=~s ti=~s" tr ti)

      (setf sumr (+ areal tr))
      (setf sumi (+ aimag ti)))))


;; perhaps default threshold should be a function of precision 

;; coefs is a list of real-complex-real-complex...
(defun cpolyroot (coefs zr zi &optional (threshold 1.0d-15) (iters 20))
  (setf coefs (mapcar #'into coefs))
  (let ((dp (cderiv coefs)))
    (onecroot  #'(lambda(zr zi)(polycceval coefs zr zi))
	      #'(lambda(zr zi)(polycceval dp zr zi))
	      (into zr)
	      (into zi)
	      (into threshold)
	      iters)))

(defun cabs(zr zi)(sqrt(+ (* zr zr)(* zi zi))))

(defun cabs2(zr zi)(+ (* zr zr)(* zi zi)))

;; some reading material among the google stuff from 'complex newton convergence polynomial'
;; http://facstaff.unca.edu/mcmcclur/professional/NewtonsMethodPP.pdf


;; (cpolyroot '(1 0 0 0 1 0) 0 0.5d0)  ;; x^2+1, start at 0.5i
;; (cpolyroot '(1 0 0 0 1 0) 0 -0.5d0)  ;; x^2+1, start at -0.5i
;; (cpolyroot '(1 0 0 0 -1 0) 0 -0.5d0)  ;; x^2-1, start at 0.5i; no convergence
;; (cpolyroot '(1 0 0 0 -1 0) .2d0 -0.5d0)  ;; x^2-1, start at .2+0.5i; converges
;; (cpolyroot '(1 0 0 0 -1 0) 0.5d0 0.0)  ;; x^2-1, start at 0.5
;; (cpolyroot '(1 0 0 0 -1 0) 0.2d0 0.0)  ;; x^2-1, start at 0.5
;; (cpolyroot '(1 0 0 0 -1 0) 1.0d-30 0.0)  ;; x^2-1, start at 0.5 ;no




;;;; the mpfr version, evaluate complex poly at complex point

(defun mpfrpolycceval (calist zreal zimag)  ;; complex numbers, each mpfr in alist.
  (let ((sumr (into 0))
	(sumi (into 0))
	(ti (into 0)))
    (setq zreal(into zreal) zimag (into zimag)) ;make sure mpfr
    (setq calist (mapcar #'into calist))
    (do* ((thelist calist (cddr thelist))
	  (areal (car thelist)(car thelist))
	  (aimag (cadr thelist)(cadr thelist)))
	((null thelist) (values sumr sumi))
   ;;   (format t "~% sumr=~s sumi=~s" sumr sumi)
      (dsetv ti (ip* sumr sumi zreal zimag))
      (dsetv sumr (+ areal (rp* sumr sumi zreal zimag)))
      (dsetv sumi (+ aimag ti )))))


;; polycoefs are real, point is complex  (mpfrpolyceval '(1 0 1) 10 20); --> -299 + 400i
(defun mpfrpolyceval (calist zreal zimag)  ;; real numbers, each mpfr in alist.
  (let ((sumr (into 0))
	(sumi (into 0))
	(tt (into 0)))
    (setq zreal(into zreal) zimag (into zimag)) ;make sure mpfr
    (setq calist (mapcar #'into calist))
    (do* ((thelist calist (cdr thelist))
	  (areal (car thelist)(car thelist)))
	((null thelist) (values sumr sumi))
      (dsetv tt (ip* sumr sumi zreal zimag))
      (dsetv sumr (+ areal (rp* sumr sumi zreal zimag)))  ;;    areal+ realpart[(sumr+i*sumi)*(zr+i*zi)]
      (dsetv sumi tt)))) 

