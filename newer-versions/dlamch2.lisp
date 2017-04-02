(in-package :qd)
 ;;; Compiled by f2cl version 2.0 beta Date: 2005/06/01 15:29:41 
;;; Using Lisp International Allegro CL Enterprise Edition 7.0 [Windows] (Jan 30, 2006 8:40)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t)
;;;           (:relaxed-array-decls t) (:coerce-assigns :as-needed)
;;;           (:array-type ':simple-array) (:array-slicing t)
;;;           (:declare-common nil) (:float-format single-float))


;;; hand-edited to remove declarations and to change constants


(let* ((one (into 1)) (zero (into 0)))
  (let ((eps (into 0))
        (sfmin (into 0))
        (base (into 0))
        (t$ (into 0))
        (rnd (into 0))
        (emin (into 0))
        (rmin (into 0))
        (emax (into 0))
        (rmax (into 0))
        (prec (into 0))
        (first$ nil))

    (setq first$ f2cl-lib:%true%)
    (defun dlamch (cmach)

      (prog ((rmach (into 0)) (small (into 0)) (t$ (into 0)) (beta 0) (imax 0)
             (imin 0) (it 0) (lrnd nil) (dlamch (into 0)))

            (cond (first$
                   (setf first$ f2cl-lib:%false%)
                   (multiple-value-bind (var-0 var-1 var-2 var-3 var-4
                                         var-5 var-6 var-7)
                       (dlamc2 beta it lrnd eps imin rmin imax rmax)
                     (when var-0 (setf beta var-0))
                     (when var-1 (setf it var-1))
                     (when var-2 (setf lrnd var-2))
                     (when var-3 (setf eps var-3))
                     (when var-4 (setf imin var-4))
                     (when var-5 (setf rmin var-5))
                     (when var-6 (setf imax var-6))
                     (when var-7 (setf rmax var-7)))
                   (setf base (into beta))
                   (setf t$ (into it))
                   (cond (lrnd
                          (setf rnd one)
                          (setf eps
                                (/ (expt base (f2cl-lib:int-sub 1 it))
                                   2)))
                         (t
                          (setf rnd zero)
                          (setf eps
                                (expt base (f2cl-lib:int-sub 1 it)))))
                   (setf prec (* eps base))
                   (setf emin (into imin))
                   (setf emax (into imax))
                   (setf sfmin rmin)
                   (setf small (/ one rmax))
                   (cond ((>= small sfmin)
                          (setf sfmin (* small (+ one eps)))))))
            (cond ((multiple-value-bind (ret-val var-0 var-1)
                       (lsame cmach "E")

                     (when var-0 (setf cmach var-0))
                     ret-val)
                   (setf rmach eps))
                  ((multiple-value-bind (ret-val var-0 var-1)
                       (lsame cmach "S")

                     (when var-0 (setf cmach var-0))
                     ret-val)
                   (setf rmach sfmin))
                  ((multiple-value-bind (ret-val var-0 var-1)
                       (lsame cmach "B")
                     (when var-0 (setf cmach var-0))
                     ret-val)
                   (setf rmach base))
                  ((multiple-value-bind (ret-val var-0 var-1)
                       (lsame cmach "P")

                     (when var-0 (setf cmach var-0))
                     ret-val)
                   (setf rmach prec))
                  ((multiple-value-bind (ret-val var-0 var-1)
                       (lsame cmach "N")

                     (when var-0 (setf cmach var-0))
                     ret-val)
                   (setf rmach t$))
                  ((multiple-value-bind (ret-val var-0 var-1)
                       (lsame cmach "R")

                     (when var-0 (setf cmach var-0))
                     ret-val)
                   (setf rmach rnd))
                  ((multiple-value-bind (ret-val var-0 var-1)
                       (lsame cmach "M")

                     (when var-0 (setf cmach var-0))
                     ret-val)
                   (setf rmach emin))
                  ((multiple-value-bind (ret-val var-0 var-1)
                       (lsame cmach "U")

                     (when var-0 (setf cmach var-0))
                     ret-val)
                   (setf rmach rmin))
                  ((multiple-value-bind (ret-val var-0 var-1)
                       (lsame cmach "L")

                     (when var-0 (setf cmach var-0))
                     ret-val)
                   (setf rmach emax))
                  ((multiple-value-bind (ret-val var-0 var-1)
                       (lsame cmach "O")

                     (when var-0 (setf cmach var-0))
                     ret-val)
                   (setf rmach rmax)))
            (setf dlamch rmach)
            (go end_label)
       end_label (return (values dlamch cmach))))))

;;; Compiled by f2cl version 2.0 beta Date: 2005/06/01 15:29:41 
;;; Using Lisp International Allegro CL Enterprise Edition 7.0 [Windows] (Jan 30, 2006 8:40)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t)
;;;           (:relaxed-array-decls t) (:coerce-assigns :as-needed)
;;;           (:array-type ':simple-array) (:array-slicing t)
;;;           (:declare-common nil) (:float-format single-float))

(let ((lieee1 nil) (lbeta 0) (lrnd nil) (f2cl-lib:lt 0) (first$ nil))

  (setq first$ f2cl-lib:%true%)
 (defun dlamc1 (beta t$ rnd ieee1)

    (prog ((a (into 0)) (b (into 0)) (c (into 0)) (f (into 0)) (one (into 0))
           (qtr (into 0)) (savec (into 0)) (t1 (into 0)) (t2 (into 0)))

          (cond (first$
                 (tagbody
                     (setf first$ f2cl-lib:%false%)
                     (setf one(into 1))
                     (setf a (into 1))
                     (setf c (into 1))
                   label10
                     (cond ((= c one)
                            (setf a (* 2 a))
                            (setf c
                                  (multiple-value-bind (ret-val var-0
                                                        var-1)
                                      (dlamc3 a one)

                                    (when var-0 (setf a var-0))
                                    (when var-1 (setf one var-1))
                                    ret-val))
                            (setf c
                                  (multiple-value-bind (ret-val var-0
                                                        var-1)
                                      (dlamc3 c (- a))

                                    (when var-0 (setf c var-0))
                                    ret-val))
                            (go label10)))
                     (setf b (into 1))
                     (setf c
                           (multiple-value-bind (ret-val var-0 var-1)
                               (dlamc3 a b)

                             (when var-0 (setf a var-0))
                             (when var-1 (setf b var-1))
                             ret-val))
                   label20
                     (cond ((= c a)
                            (setf b (* 2 b))
                            (setf c
                                  (multiple-value-bind (ret-val var-0
                                                        var-1)
                                      (dlamc3 a b)

                                    (when var-0 (setf a var-0))
                                    (when var-1 (setf b var-1))
                                    ret-val))
                            (go label20)))
                     (setf qtr (/ one 4))
                     (setf savec c)
                     (setf c
                           (multiple-value-bind (ret-val var-0 var-1)
                               (dlamc3 c (- a))

                             (when var-0 (setf c var-0))
                             ret-val))
                     (setf lbeta (f2cl-lib:int (+ c qtr)))
                     (setf b (into lbeta))
                     (setf f (dlamc3 (/ b 2) (/ (- b) 100)))
                     (setf c
                           (multiple-value-bind (ret-val var-0 var-1)
                               (dlamc3 f a)

                             (when var-0 (setf f var-0))
                             (when var-1 (setf a var-1))
                             ret-val))
                     (cond ((= c a) (setf lrnd f2cl-lib:%true%))
                           (t (setf lrnd f2cl-lib:%false%)))
                     (setf f (dlamc3 (/ b 2) (/ b 100)))
                     (setf c
                           (multiple-value-bind (ret-val var-0 var-1)
                               (dlamc3 f a)

                             (when var-0 (setf f var-0))
                             (when var-1 (setf a var-1))
                             ret-val))
                     (if (and lrnd (= c a))
                         (setf lrnd f2cl-lib:%false%))
                     (setf t1
                           (multiple-value-bind (ret-val var-0 var-1)
                               (dlamc3 (/ b 2) a)

                             (when var-1 (setf a var-1))
                             ret-val))
                     (setf t2
                           (multiple-value-bind (ret-val var-0 var-1)
                               (dlamc3 (/ b 2) savec)

                             (when var-1 (setf savec var-1))
                             ret-val))
                     (setf lieee1 (and (= t1 a) (> t2 savec) lrnd))
                     (setf f2cl-lib:lt 0)
                     (setf a (into 1))
		   (setf c(into 1))
		  label30
                     (cond ((= c one)
                            (setf f2cl-lib:lt
                                  (f2cl-lib:int-add f2cl-lib:lt 1))
                            (setf a (* a lbeta))
                            (setf c
                                  (multiple-value-bind (ret-val var-0
                                                        var-1)
                                      (dlamc3 a one)


                                    (when var-0 (setf a var-0))
                                    (when var-1 (setf one var-1))
                                    ret-val))
                            (setf c
                                  (multiple-value-bind (ret-val var-0
                                                        var-1)
                                      (dlamc3 c (- a))

                                    (when var-0 (setf c var-0))
                                    ret-val))
                            (go label30))))))
          (setf beta lbeta)
          (setf t$ f2cl-lib:lt)
          (setf rnd lrnd)
          (setf ieee1 lieee1)
          (go end_label)
     end_label (return (values beta t$ rnd ieee1)))))

;;; Compiled by f2cl version 2.0 beta Date: 2005/06/01 15:29:41 
;;; Using Lisp International Allegro CL Enterprise Edition 7.0 [Windows] (Jan 30, 2006 8:40)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t)
;;;           (:relaxed-array-decls t) (:coerce-assigns :as-needed)
;;;           (:array-type ':simple-array) (:array-slicing t)
;;;           (:declare-common nil) (:float-format single-float))

(let ((lbeta 0)
      (lemax 0)
      (lemin 0)
      (leps (into 0))
      (lrmax (into 0))
      (lrmin (into 0))
      (f2cl-lib:lt 0)
      (first$ nil)
      (iwarn nil))

  (setq first$ f2cl-lib:%true%)
  (setq iwarn f2cl-lib:%false%)
  (defun dlamc2 (beta t$ rnd eps emin rmin emax rmax)

    (prog ((a (into 0)) (b (into 0)) (c (into 0)) (half (into 0)) (one (into 0))
           (rbase (into 0)) (sixth$ (into 0)) (small (into 0)) (third$ (into 0))
           (two (into 0)) (zero (into 0)) (gnmin 0) (gpmin 0) (i 0)
           (ngnmin 0) (ngpmin 0) (ieee nil) (lieee1 nil) (lrnd nil)
           (abs$ (into 0)))
  
          (cond (first$
                 (tagbody
                     (setf first$ f2cl-lib:%false%)
                     (setf zero (into 0))
                     (setf one (into 1))
                     (setf two (into 2))
                     (multiple-value-bind (var-0 var-1 var-2 var-3)
                         (dlamc1 lbeta f2cl-lib:lt lrnd lieee1)
                       (setf lbeta var-0)
                       (setf f2cl-lib:lt var-1)
                       (setf lrnd var-2)
                       (setf lieee1 var-3))
                     (setf b (into lbeta))
                     (setf a (expt b (f2cl-lib:int-sub f2cl-lib:lt)))
                     (setf leps a)
                     (setf b (/ two 3))
                     (setf half (/ one 2))
                     (setf sixth$
                           (multiple-value-bind (ret-val var-0 var-1)
                               (dlamc3 b (- half))
                             (when var-0 (setf b var-0))
                             ret-val))
                     (setf third$
                           (multiple-value-bind (ret-val var-0 var-1)
                               (dlamc3 sixth$ sixth$)
                             (when var-0 (setf sixth$ var-0))
                             (when var-1 (setf sixth$ var-1))
                             ret-val))
                     (setf b
                           (multiple-value-bind (ret-val var-0 var-1)
                               (dlamc3 third$ (- half))
                             (when var-0 (setf third$ var-0))
                             ret-val))
                     (setf b
                           (multiple-value-bind (ret-val var-0 var-1)
                               (dlamc3 b sixth$)
                             (when var-0 (setf b var-0))
                             (when var-1 (setf sixth$ var-1))
                             ret-val))
                     (setf b (into (abs b)))
                     (if (< b leps) (setf b leps))
                     (setf leps
                         (into 1))
                   label10
                     (cond ((and (> leps b) (> b zero))
                            (setf leps b)
                            (setf c
                                  (dlamc3 (* half leps)
                                   (* (expt two 5) (expt leps 2))))
                            (setf c
                                  (multiple-value-bind (ret-val var-0
                                                        var-1)
                                      (dlamc3 half (- c))
                                    (when var-0 (setf half var-0))
                                    ret-val))
                            (setf b
                                  (multiple-value-bind (ret-val var-0
                                                        var-1)
                                      (dlamc3 half c)
                                    (when var-0 (setf half var-0))
                                    (when var-1 (setf c var-1))
                                    ret-val))
                            (setf c
                                  (multiple-value-bind (ret-val var-0
                                                        var-1)
                                      (dlamc3 half (- b))
                                    (when var-0 (setf half var-0))
                                    ret-val))
                            (setf b
                                  (multiple-value-bind (ret-val var-0
                                                        var-1)
                                      (dlamc3 half c)
                                    (when var-0 (setf half var-0))
                                    (when var-1 (setf c var-1))
                                    ret-val))
                            (go label10)))
                     (if (< a leps) (setf leps a))
                     (setf rbase (/ one lbeta))
                     (setf small one)
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i 3) nil)
                                   (tagbody
                                       (setf small
                                             (multiple-value-bind (ret-val
                                                                   var-0
                                                                   var-1)
                                                 (dlamc3
                                                  (* small rbase) zero)

                                               (when var-1
                                                 (setf zero var-1))
                                               ret-val))
                                     label20))
                     (setf a
                           (multiple-value-bind (ret-val var-0 var-1)
                               (dlamc3 one small)

                             (when var-0 (setf one var-0))
                             (when var-1 (setf small var-1))
                             ret-val))
                     (multiple-value-bind (var-0 var-1 var-2)
                         (dlamc4 ngpmin one lbeta)

                       (when var-0 (setf ngpmin var-0))
                       (when var-1 (setf one var-1))
                       (when var-2 (setf lbeta var-2)))
                     (multiple-value-bind (var-0 var-1 var-2)
                         (dlamc4 ngnmin (- one) lbeta)

                       (when var-0 (setf ngnmin var-0))
                       (when var-2 (setf lbeta var-2)))
                     (multiple-value-bind (var-0 var-1 var-2)
                         (dlamc4 gpmin a lbeta)

                       (when var-0 (setf gpmin var-0))
                       (when var-1 (setf a var-1))
                       (when var-2 (setf lbeta var-2)))
                     (multiple-value-bind (var-0 var-1 var-2)
                         (dlamc4 gnmin (- a) lbeta)

                       (when var-0 (setf gnmin var-0))
                       (when var-2 (setf lbeta var-2)))
                     (setf ieee f2cl-lib:%false%)
                     (cond ((and (= ngpmin ngnmin) (= gpmin gnmin))
                            (cond ((= ngpmin gpmin)
                                   (setf lemin ngpmin))
                                  ((= (f2cl-lib:int-add gpmin
                                                        (f2cl-lib:int-sub ngpmin))
                                      3)
                                   (setf lemin
                                         (f2cl-lib:int-add (f2cl-lib:int-sub ngpmin
                                                                             1)
                                                           f2cl-lib:lt))
                                   (setf ieee f2cl-lib:%true%))
                                  (t
                                   (setf lemin
                                         (min (the f2cl-lib:integer4
                                                   ngpmin)
                                              (the f2cl-lib:integer4
                                                   gpmin)))
                                   (setf iwarn f2cl-lib:%true%))))
                           ((and (= ngpmin gpmin) (= ngnmin gnmin))
                            (cond ((= (abs (f2cl-lib:int-add ngpmin
                                                             (f2cl-lib:int-sub ngnmin)))
                                      1)
                                   (setf lemin
                                         (max (the f2cl-lib:integer4
                                                   ngpmin)
                                              (the f2cl-lib:integer4
                                                   ngnmin))))
                                  (t
                                   (setf lemin
                                         (min (the f2cl-lib:integer4
                                                   ngpmin)
                                              (the f2cl-lib:integer4
                                                   ngnmin)))
                                   (setf iwarn f2cl-lib:%true%))))
                           ((and (= (abs (f2cl-lib:int-add ngpmin
                                                           (f2cl-lib:int-sub ngnmin)))
                                    1)
                                 (= gpmin gnmin))
                            (cond ((= (f2cl-lib:int-add gpmin
                                                        (f2cl-lib:int-sub (min (the f2cl-lib:integer4
                                                                                    ngpmin)
                                                                               (the f2cl-lib:integer4
                                                                                    ngnmin))))
                                      3)
                                   (setf lemin
                                         (f2cl-lib:int-add (f2cl-lib:int-sub (max (the f2cl-lib:integer4
                                                                                       ngpmin)
                                                                                  (the f2cl-lib:integer4
                                                                                       ngnmin))
                                                                             1)
                                                           f2cl-lib:lt)))
                                  (t
                                   (setf lemin
                                         (min (the f2cl-lib:integer4
                                                   ngpmin)
                                              (the f2cl-lib:integer4
                                                   ngnmin)))
                                   (setf iwarn f2cl-lib:%true%))))
                           (t
                            (setf lemin
                                  (min (the f2cl-lib:integer4 ngpmin)
                                       (the f2cl-lib:integer4 ngnmin)
                                       (the f2cl-lib:integer4 gpmin)
                                       (the f2cl-lib:integer4 gnmin)))
                            (setf iwarn f2cl-lib:%true%)))
                     (cond (iwarn
                            (setf first$ f2cl-lib:%true%)
                            (f2cl-lib:fformat 6
                                              ("~%" "~%"
                                               " WARNING. The value EMIN may be incorrect:-"
                                               "  EMIN = " 1 (("~8D"))
                                               "~%"
                                               " If, after inspection, the value EMIN looks"
                                               " acceptable please comment out "
                                               "~%"
                                               " the IF block as marked within the code of routine"
                                               " DLAMC2," "~%"
                                               " otherwise supply EMIN explicitly."
                                               "~%" "~%")
                                              lemin)))
                     (setf ieee (or ieee lieee1))
                     (setf lrmin (into 1))
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i
                                       (f2cl-lib:int-add 1
                                                         (f2cl-lib:int-sub lemin)))
                                    nil)
                                   (tagbody
                                       (setf lrmin
                                             (multiple-value-bind (ret-val
                                                                   var-0
                                                                   var-1)
                                                 (dlamc3
                                                  (* lrmin rbase) zero)

                                               (when var-1
                                                 (setf zero var-1))
                                               ret-val))
                                     label30))
                     (multiple-value-bind (var-0 var-1 var-2 var-3
                                           var-4 var-5)
                         (dlamc5 lbeta f2cl-lib:lt lemin ieee lemax
                          lrmax)

                       (when var-0 (setf lbeta var-0))
                       (when var-1 (setf f2cl-lib:lt var-1))
                       (when var-2 (setf lemin var-2))
                       (when var-3 (setf ieee var-3))
                       (when var-4 (setf lemax var-4))
                       (when var-5 (setf lrmax var-5))))))
          (setf beta lbeta)
          (setf t$ f2cl-lib:lt)
          (setf rnd lrnd)
          (setf eps leps)
          (setf emin lemin)
          (setf rmin lrmin)
          (setf emax lemax)
          (setf rmax lrmax)
          (go end_label)
     end_label (return (values beta t$ rnd eps emin rmin emax rmax)))))

;;; Compiled by f2cl version 2.0 beta Date: 2005/06/01 15:29:41 
;;; Using Lisp International Allegro CL Enterprise Edition 7.0 [Windows] (Jan 30, 2006 8:40)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t)
;;;           (:relaxed-array-decls t) (:coerce-assigns :as-needed)
;;;           (:array-type ':simple-array) (:array-slicing t)
;;;           (:declare-common nil) (:float-format single-float))

(defun dlamc3 (a b)

  (prog ((dlamc3 (into 0)))

        (setf dlamc3 (+ a b))
        (go end_label)
   end_label (return (values dlamc3 a b))))

;;; Compiled by f2cl version 2.0 beta Date: 2005/06/01 15:29:41 
;;; Using Lisp International Allegro CL Enterprise Edition 7.0 [Windows] (Jan 30, 2006 8:40)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t)
;;;           (:relaxed-array-decls t) (:coerce-assigns :as-needed)
;;;           (:array-type ':simple-array) (:array-slicing t)
;;;           (:declare-common nil) (:float-format single-float))

(defun dlamc4 (emin start base)

  (prog ((a (into 0)) (b1 (into 0)) (b2 (into 0)) (c1 (into 0)) (c2 (into 0))
         (d1 (into 0)) (d2 (into 0)) (one (into 0)) (rbase (into 0)) (zero (into 0))
         (i 0))

        (setf a start)
        (setf one (into 1))
        (setf rbase (/ one base))
        (setf zero (into 0))
        (setf emin 1)
        (setf b1
              (multiple-value-bind (ret-val var-0 var-1)
                  (dlamc3 (* a rbase) zero)

                (setf zero var-1)
                ret-val))
        (setf c1 a)
        (setf c2 a)
        (setf d1 a)
        (setf d2 a)
   label10 (cond ((and (= c1 a) (= c2 a) (= d1 a) (= d2 a))
                  (setf emin (f2cl-lib:int-sub emin 1))
                  (setf a b1)
                  (setf b1
                        (multiple-value-bind (ret-val var-0 var-1)
                            (dlamc3 (/ a base) zero)
                          (setf zero var-1)
                          ret-val))
                  (setf c1
                        (multiple-value-bind (ret-val var-0 var-1)
                            (dlamc3 (* b1 base) zero)

                          (setf zero var-1)
                          ret-val))
                  (setf d1 zero)
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i base) nil)
                                (tagbody (setf d1 (+ d1 b1)) label20))
                  (setf b2
                        (multiple-value-bind (ret-val var-0 var-1)
                            (dlamc3 (* a rbase) zero)

                          (setf zero var-1)
                          ret-val))
                  (setf c2
                        (multiple-value-bind (ret-val var-0 var-1)
                            (dlamc3 (/ b2 rbase) zero)

                          (setf zero var-1)
                          ret-val))
                  (setf d2 zero)
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i base) nil)
                                (tagbody (setf d2 (+ d2 b2)) label30))
                  (go label10)))
        (go end_label)
   end_label (return (values emin nil nil))))

;;; Compiled by f2cl version 2.0 beta Date: 2005/06/01 15:29:41 
;;; Using Lisp International Allegro CL Enterprise Edition 7.0 [Windows] (Jan 30, 2006 8:40)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t)
;;;           (:relaxed-array-decls t) (:coerce-assigns :as-needed)
;;;           (:array-type ':simple-array) (:array-slicing t)
;;;           (:declare-common nil) (:float-format single-float))

(let* ((zero (into 0)) (one (into 1)))

  (defun dlamc5 (beta p emin ieee emax rmax)

    (prog ((oldy (into 0)) (recbas (into 0)) (y (into 0)) (z (into 0)) (exbits 0)
           (expsum 0) (i 0) (lexp 0) (nbits 0) (try 0) (uexp 0))

          (setf lexp 1)
          (setf exbits 1)
     label10 (setf try (f2cl-lib:int-mul lexp 2))
          (cond ((<= try (f2cl-lib:int-sub emin))
                 (setf lexp try)
                 (setf exbits (f2cl-lib:int-add exbits 1))
                 (go label10)))
          (cond ((= lexp (f2cl-lib:int-sub emin)) (setf uexp lexp))
                (t
                 (setf uexp try)
                 (setf exbits (f2cl-lib:int-add exbits 1))))
          (cond ((> (f2cl-lib:int-add uexp emin)
                    (f2cl-lib:int-add (f2cl-lib:int-sub lexp)
                                      (f2cl-lib:int-sub emin)))
                 (setf expsum (f2cl-lib:int-mul 2 lexp)))
                (t (setf expsum (f2cl-lib:int-mul 2 uexp))))
          (setf emax
                (f2cl-lib:int-sub (f2cl-lib:int-add expsum emin) 1))
          (setf nbits (f2cl-lib:int-add 1 exbits p))
          (cond ((and (= (mod nbits 2) 1) (= beta 2))
                 (setf emax (f2cl-lib:int-sub emax 1))))
          (cond (ieee (setf emax (f2cl-lib:int-sub emax 1))))
          (setf recbas (/ one beta))
          (setf z (- beta one))
          (setf y zero)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1)) ((> i p) nil)
                        (tagbody
                            (setf z (* z recbas))
                            (if (< y one) (setf oldy y))
                            (setf y
                                  (multiple-value-bind (ret-val var-0
                                                        var-1)
                                      (dlamc3 y z)

                                    (setf y var-0)
                                    (setf z var-1)
                                    ret-val))
                          label20))
          (if (>= y one) (setf y oldy))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1)) ((> i emax) nil)
                        (tagbody
                            (setf y
                                  (multiple-value-bind (ret-val var-0
                                                        var-1)
                                      (dlamc3 (* y beta) zero)

                                    (setf zero var-1)
                                    ret-val))
                          label30))
          (setf rmax y)
          (go end_label)
     end_label (return (values beta nil emin ieee emax rmax)))))

