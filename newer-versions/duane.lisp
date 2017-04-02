(eval-when (compile)
 (ff:def-foreign-call
   (qd_add  "c_qd_add")
   ((op1  (:array :double) (simple-array double-float (4)))
    (op2  (:array :double) (simple-array double-float (4) ))
    (target  (:array :double) (simple-array double-float(4) )))
   :returning :void
   :arg-checking nil :call-direct t))

(defun call-qd_add (a b dest)
  (declare (optimize speed)
   ((simple-array double-float (*)) a b dest))
  (qd_add a b dest))

(defun call-qd_add2 (a b dest)
  (declare (optimize speed)
   (type (simple-array double-float (*)) a b dest))
  (qd_add a b dest))
