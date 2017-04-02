

(defun mdfss
    ;;multiply-double-floats-setting-post-status
    (a b status)
  (declare (double-float a b)
	   (type (simple-array (unsigned-byte 32)) status)
	   (optimize (speed 3)(safety 0)))
        (fclex); clear exceptions
       (prog1  (* a b)
        (fstsw status) ;;; store 16 bits of status register into a word-aligned 16 bits
	))


(defun mdfc
    ;;multiply-double-floats-with-control-word
    (a b cw) ;; cw is the word set into fp control, e.g. rounding mode
  (declare (double-float a b)
	   (type (simple-array (unsigned-byte 32)) cw)
	   (optimize (speed 3)(safety 0)))
  (fldcw cw)				; set control word
  (* a b))

;; maybe  saving/restoriing old cw would be polite

       
(defparameter stat (make-array 1 :element-type '(unsigned-byte 32)))

;;(setq comp::*hack-compiler-output* '(mdfc))
;; edit the file
;; :cont
;;(setq comp::*hack-compiler-output* nil)
