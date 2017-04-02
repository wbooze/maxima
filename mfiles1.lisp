;;; mfiles1.lisp
;;; loaded by mfiles.mac
;;; Copyright (C) 2011,  Edwin L. Woollett  <woollett@charter.net>       
;;;    http://www.csulb.edu/~woollett
;;;  I release this code file
;;; 


 (defun $directory (path) (list* '(mlist) (directory path)))
 
 
 (defun rename1 (mfrom mto)
   (cons '(mlist simp) (multiple-value-list (rename-file mfrom mto))))
  
;;  alternative methods could have been used:

;;  (defun rename2 (mfrom mto)
;;     (append '((mlist simp)) (multiple-value-list (rename-file mfrom mto))))
;;
  
;; (defun rename3 (mfrom mto)
;;  (take '(mlist) (multiple-value-list (rename-file mfrom mto))))
;;  
 
 
  



 

 



