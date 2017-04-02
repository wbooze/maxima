;;;                 COPYRIGHT NOTICE
;;;  
;;;  Copyright (C) 2007 Mario Rodriguez Riotorto
;;;  
;;;  This program is free software; you can redistribute
;;;  it and/or modify it under the terms of the
;;;  GNU General Public License as published by
;;;  the Free Software Foundation; either version 2 
;;;  of the License, or (at your option) any later version. 
;;;  
;;;  This program is distributed in the hope that it
;;;  will be useful, but WITHOUT ANY WARRANTY;
;;;  without even the implied warranty of MERCHANTABILITY
;;;  or FITNESS FOR A PARTICULAR PURPOSE. See the 
;;;  GNU General Public License for more details at
;;;  http://www.gnu.org/copyleft/gpl.html

;;; This is a cyclic cellular automaton simulation package for Maxima

;;; For questions, suggestions, bugs and the like, feel free
;;; to contact me at
;;; mario @@@ edu DOT xunta DOT es
;;; http://www.telefonica.net/web2/biomates/



;; Copies an array. Copies multidimensional arrays.
(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
      (make-array
         dims
         :element-type (array-element-type array)
         :displaced-to array)
      dims)))


;; Transforms a 2d array into a Maxima matrix
(defun array2matrix (ar)
  (let* ((dims (array-dimensions ar))
         (nrows (car dims))
         (ncols (cadr dims))
         arow)
    (append
       '(($matrix simp))
       (loop for nr below nrows do
          (setf arow (loop for nc below ncols
                        collect (aref ar nr nc)) )
          collect (cons '(mlist simp) arow)) ) ) )



;; Usage:
;;     ca_cyclic_von_neumann(inistate,k,n,numiter)
;; where:
;;     inistate: Maxima matrix containing numbers from zero
;;        up to (k-1). Minimum dimension: 3x3.
;;     k: number of states for each cell, ranging
;;        from 0 up to (k-1)
;;     n: 1, 2, 3 or 4. If a cell is in state s, it changes to
;;        state (s+1) mod k if there are at least n cells in the neightborhood
;;        in this state. This is a contagious process.
;;     numiter: number of iterations to simulate
;;
(defun $ca_cyclic_von_neumann (inistate k n numiter)
  (when (or (not (integerp k))
            (< k 2))
    (merror "Number of different cell states must be an integer greater than 1") )
  (when (or (not ($matrixp inistate))
            (notevery
               #'(lambda (z) (and (integerp z) (>= z 0) (< z k)))
               (apply #'append (map 'list #'rest (rest inistate))) )
            (< ($length inistate) 3)
            (< ($length (cadr inistate)) 3))
    (merror "Initial state must be a matrix of integers between 0 and ~M" (- k 1)) )
  (when (not (member n '(1 2 3 4)))
    (merror "Number of infected neighbors must be an integer from 1 to 4"))
  (when (or (not (integerp numiter))
            (< numiter 1))
    (merror "Number of iterations must be an integer greater than 0"))
  (let (history actual next nextstate vecinity
        prevcol prevrow nextcol nextrow
        (ncols (length (cdadr inistate)))
        (nrows (length (cdr inistate))))
    (setf
       actual (make-array 
                 (list nrows ncols)
                 :element-type 'unsigned-byte
                 :initial-contents (map 'list #'cdr (cdr inistate)))
       next (make-array
               (list nrows ncols)
               :element-type 'unsigned-byte
               :initial-element 0))
    (setf
       history
       (append
          '((mlist simp))
          (list inistate)
          (loop for m from 1 to numiter do
             (loop for nr below nrows do
                (setf
                   prevrow (mod (- nr 1) nrows)
                   nextrow (mod (+ nr 1) nrows) )
                (loop for nc below ncols do
                   (setf
                      prevcol (mod (- nc 1) ncols)
                      nextcol (mod (+ nc 1) ncols) )
                   (setf nextstate
                         (mod (+ (aref actual nr nc) 1) k))
                   (setf vecinity 
                         (list (aref actual prevrow nc)
                               (aref actual nr nextcol)
                               (aref actual nextrow nc)
                               (aref actual nr prevcol)))
                   (if (>= (count nextstate vecinity) n)
                      (setf (aref next nr nc) nextstate)
                      (setf (aref next nr nc) (aref actual nr nc))   )  ))
             (setf actual (copy-array next))
             collect (array2matrix actual)  ) ))
    history ))








