;;;; wizard-professor.lisp

(in-package #:wizard-professor)

(defparameter *words*
  (with-open-file (in "/usr/share/dict/words")
    (loop for line = (read-line in nil) while line collect line)))
  
(fare-memoization:define-memo-function edit-distance
    (source target &key
	    (insert-cost 2) (delete-cost 2) (substitute-cost 3)
	    (transpose-cost 3) (nop-cost 0))
  (cond
    ((zerop (length source))
     (* (length target) insert-cost))
    ((zerop (length target))
     (* (length source) delete-cost))
    ((string-equal source target)
     (* (length target) nop-cost))
    (t
     (reduce #'min
	     (remove nil
		     (list
		      (+ insert-cost (edit-distance source (subseq target 1)))
		      (+ delete-cost (edit-distance (subseq source 1) target))
		      (+ substitute-cost (edit-distance (subseq source 1) (subseq target 1)))
		      (when (and
			     (>= (length source) 2)
			     (>= (length target) 2)
			     (char-equal (char source 0) (char target 1))
			     (char-equal (char target 0) (char source 1)))
			(+ transpose-cost (edit-distance (subseq source 2) (subseq target 2))))
		      (when (char-equal (char source 0) (char target 0))
			(+ nop-cost (edit-distance (subseq source 1) (subseq target 1))))))))))