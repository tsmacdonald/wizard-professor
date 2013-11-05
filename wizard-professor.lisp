;;;; wizard-professor.lisp

(in-package #:wizard-professor)

(defparameter *words*
  (with-open-file (in "/usr/share/dict/words")
    (loop for line = (read-line in nil) while line collect line)))

(defparameter *results* (make-hash-table :test 'equal))

(defstruct edit-scores
  (insert-cost 2)
  (delete-cost 2)
  (substitute-cost 3)
  (transpose-cost 3)
  (nop-cost 0))

(fare-memoization:define-memo-function (memoized-edit-distance :table *results*) (source target scores)
  (cond
    ((zerop (length source))
     (* (length target) (edit-scores-insert-cost scores)))
    ((zerop (length target))
     (* (length source) (edit-scores-delete-cost scores)))
    ((string-equal source target)
     (* (length target) (edit-scores-nop-cost scores)))
    (t
     (reduce #'min
	     (remove nil
		     (list
		      (+ (edit-scores-insert-cost scores)
			 (memoized-edit-distance source (subseq target 1) scores))
		      (+ (edit-scores-delete-cost scores)
			 (memoized-edit-distance (subseq source 1) target scores))
		      (+ (edit-scores-substitute-cost scores)
			 (memoized-edit-distance (subseq source 1) (subseq target 1) scores))
		      (when (and
			     (>= (length source) 2)
			     (>= (length target) 2)
			     (char-equal (char source 0) (char target 1))
			     (char-equal (char target 0) (char source 1)))
			(+ (edit-scores-transpose-cost scores)
			   (memoized-edit-distance (subseq source 2) (subseq target 2) scores)))
		      (when (char-equal (char source 0) (char target 0))
			(+ (edit-scores-nop-cost scores)
			   (memoized-edit-distance (subseq source 1) (subseq target 1) scores)))))))))

(defun edit-distance (source target &optional (scores (make-edit-scores)))
  (clrhash *results*)
  (memoized-edit-distance source target scores))

(defun word-and-score (source target)
  (cons target (edit-distance source target)))

(defun correct (word &optional (context ()))
  (if (find word *words* :test #'string-equal)
      word
      (car (reduce
	    (lambda (x y) (if (< (cdr x) (cdr y)) x y))
	    (mapcar (lambda (candidate)
		      (word-and-score word candidate))
		    *words*)))))