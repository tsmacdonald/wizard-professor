;;;; wizard-professor.lisp

(in-package #:wizard-professor)

(defparameter *alpha* .0005)
(defparameter *edit-tolerance* 6)

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

(defun in-dictionary-p (word)
  (find word *words* :test (lambda (x y) (string-equal (string-downcase x) (string-downcase y)))))

(defun correct (word &optional (context ()))
  (if (in-dictionary-p word)
      word
      (car (reduce
	    (lambda (x y) (if (< (cdr x) (cdr y)) x y))
	    (mapcar (lambda (candidate)
		      (word-and-score word candidate))
		    *words*)))))

(defun correct2 (a b word frequencies)
  (if (and (in-dictionary-p word)
	   (>= (probability a b word frequencies) *alpha*))
      word ;;It's not an error
      (let*
	  ((candidates
	    (mapcar (lambda (w-a-s) (cons (probability a b (car w-a-s) frequencies)
					  (car w-a-s)))
		    (sort (remove-if-not
			   (lambda (word-and-score) (<= (cdr word-and-score) *edit-tolerance*))
			   (mapcar (lambda (target) (word-and-score word target)) *words*))
			  (lambda (x y) (if (< (cdr x) (cdr y)) x y)))))
	   ;;Candidates is now a list of probability-word conses
	   (best (apply #'max (mapcar #'car candidates))))
	(cdr (assoc best candidates)))))

(defun correct-line (line frequencies)
  (format nil "~{~A~^  ~}"
	  (remove-if #'null (loop for (a b c) on (append '(nil) '(nil) (cl-ppcre:split "[^\\w']+" line))
			       collecting (correct2 a b c frequencies)))))
					       

(defun correct-file (frequencies filename &optional (output-stream t))
  (with-open-file (in filename)
    (when in
      (loop for line = (read-line in nil)
         while line do (format output-stream "~s" (correct-line line frequencies))))))

(defun train-trigram-model (file)
  (with-open-file (in file)
    (let ((frequencies (make-hash-table :test 'equal)))
      (flet ((e-gethash (key table default)
	       (alexandria:ensure-gethash key table default)))
	(when in
	  (loop for line = (read-line in nil)
	     while line do
	       (let ((words (append '(nil) '(nil) (mapcar #'string-downcase (cl-ppcre:split "[^\\w']+" line)))))
		 (loop for (a b c) on words
		    do
		      (when c
			(e-gethash c
				   (e-gethash b
					      (e-gethash a frequencies (make-hash-table :test 'equal))
					      (make-hash-table :test 'equal))
				   0)
			(incf (gethash c (gethash b (gethash a frequencies))))))))))
      frequencies)))
		    

(defun counts-for (a b frequencies)
  (gethash b (gethash a frequencies (make-hash-table :test 'equal)) (make-hash-table :test 'equal)))

(defun probability (a b c frequencies &optional (k 1))
  (let* ((counts (counts-for a b frequencies))
	 (total (loop for count being the hash-values of counts summing count))
	 (size (hash-table-size counts)))
    (/ (+ (gethash c counts 0) k)
       (+ total (* k size)))))