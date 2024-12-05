#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 05
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

(defvar rules (loop for r in (input-as-list-of-lists-of-strings :separator "|")
		    while r
		    collect (mapcar #'parse-integer r)))

(defun correct-p (update)
  (loop for (a b) in rules
	always (< (or (position a update) -1)
		  (or (position b update) 99999))))

(defun corrected (update)
      (loop for (a b) in rules
	    finally (return update)
	    when (> (or (position a update) -1)
		    (or (position b update) 99999))
	      return (corrected (cons a (remove a update)))))

(format t "(Part_1 Part_2):  ~a~%"
	(loop for u-str in (subseq (input-as-list-of-lists-of-strings :separator ",")
				   (1+ (length rules)))
	      with sum-correct-mids = 0
	      with sum-corrected-mids = 0
	      finally (return (list sum-correct-mids sum-corrected-mids))
	      do (let ((u (mapcar #'parse-integer u-str)))
		   (if (correct-p u)
		       (incf sum-correct-mids (nth (truncate (length u) 2) u))
		       (incf sum-corrected-mids (nth (truncate (length u) 2) (corrected u)))))))
