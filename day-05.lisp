#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 05
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

(defvar rules (loop for r in (input-as-list-of-lists-of-integers :separator "|")
		    while r collect r))  ; skip update section

;;; return non-nil iff update is in correct order
(defun correct-p (update)
  (loop for (a b) in rules
	always (< (or (position a update) -1)
		  (or (position b update) 99999))))

;;; return corrected version of the update
(defun corrected (update)
  (loop for (a b) in rules
	finally (return update)
	when (> (or (position a update) -1)
		(or (position b update) 99999))
          ;; when any rule is broken, move the low element to the start of the list and repeat till done
	  return (corrected (cons a (remove a update)))))  ; tail recursive

(format t "(Part_1 Part_2):  ~a~%"
	(loop for u in (subseq (input-as-list-of-lists-of-integers :separator ",")
			       (1+ (length rules)))  ; skip rules section
	      finally (return (list sum-correct-mids sum-corrected-mids))
	      if (correct-p u)
		sum (nth (truncate (length u) 2) u) into sum-correct-mids
	      else
		sum (nth (truncate (length u) 2) (corrected u)) into sum-corrected-mids))
