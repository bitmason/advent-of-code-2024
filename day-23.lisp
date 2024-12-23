#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 23
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

(defun sethash (h k v) (setf (gethash k h) v))

(defvar pairs (input-as-list-of-lists-of-strings :separator "-"))

(defparameter conn (make-hash-table :test 'equal))
(loop for (a b) in pairs
      do (let ((aconn (gethash a conn nil))
	       (bconn (gethash b conn nil)))
	   (sethash conn a (cons b aconn))
	   (sethash conn b (cons a bconn))))

(defun conn-p (a b)
  (or (position b (gethash a conn nil) :test 'equal)
      (position a (gethash b conn nil) :test 'equal)))

(defun tstart-p (str) (equal "t" (subseq str 0 1)))

(defparameter ttriples nil)
(loop for a being the hash-keys in conn using (hash-value aconn)
      do (when (tstart-p a)
	   (loop for bi from 0 upto (- (length aconn) 2)
		 for b in aconn
		 do (loop for ci from (1+ bi) upto (- (length aconn) 1)
			  for c in (nthcdr (1+ bi) aconn)
			  do (when (conn-p b c)
			       (let ((ttriple (sort (list a b c) #'string-lessp)))
				 (unless (position ttriple ttriples :test 'equal)
				   (setf ttriples (cons ttriple ttriples)))))))))
	 
(format t "Part 1: ~a~%" (length ttriples))

;;; TODO: Part 2
