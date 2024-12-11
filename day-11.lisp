#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 11
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

(defun hash-set (h n v) (setf (gethash n h) v))
(defun hash-inc (h n &optional (v 1)) (hash-set h n (+ v (gethash n h 0))))
(defun hash-dec (h n &optional (v 1)) (hash-set h n (- (gethash n h 0) v)))

;;; track quantity of stones of each inscribed value (stones are unordered)
(defparameter orig-count (make-hash-table :test 'equal))
(loop for n in (first (input-as-list-of-lists-of-integers))
      do (hash-inc orig-count n))

(defun blink (old-count)
  (let ((new-count (make-hash-table :test 'equal)))
    (loop for n being the hash-keys in old-count using (hash-value c)
	  when (> c 0)
	    do (let ((s (write-to-string n)))
		 (cond ((= n 0)  ; 0 -> 1
			(hash-inc new-count 1 c))
		       ((= (mod (length s) 2) 0)  ; even length -> 1st half, 2nd half
			(hash-inc new-count (parse-integer (subseq s 0 (/ (length s) 2))) c)
			(hash-inc new-count (parse-integer (subseq s (/ (length s) 2))) c))
		       (t (hash-inc new-count (* n 2024) c)))))  ; else
    new-count))

(defun value-count (h)
  (loop for n being the hash-keys in h using (hash-value c) sum c))

(defparameter new-count nil)
(loop for blinks in '(25 75)
      for part from 1 by 1
      do (setf new-count orig-count)
	 (loop repeat blinks do (setf new-count (blink new-count)))
	 (format t "Part ~a: ~a~%" part (value-count new-count)))
