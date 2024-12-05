#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 03
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

;; instruction sets
(defvar isa-part-1 '(
  ("^mul\\((\\d+),(\\d+)\\)" (lambda (x y) (incf accumulator (* x y))))))
(defvar isa-part-2 '(
  ("^mul\\((\\d+),(\\d+)\\)" (lambda (x y) (when mul-enabled (incf accumulator (* x y)))))
  ("^do\\(\\)"               (lambda ()    (setf mul-enabled t)))
  ("^don't\\(\\)"            (lambda ()    (setf mul-enabled nil)))))

;; registers
(defparameter accumulator 0)
(defparameter mul-enabled t)

;;; run program text from position with given isa
(defun run (isa text &optional (pos 0))
  (if (< pos (length text))
  (run isa text
       (loop for (instruc exec) in isa
	     finally (return (1+ pos)) ; no match; continue
             do	(multiple-value-bind (matched groups) (re:scan-to-strings instruc text :start pos)
		  (when matched
		    (let ((args (map 'list #'(lambda (s) (parse-integer s :junk-allowed t)) groups)))
		      (apply (coerce exec 'function) args))
		    (return (+ pos (length matched))))))))) ; matched; contine after match

(run isa-part-1 *input-as-string*)
(format t "Part 1: ~a~%" accumulator)

(setf accumulator 0)
(setf mul-enabled t)
(run isa-part-2 *input-as-string*)
(format t "Part 2: ~a~%" accumulator)
