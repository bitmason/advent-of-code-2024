#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 03
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

;; registers
(defparameter accumulator 0)
(defparameter mul-enabled t)
(defparameter isa nil)

;; instruction sets
(defvar isa-part-1 '(
  ("mul"    2  (lambda (x y) (incf accumulator (* x y))))))
(defvar isa-part-2 '(
  ("mul"    2  (lambda (x y) (when mul-enabled (incf accumulator (* x y)))))
  ("do"     0  (lambda ()    (setf mul-enabled t)))
  ("don't"  0  (lambda ()    (setf mul-enabled nil)))))

(defun cpu-reset (new-isa)
  (setf isa new-isa)
  (setf accumulator 0)
  (setf mul-enabled t))

;;; Parse next resemblence of an instruction, return values: op args pos-next (or nil if none).
(defun next-potential-elem (text pos)
  (multiple-value-bind (matched groups) (re:scan-to-strings ".*?([a-z']+)\\(([\\d,]*)\\)" text :start pos)
    (when matched
      (let ((op (aref groups 0))
	    (args (if (equal (aref groups 1) "")  ; no args
		      nil
		      (mapcar #'(lambda (s) (parse-integer s :junk-allowed t)) (str:split "," (aref groups 1))))))
	(if (every #'(lambda (v) (and v)) args)
	    (values op args (+ pos (length matched)))  ; opcode is stringy, args are inty
	    (next-potential-elem text (+ pos (length matched))))))))  ; keep looking

;;; Run program text from position.
(defun run (text &optional (pos 0))
  (if (and text pos)  ; end of program text
      (multiple-value-bind (op args pos-next) (next-potential-elem text pos)
	(if op (loop for (op-test argc exec) in isa
		     when (and (str:ends-with-p op-test op) (= argc (length args)))
		       do (apply (coerce exec 'function) args)))
	(run text pos-next))))  ; continue run, tail recursive

(cpu-reset isa-part-1)
(run *input-as-string*)
(format t "Part 1: ~a~%" accumulator)

(cpu-reset isa-part-2)
(run *input-as-string*)
(format t "Part 2: ~a~%" accumulator)
