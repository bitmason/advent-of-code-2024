#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 01
;;;; Solution by Darren Stone <dstone at bitmason dot com>
;;;; Written in Common Lisp. Using Emacs. By hand. Without fucking AI.

(load "input.lisp")

(defvar pairs (input-as-list-of-lists-of-integers))
(defvar left (mapcar #'first pairs))
(defvar right (mapcar #'second pairs))
(defvar left-sorted (sort (copy-list left) #'<))
(defvar right-sorted (sort (copy-list right) #'<))

(format t "Part 1: ~a~%" (loop for a in left-sorted
			       for b in right-sorted
			       sum (abs (- a b))))

(format t "Part 2: ~a~%" (loop for n in left
			       sum (* n (loop for m in right
					      when (equal n m) count it))))
