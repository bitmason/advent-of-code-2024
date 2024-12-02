#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 02
;;;; Solution by Darren Stone <dstone at bitmason dot com>
;;;; Written in Common Lisp. Using Emacs. By hand. Without fucking AI.

(load "input.lisp")

(defvar reports (input-as-list-of-lists-of-integers))

(defun monotonic-p (r compare)
  (let ((r-sorted (sort (copy-list r) compare)))
    (equal r r-sorted)))

(defun adj-diff-ok (r)
  (loop for i from 0 upto (- (length r) 2)
	always (<= 1 (abs (- (nth i r) (nth (1+ i) r))) 3)))

(defun safe-p (r)
  (and (or (monotonic-p r #'<)
	   (monotonic-p r #'>))
       (adj-diff-ok r)))

(format t "Part 1: ~a~%" (loop for r in reports when (safe-p r) count it))

(defun remove-nth (n list)
  (nconc (subseq list 0 n) (nthcdr (1+ n) list)))

(defun safe-ish-p (r)
  (or (safe-p r)
      (loop for i from 0 upto (- (length r) 1)
	    thereis (safe-p (remove-nth i r)))))

(format t "Part 2: ~a~%" (loop for r in reports when (safe-ish-p r) count it))
