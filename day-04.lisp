#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 04
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

(defvar grid (input-as-2d-array-of-chars))

;;; possible lines for Part 1
(defvar dir-offs '(((-1  0) (-2  0) (-3  0))
		   ((-1  1) (-2  2) (-3  3))
		   (( 0  1) ( 0  2) ( 0  3))
		   (( 1  1) ( 2  2) ( 3  3))
		   (( 1  0) ( 2  0) ( 3  0))
		   (( 1 -1) ( 2 -2) ( 3 -3))
		   (( 0 -1) ( 0 -2) ( 0 -3))
		   ((-1 -1) (-2 -2) (-3 -3))))

;;; the shape for Part 2
(defvar x-offs '(((0 0) (1 1) (2 2))
		 ((2 0) (1 1) (0 2))))
  
;;; Part 1: count of XMAS at given position in grid (i.e. 0..8))
(defun xmas-count-at (y x)
  (loop for dir in dir-offs
	sum (loop for (dy dx) in (cons '(0 0) dir)
		  for c across "XMAS"
		  unless (let ((ty (+ y dy))
			       (tx (+ x dx)))
			   (and (<= 0 ty (- *height* 1))
				(<= 0 tx (- *width* 1))
				(eq c (aref grid ty tx))))
		    return 0
		  finally (return 1))))

;;; Part 2: is X-MAS at given position in grid?
(defun x-mas-at-p (y x)
  (= 2 (loop for dir in x-offs
		 count (loop for (dy dx) in dir
			      for c across "MAS" with mas = 0
			      for d across "SAM" with sam = 0
			      do (let ((ty (+ y dy))
				       (tx (+ x dx)))
				   (when (and (<= 0 ty (- *height* 1))
					      (<= 0 tx (- *width* 1)))
				     (if (eq c (aref grid ty tx)) (incf mas))
				     (if (eq d (aref grid ty tx)) (incf sam))))
				 finally (return (or (= mas 3) (= sam 3)))))))
	       
(defparameter xmas-count 0)
(defparameter x-mas-count 0)

(loop for y from 0 to (- *height* 1)
      do (loop for x from 0 to (- *width* 1)
	       do (incf xmas-count (xmas-count-at y x))
		  (if (x-mas-at-p y x) (incf x-mas-count))))

(format t "Part 1: ~a~%" xmas-count)
(format t "Part 2: ~a~%" x-mas-count)
