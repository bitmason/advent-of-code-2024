#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 08
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

(defvar amap (input-as-2d-array-of-chars))
(defparameter anodes1 nil)  ; list of antinodes for Part 1 (y x freq)
(defparameter anodes2 nil)  ; list of antinodes for Part 2 (y x freq) 

(defun in-bounds-p (y x) (and (>= y 0) (< y *height*) (>= x 0) (< x *width*)))

(loop for ya from 0 upto (- *height* 1)
  do (loop for xa from 0 upto (- *width* 1)
       do (let ((freqa (aref amap ya xa)))
	    (when (not (eq freqa #\.))  ; antenna a freq
	      (loop for yb from ya upto (- *height* 1)
	        do (loop for xb from 0 upto (- *width* 1)
		     do (let ((freqb (aref amap yb xb)))
		          (when (and (not (equal (list ya xa) (list yb xb)))
				     (or (> yb ya) (> xb yb))  ; all unordered a,b pairs
				     (eq freqa freqb))  ; antenna b freq
			    (let* ((dy (- yb ya))
				   (dx (- xb xa))
				   (an1y (- ya dy))
				   (an1x (- xa dx))
				   (an2y (+ yb dy))
				   (an2x (+ xb dx)))
			      (if (in-bounds-p an1y an1x) (push (list an1y an1x freqa) anodes1))
			      (if (in-bounds-p an2y an2x) (push (list an2y an2x freqa) anodes1))
			      )))))))))

(format t "Part 1: ~a~%" (length (remove-duplicates anodes1 :test (lambda (a b) (equal (butlast a) (butlast b))))))
								   
(loop for ya from 0 upto (- *height* 1)
  do (loop for xa from 0 upto (- *width* 1)
       do (let ((freqa (aref amap ya xa)))
	    (when (not (eq freqa #\.))  ; antenna a freq
	      (loop for yb from ya upto (- *height* 1)
	        do (loop for xb from 0 upto (- *width* 1)
		     do (let ((freqb (aref amap yb xb)))
		          (when (and (not (equal (list ya xa) (list yb xb)))
				     (or (> yb ya) (> xb yb))  ; all unordered a,b pairs
				     (eq freqa freqb))  ; antenna b freq
			    (let* ((dy (- yb ya))
				   (dx (- xb xa)))
			      (loop for n from 0 upto (max *width* *height*)  ; multiples of dx,dy in each dir
				    do (let ((an1y (- ya (* n dy)))
					     (an1x (- xa (* n dx)))
					     (an2y (+ yb (* n dy)))
					     (an2x (+ xb (* n dx))))
					 (if (in-bounds-p an1y an1x) (push (list an1y an1x freqa) anodes2))
					 (if (in-bounds-p an2y an2x) (push (list an2y an2x freqa) anodes2))
					 )))))))))))

(format t "Part 2: ~a~%" (length (remove-duplicates anodes2 :test (lambda (a b) (equal (butlast a) (butlast b))))))
