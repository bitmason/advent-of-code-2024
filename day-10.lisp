#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 10
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

(defvar topo (input-as-2d-array-of-integers))
(format t "~ax~a topo map~%" *width* *height*)

(defparameter trails-seen nil) ; track unique trailhead-to-peak trails
(defun trail-seen-p (th-y th-x peak-y peak-x) (position (list th-y th-x peak-y peak-x) trails-seen :test #'equal))

;;; # valid trails from trailhead, via this position, to peak
;;; (call with trailhead nil for Part 2 because we don't care about unique trailhead-to-peak tracking)
(defun th-score (th-y th-x this-y this-x elev-need)
  (if (or (< this-y 0) (>= this-y *height*) (< this-x 0) (>= this-x *width*))
      0  ; out of bounds
      (let ((elev (aref topo this-y this-x)))  ; current elevation
	(if (not (= elev elev-need))  ; not hiking trail elev delta
	    0
	    (if (= elev 9)  ; peak
		(if (and th-y)  ; tracking unique trailhead-to-peak or not? (i.e. Part 1 vs Part 2)
		    (if (trail-seen-p th-y th-x this-y this-x)
			0
			(progn (push (list th-y th-x this-y this-x) trails-seen) 1))
		    1)
		(+ (th-score th-y th-x (- this-y 1) this-x       (1+ elev))  ; continue in each direction
		   (th-score th-y th-x this-y       (1+ this-x)  (1+ elev))
		   (th-score th-y th-x (1+ this-y)  this-x       (1+ elev))
		   (th-score th-y th-x this-y       (- this-x 1) (1+ elev))))))))

(format t "Part 1: ~a~%"
  (loop for th-y from 0 upto (- *height* 1) 
	sum (loop for th-x from 0 upto (- *width* 1)
		  sum (th-score th-y th-x th-y th-x 0))))

(format t "Part 2: ~a~%"
  (loop for th-y from 0 upto (- *height* 1) 
	sum (loop for th-x from 0 upto (- *width* 1)
		  sum (th-score nil nil th-y th-x 0))))
