#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 06
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

(defparameter grid (input-as-2d-array-of-chars))
(defvar guard-chars '(#\^ #\> #\v #\<)) ; direction index: 0=up,1=right,2=down,3=left
(defvar visited-chars '(#\" #\) #\_ #\()) ; visited with indexed direction
(defvar dir-offs '((-1 0) (0 1) (1 0) (0 -1))) ; grid pos offset corresponding to guard-chars
(defun guard-at-p (y x) (position (aref grid y x) guard-chars)) ; guard at pos? nil or index into guard-chars
(defun obst-at-p (y x) (eq #\# (aref grid y x))) ; obstacle @ pos?

;;; (y x dir) for guard
(defun guard-at ()
  (loop named outer for y from 0 upto (- *height* 1)
	do (loop for x from 0 upto (- *width* 1)
		 do (let ((dir (guard-at-p y x)))       ; nil or dir index 0..3
		      (if dir (return-from outer (list y x dir)))))))

; visited (list of 4 dirs, each 2d array), for Part 2
(defparameter grid-visited-dirs nil)
(defun visited-reset ()
  (setf grid-visited-dirs (list
	(make-array (list *height* *width*) :initial-element nil)
	(make-array (list *height* *width*) :initial-element nil)
	(make-array (list *height* *width*) :initial-element nil)
	(make-array (list *height* *width*) :initial-element nil))))
(visited-reset)

;;; Run guard around map, marking visited positions, until she: exits or loops.
(defun run (y x dir)
  (let* ((dir-off (nth (or dir 0) dir-offs)) ; (y x) offset to new pos
	 (y-next (+ y (first dir-off))) ; y guard wants to move to
	 (x-next (+ x (second dir-off)))) ; x guard wants to move to
    (setf (aref grid y x) (nth dir visited-chars)) ; mark visited for Part 1
    (setf (aref (nth dir grid-visited-dirs) y x) t) ; for Part 2
    (cond ((or (< y-next 0) (>= y-next *height*) (< x-next 0) (>= x-next *width*)) 
	   'exit) ; exit!
	  ((obst-at-p y-next x-next) ; blocked!
	   (setf dir (mod (1+ dir) 4))
	   (setf (aref grid y x) (nth dir guard-chars))
	   (run y x dir)) ; rotate CW
	  ((eq (aref grid y-next x-next) (nth dir visited-chars))
	   'loop) ; start of loop! (Part 1)
	  ((aref (nth dir grid-visited-dirs) y-next x-next)
	   'loop) ; in a loop (Part 2)
	  (t (setf (aref grid y-next x-next) (nth dir guard-chars))
	     (run y-next x-next dir)))))


(destructuring-bind (y x dir) (guard-at)
  (run y x dir))
(format t "Part 1: ~a~%" (loop for y from 0 upto (- *height* 1)
			       sum (loop for x from 0 upto (- *width* 1)
					 when (member (aref grid y x) visited-chars) count it)))

(setf grid (input-as-2d-array-of-chars)) ; reset
(visited-reset)
(destructuring-bind (guard-y guard-x guard-dir) (guard-at)
  (format t "Part 2: ~a~%" (loop for y from 0 upto (- *height* 1)
				 sum (loop for x from 0 upto (- *width* 1)
					   with pos-count = 0
					   finally (return pos-count)
					   do (setf grid (input-as-2d-array-of-chars)) ; reset
					      (visited-reset)
					      (when (not (or (guard-at-p y x) (obst-at-p y x)))
						(setf (aref grid y x) #\#) ; new obstacle
						(if (eq (run guard-y guard-x guard-dir) 'loop)
						    (incf pos-count)))))))
