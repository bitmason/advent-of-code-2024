#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 13
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

(defparameter machines nil)

(let ((ax nil) (ay nil) (bx nil) (by nil) (px nil) (py nil))
  (loop for line in *input-as-list-of-strings*
	do (multiple-value-bind (matched groups) (re:scan-to-strings "^.+X.(\\d+), Y.(\\d+)" line)
	     (when matched
	       (cond ((not ax)
		      (setf ax (parse-integer (aref groups 0)))
		      (setf ay (parse-integer (aref groups 1))))
		     ((not bx)
		      (setf bx (parse-integer (aref groups 0)))
		      (setf by (parse-integer (aref groups 1))))
		     (t
		      (setf px (parse-integer (aref groups 0)))
		      (setf py (parse-integer (aref groups 1)))
		      (setf machines (cons (list ax ay bx by px py) machines))
		      (setf ax nil)
		      (setf bx nil)
		      (setf px nil)))))))

(defun solutions (ax ay bx by px py &optional (offset 0))  ; list of solution press counts: ((a b) (a b) ...)) or nil
  (loop for a from 0 upto (+ 100 (floor offset 100))
	with solutions = nil
	finally (return solutions)
	do (loop for b from 0 upto (+ 100 (floor offset 100))
		 do (when (and (= (+ (* a ax) (* b bx)) (+ px offset))
			       (= (+ (* a ay) (* b by)) (+ py offset)))
		      (setf solutions (cons (list a b) solutions))))))

(format t "Part 1: ~a~%"
	(loop for (ax ay bx by px py) in machines
	      sum (loop for (a b) in (solutions ax ay bx by px py)
			minimizing (+ (* 3 a) b))))

(format t "Part 2: ~a~%"  ; NOTE: THIS WILL, OF COURSE, NOT FINISH (note to self: grab a pencil & paper)
	(loop for (ax ay bx by px py) in machines
	      sum (loop for (a b) in (solutions ax ay bx by px py 10000000000000)
			minimizing (+ (* 3 a) b))))
