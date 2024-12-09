#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 09
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

(defparameter sizes (loop for s across (str:trim *input-as-string*)
			  collect (- (char-code s) (char-code #\0)))) ; file|free|file|free|...

(defparameter disk-size (loop for s in sizes sum s))
(defparameter block-ids (make-array disk-size)) ; file id of each block or nil if free

(defparameter free-trapped nil) ; # free embedded
(defparameter free-right nil) ; # congtiguous free on right

(defun init () ; create block-ids
  (setf free-trapped 0)
  (setf free-right 0)
  (loop for s in sizes
	for si from 0 by 1
	with blocks-i = 0
	with file-count = 0
	do (loop for i from blocks-i upto (+ blocks-i (- s 1))
		 do (setf (aref block-ids i)
			  (if (zerop (mod si 2)) file-count nil))) ; mark file id
	   (if (zerop (mod si 2))
	       (incf file-count)
	       (incf free-trapped s)) ; count trapped
	   (incf blocks-i s)))

(defun checksum () ; per problem description
  (loop for file-id across block-ids
	for bi from 0 by 1
	when file-id
	sum (* file-id bi)))

;;; === Part 1 ===

(defun fill-first-free ()
  (let ((first-free-i (loop for b across block-ids for bi from 0 by 1 finally (return bi) until (not b)))
	(last-file-i (- disk-size free-right 1)))
    (setf (aref block-ids first-free-i) (aref block-ids last-file-i))
    (decf free-trapped)
    (setf (aref block-ids last-file-i) nil)
    (incf free-right)))

(init)
(loop while (> free-trapped 0)
      do (fill-first-free))

(format t "Part 1: ~a~%" (checksum))

;;; === Part 2 ===

(defun free-at (i)
  (loop for bi from i upto (- disk-size 1) while (not (aref block-ids bi)) count bi))

(defun first-free-fit (file-size orig-i) ; index of first contiguous free region from left for file-size or nil
  (loop for bi from 0 upto (- orig-i 1)
	finally (return nil)
	do (let ((avail (free-at bi)))
	     (if (>= avail file-size)
 		 (return bi)))))

(init)
(loop for file-id from (truncate (length sizes) 2) downto 0
      with orig-i = nil
      do (setf orig-i (loop for s in sizes for i from 0 while (< i (* 2 file-id)) sum s)) ; original loc
	 (let* ((file-size (nth (* file-id 2) sizes))
		(free-fit-i (first-free-fit file-size orig-i)))
	   (when free-fit-i
	     (loop for n from 0 upto (- file-size 1)
		   do (setf (aref block-ids (+ free-fit-i n)) file-id)
		      (setf (aref block-ids (+ orig-i n)) nil))
	     (incf free-right file-size)))) ; NOTE: don't bother wiping)

(format t "Part 2: ~a~%" (checksum))
