#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 15
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

;;; warehouse
(defparameter *w* (length (first *input-as-list-of-strings*)))
(loop for line in *input-as-list-of-strings*
      for h from 0 by 1
      do (if (zerop (length (str:trim line))) (defparameter *h* h)))
(defparameter *ware*
  (make-array (list *h* *w*) :initial-contents
	      (loop for row-count from 0 upto (- *h* 1)
		    for row in *input-as-list-of-strings*
		    collect (loop for c across row collect c))))

(defvar moves (loop for dir across (apply #'str:concat (nthcdr (1+ *h*) *input-as-list-of-strings*))
		    collect (position dir "^>v<")))

(defvar move-vecs (list '(-1 0) '(0 1) '(1 0) '(0 -1))) ; (y x)

(defun yx ()
  (loop named outer for y from 0 upto (- *h* 1)
	do (loop for x from 0 upto (- *w* 1)
		 do (if (eq (aref *ware* y x) #\@)
			(return-from outer (list y x))))))

(defun sum-of-box-gps ()
  (loop for y from 0 upto (- *h* 1)
	sum (loop for x from 0 upto (- *w* 1)
		  when (eq (aref *ware* y x) #\O)
		    sum (+ (* 100 y) x))))

;;; push one or more boxes starting @ y x in given dir
(defun push-boxes (y x dir)
  (let ((next-y y)
	(next-x x))
    (loop named outer while (eq (aref *ware* next-y next-x) #\O) ; keep pushing boxes
	  do (setf next-y (+ next-y (first (nth dir move-vecs))))
	     (setf next-x (+ next-x (second (nth dir move-vecs)))))
    (if (eq (aref *ware* next-y next-x) #\.)
	(progn (setf (aref *ware* next-y next-x) #\O) ; box(es) have room to shift
	       (setf (aref *ware* y x) #\.)
	       t)
	nil)))
		   
(defun apply-move (dir)
  (destructuring-bind (y x) (yx)
    (destructuring-bind (dest-y dest-x) (mapcar #'+ (list y x) (nth dir move-vecs))
      (let ((dest-contents (aref *ware* dest-y dest-x)))
	(cond ((eq dest-contents #\#)) ; do nothing @ wall
	      ((eq dest-contents #\.)  ; move into empty space
	       (setf (aref *ware* dest-y dest-x) #\@)
	       (setf (aref *ware* y x) #\.))
	      (t                       ; push box(es)
	       (when (push-boxes dest-y dest-x dir)
		 (setf (aref *ware* dest-y dest-x) #\@)
		 (setf (aref *ware* y x) #\.))))))))

(loop for m in moves
      do (apply-move m))

(format t "Part 1: ~a~%" (sum-of-box-gps))
