#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 22
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

(defun mix (secret n) (logxor secret n))
(defun prune (secret) (mod secret 16777216))

(defun secret (this n)
  (if (zerop n)
      this
      (let ((new this))
	(setf new (prune (mix new (* new 64))))
	(setf new (prune (mix new (floor new 32))))
	(setf new (prune (mix new (* new 2048))))
	(secret new (- n 1)))))

(format t "Part 1: ~a~%" (loop for init in *input-as-list-of-strings*
			       sum (secret (parse-integer init) 2000)))

;;; TODO: Part 2
