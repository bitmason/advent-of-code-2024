;;;; Load this at start of each day-NN.lisp file.
;;;; Provides several globals including *input* and *input-lines* based
;;;; on NN and optional command line args.
;;;;
;;;; Examples of commandline usage and the stream that will be read to
;;;; provide a list of strings as input to the solver.
;;;;
;;;; % ./day-01.lisp
;;;; Reading input from day-01-input.txt
;;;;
;;;; % ./day-01.lisp test
;;;; Reading input from day-01-test.txt
;;;;
;;;; % ./day-05-part-2.lisp
;;;; Reading input from day-05-input.txt
;;;;
;;;; % ./day-05-part-2.lisp part-2
;;;; Reading input from day-05-part-2.txt
;;;;
;;;; % ./day-03.lisp -
;;;; Reading input from stdin

;;; typically "day-NN", derived from compiling/executing filename
(defvar *day-name* (cadr (str:rsplit "/" (uiop:split-name-type (uiop:getenv "_")) :limit 2)))
    
;;; argv[1] or "input" if not supplied (may be "-" if stdin requested)
(defvar *input-arg* (or (car (uiop:command-line-arguments)) "input"))

;;; e.g. "day-01-input.txt" or "day-05-test-3.txt" or "input.txt" or "stdin"
(defvar *input-filename*
  (if (equal *input-arg* "-")
      "stdin"
      (if *day-name*
	  (format nil "~a-~a.txt" *day-name* *input-arg*)
	  (format nil "~a.txt" *input-arg*))))

(if *day-name* (format t "~a~%" (str:upcase *day-name*)))

(format t "Reading input from ~a~%" *input-filename*)

;;; Open file or stdin
(defvar *input-stream*
  (if (equal *input-arg* "-")
      *standard-input*
      (open (str:concat *day-name* "-" *input-arg* ".txt")
	    :if-does-not-exist nil)))
(when (not *input-stream*)
  (format t "Can't open file~%")
  (uiop:quit 1))

;;; All input in one string
(defvar *input*
  (if *input-stream*
      (uiop:slurp-stream-string *input-stream*)
      ""))

;;; List of strings read from file or stdin
(defvar *input-lines*
  (str:lines *input*))

(format t "Input contains ~a characters in ~a lines~%" (length *input*) (length *input-lines*))
