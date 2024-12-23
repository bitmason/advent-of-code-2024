;;;; Advent of Code 2024: Handy input utilities!
;;;; Darren Stone < dstone at bitmason dot com >
;;;;
;;;; I load this at start of each day-NN.lisp file to deliver some variables
;;;; and functions related to that day's problem or test input, creating various
;;;; data structures that might be useful. e.g. lists, arrays, integers, etc.

;;;; Input filename will be based on the day-NN in the source file name
;;;; and commandline args can override this for test input.
;;;;
;;;; Examples:
;;;; % ./day-01.lisp  ==>  Reading input from day-01-input.txt
;;;; % ./day-01.lisp test  ==>  Reading input from day-01-test.txt
;;;; % ./day-05-part-2.lisp  ==>  Reading input from day-05-input.txt
;;;; % ./day-05-part-2.lisp part-2  ==>  Reading input from day-05-part-2.txt
;;;; % ./day-03.lisp  -  ==>  Reading input from stdin

;;; typically "day-NN", derived from compiling/executing filename
(defvar *day-name* (cadr (str:rsplit "/" (uiop:split-name-type (uiop:getenv "_")) :limit 2)))
(if *day-name* (format t "~a~%" (str:upcase *day-name*)))
    
;;; argv[1] or "input" if not supplied (may be "-" if stdin requested)
(defvar *input-arg* (or (car (uiop:command-line-arguments)) "input"))

;;; e.g. "day-01-input.txt" or "day-05-test-3.txt" or "input.txt" or "stdin"
(defvar *input-filename*
  (if (equal *input-arg* "-")
      "stdin"
      (if *day-name*
	  (format nil "~a-~a.txt" *day-name* *input-arg*)
	  (format nil "~a.txt" *input-arg*))))

;;; Open file or stdin
(format t "Reading input from ~a~%" *input-filename*)
(defvar *input-stream*
  (if (equal *input-arg* "-")
      *standard-input*
      (open (str:concat *day-name* "-" *input-arg* ".txt")
	    :if-does-not-exist nil)))
(when (not *input-stream*)
  (format t "Can't open file~%")
  (uiop:quit 1))

;;; ===========================================================================
;;; Input parsed into an assortment of structures
;;; ===========================================================================

;;; All input in one string
(defvar *input-as-string*
  (if *input-stream*
      (uiop:slurp-stream-string *input-stream*)
      ""))

;;; List. Each line as a string.
(defvar *input-as-list-of-strings*
  (str:lines *input-as-string*))

(format t "Input contains ~a characters in ~a lines~%"
	(length *input-as-string*)
	(length *input-as-list-of-strings*))

;;; List. Each line as a list of strings, parsed with space or other separator.
(defun input-as-list-of-lists-of-strings (&key (separator " ") (omit-nulls t))
  (loop for line in *input-as-list-of-strings*
	collect (str:split separator line :omit-nulls omit-nulls)))

;;; List. Each line as a list of integers, parsed with space or other delimeter.
(defun input-as-list-of-lists-of-integers (&key (separator " ") (omit-nulls t))
  (loop for line in (input-as-list-of-lists-of-strings :separator separator :omit-nulls omit-nulls)
	collect (mapcar #'(lambda (n) (parse-integer n :junk-allowed t)) line)))

;;; The following make sense if the input can be parsed as a rectangular array.
(defvar *height* (length *input-as-list-of-strings*))
(defvar *width* (length (first *input-as-list-of-strings*)))

;;; 2D array. Row-major. Elements are chars.
(defun input-as-2d-array-of-chars ()
  (make-array (list *height* *width*) :initial-contents
 	      (loop for row in *input-as-list-of-strings*
 		    collect (loop for c across row collect c))))

;;; 2D array. Row-major. Elements are digits parsed to integers 0-9.
(defun input-as-2d-array-of-integers ()
  (make-array (list *height* *width*) :initial-contents
	      (loop for row in *input-as-list-of-strings*
		    collect (loop for n across row
				  collect (- (char-code n) (char-code #\0))))))

