#!/usr/bin/env -S sbcl --core setup.core --script

;;;; Advent Of Code 2024: Day 12
;;;; Solution by Darren Stone <dstone at bitmason dot com>

(load "input.lisp")

(defvar garden (input-as-2d-array-of-chars))

(defun sethash (h k v) (setf (gethash k h) v))
(defun inchash (h k &optional (v 1)) (sethash h k (+ v (gethash k h 0))))
(defun dechash (h k &optional (v 1)) (sethash h k (- (gethash k h 0) v)))

(defparameter areas (make-hash-table :test 'equal))
(defparameter perims (make-hash-table :test 'equal))

(defparameter regions (make-hash-table :test 'equal))

;;; key (for regions, areas, & perim hashes) of form: (crop y-origin x-origin) or nil
(defun region-key (y x) 
  (let ((crop (aref garden y x)))
    (loop for (c yo xo) being the hash-keys in regions using (hash-value cells)
	  finally (return nil)
	  if (and (eq c crop)
		  (position (list y x) cells :test 'equal))
	     return (list c yo xo))))
     
(defun in-bounds-p (y x) (and (>= y 0) (< y *height*) (>= x 0) (< x *width*)))

;;; list of adj cells
(defun adj (y x)  
  (list (list (- y 1) x)
	(list y (1+ x))
	(list (1+ y) x)
	(list y (- x 1))))

;;; adjacent only prev in scanning order (for first pass checks)
(defun adj-prev (y x) 
  (list (list (- y 1) x)
	(list y (- x 1))))
  
(defun perim-count (y x)
  (let ((crop (aref garden y x)))
    (loop for (ay ax) in (adj y x)
	  count (or (and (in-bounds-p ay ax)
			 (not (eq (aref garden ay ax) crop)))
		    (not (in-bounds-p ay ax))))))

;;; t iff any cell in either region is adj to any cell in the other, independent of crops
(defun regions-adj-p (k1 k2)  
  (let ((k1-cells (gethash k1 regions))
	(k2-cells (gethash k2 regions)))
    (loop named outer for (cy1 cx1) in k1-cells
	  finally (return-from outer nil)
	  do (loop for ayx in (adj cy1 cx1)
		   do (when (position ayx k2-cells :test 'equal)
			(return-from outer t))))))

;;; merge two regions (& areas & perims) -- crop is already assumed to match
(defun regions-merge (k1 k2)  
  (sethash regions k1 (append (gethash k1 regions) (gethash k2 regions)))
  (remhash k2 regions)
  (inchash areas k1 (gethash k2 areas 0))
  (remhash k2 areas)      
  (inchash perims k1 (gethash k2 perims 0))
  (remhash k2 perims))

;;; first pass to create: regions, areas, perims
(loop for y from 0 upto (- *height* 1)
      do (loop for x from 0 upto (- *width* 1)
	       do (let* ((crop (aref garden y x))
			 (rkey (region-key y x)))   ; existing?
		    (when (not rkey)
		      (loop for (ay ax) in (adj-prev y x)
			    do (when (and (in-bounds-p ay ax)
					(eq (aref garden ay ax) crop))
				   (setf rkey (region-key ay ax))
				   (sethash regions rkey (cons (list y x) (gethash rkey regions))))) ; existing; connect
		      (when (not rkey) ; still not connected, so new
			(setf rkey (list crop y x)) 
			(sethash regions rkey (list (list y x)))))
		    (inchash areas rkey)
		    (inchash perims rkey (perim-count y x)))))

;;; merge adjacent regions from first pass
(let ((to-do t))
  (loop while to-do
	do (setf to-do nil)
	   (loop named outer for k1 being the hash-keys in regions
		 do (loop for k2 being the hash-keys in regions
			  do (when (and (not (equal k1 k2))
					(eq (first k1) (first k2))
					(regions-adj-p k1 k2))
			       (regions-merge k1 k2)
			       (setf to-do t)
			       (return-from outer))))))

(format t "Part 1: ~a~%"
	(loop for region being the hash-keys in areas
		using (hash-value area)
	      sum (* area (gethash region perims))))

;;; TODO: Part 2
