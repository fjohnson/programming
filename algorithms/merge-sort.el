;;; merge-sort.el --- mergesort implementation 

;; Author: Fletcher Johnson <flt.johnson@gmail.com>
;; Created: May 14th 2011 
;; Keywords: algorithm

;;; Commentary:
;;  Merge sort algorithm written in emacs lisp
;;  This sort is based on integers and is not generic
;;  It may not be the full speed it should actually be because
;;  I may be using certain lisp functions that are copying and creating
;;  new lists. I haven't checked this.

;;; Code:
(provide 'merge-sort)

(defun merge-sort (S &optional comp-func)
  "The merge sort function. A list is split in half, the greater
of which is assigned to the left half. This process repeats
until the lists are of size 1. After splitting you are left with
two sorted lists which can be combined in linear time. Total speed
is then the time to merge plus the number of splits = O(n*logn)
An optional comparison function can be specified to determine the
sort order. Otherwise the default comparision is assumed to be on
integers."

  ;;Define a default comparison function if one is not given.
  
  (if (null comp-func) (progn (require 'extra)
			      (setq comp-func 
				    (symbol-function 'default-comparator))))

  (let ((len (length S)))
    (if (= len 1) S
      (merge 	
					;split the list
       (merge-sort 			;and merge the halfs
	(butlast S  (/ (+ len 1) 2))
	comp-func)

       (merge-sort
	(nthcdr (/ len 2) S)
	comp-func)

       comp-func))))

(defun merge (s1 s2 comp-func)
  "Combine two sorted lists into one."
  (let ((S '()))
    (while (and (not (null s1))
		(not (null s2)))
      (let ((e1 (car s1))
	    (e2 (car s2)))

	(if (<= (funcall comp-func e1 e2) 0)
	    (progn (setq e e1)
		   (pop s1))
	  (progn (setq e e2)
		 (pop s2)))

	(if (null S)
	    (progn (setq S (list e))
		   (setq end S))
	  (setq end 
		(setcdr end (list e))))))

    ;;Insert any remaining items from the larger of the two lists.
    (while (not (null s1))		
      (setq end (setcdr end (list (pop s1)))))
    (while (not (null s2))
      (setq end (setcdr end (list (pop s2)))))
    S))

;;Optional Test code

(merge-sort '(1))
(merge-sort '(1 2))
(merge-sort '(1 2 3 4))
(merge-sort '(3 1 6 2))
(merge-sort '(9 4 6 1 0 9 15 -3 2))

;(merge-sort '("give" "a" "dog" "a" "bone" "a" "day") 'string-comparator-function)
;;; merge-sort.el ends here