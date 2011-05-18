;;; set-sorted-creation.el --- integer set implementation derived from sorted sets

;; Author: Fletcher Johnson <flt.johnson@gmail.com>
;; Created: May 14 2011
;; Keywords: algorithm

;;; Code:
(defun simple-set (s1 s2 op &optional comp-func)
"Create a set by from s1 and s2 by sorting them and then combining the result.

You can create a set from two sorted lists in linear time.
This set can be created through the union, intersection or subtraction
of the two lists (sets themselves in their own right). 

Inputs:
op -> A string being one of \"i\" for intersection, \"u\" for union,  
      and \"s\" for subtraction
s1,s2 -> Two lists of integers which may be unsorted.
comp-func -> An optional comparision function. "

  ;;Set up a default comparator
  (if (null comp-func) (progn (require 'extra)
  			      (setq comp-func 
				    (symbol-function 'default-comparator)))) 

  (fset 'sort-func (or (require 'quick-sort)
		       (require 'merge-sort)))
  
  (setq s1 (sort-func s1 comp-func))		
  (setq s2 (sort-func s2 comp-func))

  (defun set-last (e)
    (setq last (setcdr last (list e))))
  
  (let* ( (set '(()))
	  (last set))
    (while (and s1 s2)			
      (cond ((= (funcall comp-func 
			 (car s1)
			 (car s2)) 0) (if (or (string= op "i")
					    (string= op "u")) (progn (set-last (pop s1))
								    (pop s2))
				      (progn (pop s1) (pop s2))))
	    ((< (funcall comp-func 
			 (car s1)
			 (car s2)) 0) (if (or (string= op "u")
					   (string= op "s")) (set-last (pop s1))
				     (pop s1)))
	    (t (if (string= op "u") (set-last (pop s2))
		 (pop s2)))))

    ;; Any remaining elements from the shorter of the two lists should be added
    ;; to our new list if the operation is a union.
    (if (string= op "u")
	(let ((remainder (or s1 s2)))
	  (while remainder
	    (setq last (setcdr last (list (pop remainder)))))))
    (cdr set)))

;;Optional Test code
(require 'extra)
(simple-set '(1 2 3) '(2 8) "i")     
(simple-set '(1 2 3) '(2 8) "u" 'default-comparator)
(simple-set '(1 2 3) '(2 8) "s" 'default-comparator)
(simple-set '("abba" "gold") '("was" "gold") "u" 'string-comparator-function)

;;; filename ends here