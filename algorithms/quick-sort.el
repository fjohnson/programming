;;; quick-sort.el --- quicksort implementation

;; Author: Fletcher Johnson <flt.johnson@gmail.com>
;; Created: May 15th 2011
;; Keywords: algorithm

;;; Commentary:
;; Quick sort algoritm written in emacs lisp

;;; Code:
(provide 'quick-sort)

(defun quick-sort (S &optional comp-func)
    
"Sort a sequence S using Quick Sort and the optional comparator comp-func.
comp-func should take two args a1 and a2 and return l.t 0, 0, or
g.t 0 if a1 is less than, equal to or greater than a2
respectively. If an optional comparator is not given then the
default comparison is based on assumed ints.

Quick sort works by first selecting an element (optionally
random) in the sequence. This element, the pivot, is then used to
separate the sequence into two sequences which contain all the
elements less than the pivot and all the elements greater than
the pivot. This process is applied recursively on the sequences
until the sequences are both of size 1. On return from a
recursive application the sequences are sorted and the result of
the two sequences are combined with the pivot to produce the
final sorted result."

;;Define a default comparison function if one is not given.
  (if (null comp-func) (progn (require 'extra)
  			      (setq comp-func 
				    (symbol-function 'default-comparator))))
  

  (if (null (cdr S)) S ;;list has a single element
    
    (let ( (pivot (list (pop S))) ;;recurse because length(list) > 1
	   (geset '())
	   (leset '()) )
      (while (not (null S))
	(let ((e (pop S)))
	  (cond ((< (funcall comp-func e (car pivot)) 0)
		 (setq leset 
		       (cons e leset)))

		((= (funcall comp-func e (car pivot)) 0)
		 (setq pivot 
		       (cons e pivot)))

		((> (funcall comp-func e (car pivot)) 0)
		 (setq geset
		       (cons e geset))))))
      (append (quick-sort leset comp-func) 
	      (append pivot (quick-sort geset comp-func))))))

;;Optional Test code
;;(require 'extra)

;;Sort a random list of numbers
;;(setq llst (random-num-list 10))
;;(quick-sort llst)
;;llst

;;Sort a random list of strings
;;(and (setq llst (random-letter-string-list 5))
;;     (quick-sort llst 'string-comparator-function))
;;llst	  
	
;;; quick-sort.el ends here	    
