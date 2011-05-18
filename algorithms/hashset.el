;;; hashset.el --- hashset implementation using built-in emacs hashtables

;; Author: Fletcher Johnson <flt.johnson@gmail.com
;; Created: Winter 2010
;; Keywords: algorithm

;; create a hashset
(defun create-hash-set (element-list)
  (if (not (listp element-list))
      (message "please provide a proper element list")
    (let ((hashset (make-hash-table :test 'equal)))
      (while element-list
	(puthash (car element-list)  ;;key
		 (pop element-list)  ;;value
		 hashset))
      hashset)))



;;check whether an element exists in the hashset
(defun hashset-contains (element table)
  (if (gethash element table 'nil)
      't
    'nil))

;;put an element into the hashset
(defun hashset-put (element table)
  (puthash element element table))

;;test the hashset

;;elements to be inserted into the hashset
(setq elist '("things" "are" 3 4))
(setq table (create-hash-set elist))

(equal "things" (gethash "things" table))
(hashset-contains "things" table)
(hashset-put "apples" table)
(hashset-contains "apples" table)

;;; hashset.el ends here