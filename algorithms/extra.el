;;; extra.el --- extra misc functions and vars

;; Author: Fletcher Johnson <flt.johnson@gmail.com>
;; Created: May 15th 2011
;; Keywords: algorithm

;;; Code:
(provide 'extra)

(defun default-comparator (e1 e2)
  "A comparision function for integers"
  (cond ((< e1 e2) -1)	
	((= e1 e2)  0)	
	((> e1 e2)  1)))

(defun string-comparator-function (s1 s2)
  "A comparision function for strings"
  (cond ((string< s1 s2) -1)
	((string-equal s1 s2) 0)
	('t 1)))

(defun random-letter-string-list (words-to-add)
  "Generate a list, WORDS-TO-ADD long, of random words composed only of letters."
  (if (= 0 words-to-add) '()

    (setq string-len (+ 3 (random 11)))
    (setq char-list '())
  
    (while (> string-len -1)
      (if (= (random 2) 0) 
	  (setq char-list (cons (+ (random 26) 65) char-list))
	(setq char-list (cons (+ (random 26) 97) char-list)))
      (setq string-len (- string-len 1)))

    (cons (apply 'string char-list) 
	  (random-letter-string-list (- words-to-add 1)))))

(random-letter-string-list 3)    

(defun random-num-list (numbers-to-add)
  "Generate a list NUMBERS-TO-ADD in length with random ints."
  (if (<= numbers-to-add 0) '()
    (cons (random 15) 
	 (random-num-list (- numbers-to-add 1)))))
(random-num-list 15)

;;; extra.el ends here