(require 'uiop)
(load "utils.lisp")

#| 
We want to be able to loop through the thing in multiple ways: by rows, by
columns, by left diagonal, and by right diagonal (in both directions)
|#

(defun rest-of (list num) 
  (if (= num 0)
    list
    (rest-of (car list) (1- num))))

(defun count-xmas (string) 
  (if (< (length string) 4)
    0
    (+ (if (string= (subseq string 0 4) "XMAS") 1 0) (count-xmas (subseq string 1)))))

(defun count-xmas-list-forward (list)
  (reduce (lambda (tot next) (+ tot (count-xmas next))) list :initial-value 0))

(defun count-xmas-list-bidirectional (list)
  (+ (count-xmas-list-forward list) (count-xmas-list-forward (map 'list (lambda (x) (reverse x)) list))))

(defun listify (string)
  (if (= (length string) 0)
    '()
    (append (list (subseq string 0 1)) (listify (subseq string 1)))))

(defun piecewise-merge (a b)
  (if (not (= (length a) (length b))) (error "Lengths of lists must be the same") ())
  (if (= (length a) 0)
    '()
    (append
      (list (concatenate 'string (first a) (first b)))
      (piecewise-merge (cdr a) (cdr b)))))

(defun piecewise-merge-list (list)
  (reduce (lambda (cur next) (piecewise-merge cur next)) list))

(defun make-column (list)
  (piecewise-merge-list (map 'list (lambda (x) (listify x)) list)))

;; shift each list left or right by the row it is in to get the diagonal


(let ((input (uiop:read-file-lines "day-04.input")))
      (format t "~a~%" (count-xmas-list-bidirectional input)))
