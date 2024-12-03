(require 'uiop)
(load "utils.lisp")

(defun parse-integer-list (list)
  (map 'list (lambda (x) (parse-integer x)) list))

(let* ((lines (uiop:read-file-lines "day-01a.input"))
       (values-str (map 'list (lambda (x) (split-by-space x)) lines))
       (values-num (map 'list (lambda (x) (parse-integer-list x)) values-str))
       (curr 0)
       (left '())
       (right '())
       (diff '()))
  (dolist (value values-num)
    (push (first value) left)
    (push (car (last value)) right))
  (setf left (sort left #'<))
  (setf right (sort right #'<))
  (setf diff (loop for l in left
        for r in right
        collect (abs (- r l))))
  (setf diff (reduce (lambda (tot next) (+ tot next)) diff :initial-value 0))
  (format t "~a~%" diff))
