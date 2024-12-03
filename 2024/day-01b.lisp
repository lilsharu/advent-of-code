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
       (diff '())
       (hashtab (make-hash-table)))
  (dolist (value values-num)
    (push (first value) left)
    (push (car (last value)) right))
  (setf left (sort left #'<))
  (setf right (sort right #'<))
  (dolist (value right)
    (setf (gethash value hashtab) (1+ (gethash value hashtab 0))))
  (setf curr (reduce
               (lambda (tot next) (+ tot (* next (gethash next hashtab 0))))
               left :initial-value 0))
  (format t "~a~%" curr))