(require 'uiop)
(load "utils.lisp")

(defun differences (values)
  "Return a list of differences between sequential numbers in VALUES."
  (loop for first in values
        for second in (cdr values)
        collect (- second first)))

(defun all-positive (values) 
  "Determines if all the values in a list are positive"
  (reduce (lambda (result remaining) (and result (> remaining 0))) 
          values
          :initial-value t))

(defun all-negative (values) 
  "Determines if all the values in a list are negative"
  (reduce (lambda (result remaining) (and result (< remaining 0)))
          values
          :initial-value t))

(defun same-sign (values)
  (or (all-positive values) (all-negative values)))

(defun abs-list (values)
  (map 'list (lambda (x) (abs x)) values))

(defun all-within-range (values start end)
  (reduce (lambda (result remaining)
            (and result (and (>= remaining start) (<= remaining end))))
          values
          :initial-value T))

(defun valid-report (report)
  (let* ((values-str (split-by-space report))
         (values (map 'list (lambda (x) (parse-integer x)) values-str))
         (diffs (differences values))
         (sign (same-sign diffs))
         (range (all-within-range (abs-list diffs) 1 3)))
    (and sign range)))

(defun count-valid-reports ()
    (let* ((input (uiop:read-file-lines "day-02a.input"))
           (validities (map 'list (lambda (x) (valid-report x)) input)))
      (reduce (lambda (total next) (if next (+ total 1) total)) validities :initial-value 0)))

(format t "~a~%" (count-valid-reports))
