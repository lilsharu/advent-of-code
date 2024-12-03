(require 'uiop)
(load "utils.lisp")

(defun differences (values)
  "Return a list of differences between sequential numbers in VALUES."
  (loop for first in values
        for second in (cdr values)
        collect (- second first)))

(defun outside-range (value start end)
  (or (< value start) (> value end)))

(defun all-within-range (values start end)
  (reduce (lambda (result remaining)
            (and result (and (>= remaining start) (<= remaining end))))
          values
          :initial-value T))

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

(defun valid-report-nums (values)
  (let ((diffs (differences values)))
        (and (same-sign diffs) (all-within-range (abs-list diffs) 1 3))))

(defun remove-at-index (lst index)
  (append (subseq lst 0 index) (subseq lst (1+ index))))

(defun try-making-valid-with (values index)
  (let ((new-values (remove-at-index values index)))
    (valid-report-nums new-values)))

(defun loop-through-list-valid (values current)
  (if (>= current (length values))
    nil
    (if (try-making-valid-with values current)
      t
      (loop-through-list-valid values (1+ current)))))

(defun try-making-valid (values)
  (or (valid-report-nums values) (loop-through-list-valid values 0)))

(defun parse-report (report)
  (let ((values-str (split-by-space report)))
    (map 'list (lambda (x) (parse-integer x)) values-str)))

(defun valid-report (report)
  (let* ((values-str (split-by-space report))
         (values (map 'list (lambda (x) (parse-integer x)) values-str)))
    (try-making-valid values)))

(defun count-valid-reports ()
    (let* ((input (uiop:read-file-lines "day-02a.input"))
           (validities (map 'list (lambda (x) (valid-report x)) input)))
      (reduce (lambda (total next) (if next (+ total 1) total)) validities :initial-value 0)))

(format t "~a~%" (count-valid-reports))
