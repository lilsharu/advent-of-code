(require 'uiop)
(load "utils.lisp")

(ql:quickload "cl-ppcre")

(defvar find_mul (ppcre:create-scanner "mul\\(\\d{1,3},\\d{1,3}\\)"))
(defvar find_num (ppcre:create-scanner "\\d{1,3}"))

(defun mul (values)
  (let* ((num-str (ppcre:all-matches-as-strings find_num values))
         (num-num (map 'list (lambda (x) (parse-integer x)) num-str)))
    (* (first num-num) (car (last num-num)))))

(let* ((input (uiop:read-file-lines "day-03.input"))
       (results '())
       (tot 0))
  (dolist (line input)
    (setf results (ppcre:all-matches-as-strings find_mul line))
    (dolist (match results)
      (setf tot (+ tot  (mul match)))))
    (format t "~a~%" tot))
