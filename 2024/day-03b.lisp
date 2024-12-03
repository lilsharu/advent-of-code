(require 'uiop)
(load "utils.lisp")

(ql:quickload "cl-ppcre")

(setq find_mul (ppcre:create-scanner "do\\(\\)|don't\\(\\)|mul\\(\\d{1,3},\\d{1,3}\\)"))
(setq find_num (ppcre:create-scanner "\\d{1,3}"))

(defun mul (values)
  (let* ((num-str (ppcre:all-matches-as-strings find_num values))
         (num-num (map 'list (lambda (x) (parse-integer x)) num-str)))
    (* (first num-num) (car (last num-num)))))

(defun process (current query)
  (if (string= "do()" query)
    (list (first current) t)
    (if (string= "don't()" query)
      (list (first current) nil)
      (if (not (car (last current)))
        current
        (list (+ (first current) (mul query)) t)))))

(let* ((input (uiop:read-file-lines "day-03.input"))
       (results '())
       (tot '(0 t)))
  (dolist (line input)
    (setf results (ppcre:all-matches-as-strings find_mul line))
    (setf tot (reduce (lambda (curr next)
              (process curr next)) results :initial-value tot)))
    (format t "~a~%" (first tot)))
