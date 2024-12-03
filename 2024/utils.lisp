(defun split-by-space (input-string)
  "Splits INPUT-STRING into a list of words separated by spaces, retaining empty strings."
  (let ((words '())
        (start 0)
        (length (length input-string)))
    (loop for i from 0 to length do
          (when (or (= i length) (char= (char input-string i) #\Space))
            (push (subseq input-string start i) words)
            (setf start (1+ i))))
    (nreverse words)))

