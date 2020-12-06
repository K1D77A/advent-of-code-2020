(in-package #:aoc2020)

(defun conc-strings-in-list (list)
  (apply #'concatenate 'string list))

(defun aoc-6a (input)
  (reduce #'+
          (mapcar #'length 
                  (mapcar #'remove-duplicates 
                          (mapcar #'conc-strings-in-list
                                  (split-list-by
                                   (read-file input
                                              :line-processor #'string)
                                   (lambda (string) (string= string ""))))))))

(defun chars-in-common (list-of-strings)
  "Counts the characters that are in common in LIST-OF-STRINGS. Returns a list
of all the characters that are found to be in common."
  (let ((chars (make-hash-table :test #'equal))
        (res ()))
    (loop :for string :in list-of-strings
          :do (loop :for char :across string
                    :do (if (numberp (gethash char chars))
                            (incf (gethash char chars))
                            (setf (gethash char chars) 1))))
    (maphash (lambda (key val)               
               (when (>= val (length list-of-strings))
                 (push key res)))
             chars)
    res))

(defun aoc-6b (input)
  (reduce #'+
          (mapcar #'length 
                  (mapcar #'chars-in-common 
                          (split-list-by
                           (read-file input
                                      :line-processor #'string)
                           (lambda (string) (string= string "")))))))


