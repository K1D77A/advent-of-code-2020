(in-package :aoc2020)

(defun read-file (path &key (line-processor #'parse-integer)
                         (final-result 'list))
  (let ((res nil))
    (with-open-file (s path)
      (coerce (loop :for line := (read-line s nil nil)
                    :while line
                    :do (push (funcall line-processor line) res)
                    :finally (return (reverse res)))
              final-result))))
