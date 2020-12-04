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

(defun string-split (string character)
  "split STRING at CHARACTER"
  (loop :for (char . rest) :on (coerce string 'list)
        :when (char= char character)
          :do (return (values (to-string res) (to-string rest)))
        :collect char :into res))

(defun split-list-by (list split-by-pred)
  "Takes in a LIST and a SPLIT-BY-PRED, then loops over the list collecting all
the elements that are separated by (funcall split-by-pred list-ele)"
  (let ((splits nil)
        (current-split nil))
    (loop :for ele :in list 
          :if (funcall split-by-pred ele)
            :do             
               (push current-split splits)
               (setf current-split nil)
          :else :do (push ele current-split)
          :finally (push current-split splits))
    splits))
