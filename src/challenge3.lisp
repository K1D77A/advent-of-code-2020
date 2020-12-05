(in-package #:aoc2020)

(defparameter *input3* (read-file "src/input3"
                                  :line-processor #'string
                                  :final-result 'vector))

(defun wrap-around-string (string current-pos increase-by)
  "Takes in a string, the current position attempts returns a new position by 
wrapping around the string. If your string is 'abcdef' and you are currently at
position 2 (c) and wish to increase by 4, then you would end up back at 'a', so the
position 0 is returned."
  (let ((len (1- (length string))) ;account for 0 indexing 
        (new-pos (+ increase-by current-pos)))
    (if (> new-pos len)
        (let ((dif (1- (- new-pos len))));0 indexing
          dif)
        new-pos)))

(declaim (inline tree-at treep))

(defun treep (char)
  (char= char #\#))

(defun tree-at (string pos)
  (treep (aref string pos)))

(defun down-the-slope (slope start along down)
  (let* ((trees-hit (if (tree-at (elt slope 0) start) 1 0))
         (current-pos 0))
    (loop :for going-down :from down :upto (1- (length slope)) :by down
          :for string := (elt slope going-down)
          :do (setf current-pos (wrap-around-string string current-pos along))
              (when (treep (aref string current-pos))
                (incf trees-hit)))
    trees-hit))

(defun 3-a ()
  (down-the-slope *input3* 0 3 1))

(defun check-all-slopes (list-of-slopes)
  (mapcar (lambda (slope-list)
            (destructuring-bind (x y)
                slope-list
              (down-the-slope *input3* 0 x y)))
          list-of-slopes))

(defun 3-b ()
  (reduce #'* (check-all-slopes '((1 1)(3 1)(5 1)(7 1)(1 2)))))
