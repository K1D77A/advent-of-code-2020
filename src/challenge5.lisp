(in-package #:aoc-2020)

(defparameter *input5* (read-file "src/input5" :line-processor #'string))
;;can change :line-processor to #'execute-from-string and it'll calculate all of the
;;row columns while reading in.

(defparameter *instructions* (make-hash-table))

(defun new-instruction (char fun)
  (setf (gethash char *instructions*) fun))

(defun get-instruction (char)
  (gethash char *instructions*))

(defun lower-half (lower upper)
  (list lower (- upper (/ (- upper lower) 2))))

(defun upper-half (lower upper)
  (list (- upper (/ (- upper lower) 2)) upper))

(new-instruction #\F 'lower-half)
(new-instruction #\B 'upper-half)
(new-instruction #\R 'upper-half)
(new-instruction #\L 'lower-half)

(defun string-to-instructions (string)
  "Converts a string like 'FFFBBBFRRR' to a set of instructions"
  (let ((instructions (map 'list #'get-instruction string)))
    (list (subseq instructions 0 7) (subseq instructions 7))))

(defun execute-instructions (instruction-list res-name lower upper)
  "Takes a list of instructions and executes them starting with the arguments lower
upper"
  (loop :for instruction :in instruction-list
        :for (l u) := (apply instruction (list lower upper))
          :then (apply instruction (list l u))
        :finally (return (list res-name l u))))

(defun execute-from-string (string)
  (let ((instructions (string-to-instructions string)))
    (destructuring-bind (row column)
        instructions
      (let* ((row-val (execute-instructions row :row 0 128))
             (column-val (execute-instructions column :column 0 8)))
        (list  (round (second row-val))               
               (round (second column-val)))))))

(defun seat-id (row-column-list)
  (destructuring-bind (row column)
      row-column-list
    (+ column (* row 8))))

(defun aoc5-a ()
  (let* ((process-all (mapcar #'execute-from-string *input5*))
         (all-ids (mapcar #'seat-id process-all)))
    (reduce #'max all-ids)))

;;;part 2

(defun all-ids ()
  (let* ((process-all (mapcar #'execute-from-string *input5*))
         (all-ids (mapcar #'seat-id process-all)))
    all-ids))

(defun missing-number ()
  (let ((ids (sort (all-ids) #'<)))
    (loop :for (a b) :on ids
            :thereis (when (= (- b a) 2)
                       (1+ a)))))
