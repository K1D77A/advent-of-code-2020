(defpackage #:aoc-2020-2)
(in-package #:aoc2020-2)

(defparameter *input2* (read-file #P"src/input2"
                                  :line-processor #'string))

(defun between (n &rest args)
  (apply #'<= (first args) (append (list n) (rest args))))

(defun to-string (list)
  (coerce list 'string))

(defun create-validator (a b check pass)
  (list a b check pass))

(defun get-inbetween (string)
  (multiple-value-bind (inbetween remain)
      (string-split string #\Space)
    (multiple-value-bind (a b)
        (string-split inbetween #\-)
      (values (parse-integer a) (parse-integer b) remain))))

(defun generate-validator-from-form (password-form)
  (multiple-value-bind (a b remain)
      (get-inbetween password-form)
    (multiple-value-bind (string-to-count remainder)
        (string-split remain #\:)
      (multiple-value-bind (space remainder)
          (string-split remainder #\Space)
        (declare (ignore space))
        (create-validator a b (char string-to-count 0) remainder)))))

(defun validate-all-with-fun (list-of-password-forms fun)
  (let ((correct 0))
    (dolist (entry list-of-password-forms correct)
      (when (funcall fun entry)
        (incf correct)))))

(defmacro create-password-validator (name &body body)
  "this macro defines function that let you validate a password-form.
The macro creates a function named by NAME that takes 1 argument, a password-form.
within BODY there are 4 bound variables, a b char and string."
  (let ((form (gensym)))
    `(defun ,name (,form)
       (destructuring-bind (a b char string)
           (generate-validator-from-form ,form)
         ,@body))))

(create-password-validator valid-password-pt1-p
  (between (count char string) a b))

(create-password-validator valid-password-pt2-p
  (or (and (char/= (aref string (1- a)) char)
           (char= (aref string (1- b)) char))
      (and (char= (aref string (1- a)) char)
           (char/= (aref string (1- b)) char))))

;;(validate-all-with-fun *input2* #'valid-password-pt1-p)
;;(validate-all-with-fun *input2* #'valid-password-pt2-p)








