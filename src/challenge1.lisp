(defpackage #:aoc2020-1)
(in-package #:aoc2020-1)

(defparameter *input1* (read-file #P"src/input1"))


(defun equal-2020 (&rest numbers)
  (= (reduce #'+ numbers) 2020))

(defun all-equal-2020 (lst)
  (let ((multiples ()))
    (dolist (n lst (reverse multiples))
      (dolist (x (remove n lst :count 1))
        (when (equal-2020 n x)
          (push (* n x) multiples))))))

(defun all-equal-2020-pt2 (lst)
  (let ((multiples ()))
    (dolist (n lst (reverse multiples))
      (dolist (x (remove n lst :count 1))
        (dolist (y (remove x (remove n lst :count 1) :count 1))
          (when (equal-2020 n x y)
            (push (* n x y) multiples)))))))
;;this is just brute force and I get repeats of the same answers.
