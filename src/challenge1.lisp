(in-package #:aoc2020)

(defparameter *input1* (read-file "src/input1"))


(defun equal-2020 (&rest numbers)
  (= (reduce #'+ numbers) 2020))

(defun all-equal-2020 (lst)
  (dolist (i lst)
    (when (find (- 2020 i) lst :test #'=)
      (return (* (- 2020 i) i)))))

(defun all-equal-2020-pt2 (lst)
  (let ((multiples ()))
    (dolist (n lst (reverse multiples))
      (dolist (x lst)
        (dolist (y lst)
          (when (equal-2020 n x y)
            (push (* n x y) multiples)))))))
;;this is just brute force and I get repeats of the same answers.

;;;below is the best solution I found from Harlequin
;;;https://github.com/Harleqin/advent-of-code-2020/blob/master/1.lisp

(defun aoc-1a2 (&optional (ints *input1*))
  (loop :for (a . rest) :on ints      
          :thereis (loop :for b :in rest
                           :thereis (when (= (+ a b) 2020)
                                      (* a b)))))

(defun aoc-1b (&optional (ints *input1*))
  (loop :for (a . rest) :on ints
          :thereis (loop :for (b . rest) :on rest
                           :thereis (loop :for c :in rest
                                            :thereis (when (= (+ a b c) 2020)
                                                       (* a b c))))))
