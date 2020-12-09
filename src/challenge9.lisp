(in-package :aoc2020)

(defparameter *input9* (read-file "src/input9"))

(defun find-bad-n (file)
  (with-open-file (s file)
    (let ((numbers nil))
      (loop :for n := (read-line s)
            :repeat 25 :do (push (parse-integer n) numbers))
      (loop :for n := (parse-integer (read-line s))
            :if (n-is-sum-of-any n numbers)
              :do (push n numbers)
                  (setf numbers (subseq numbers 0 25))
            :else :do (return n)))))

(defun n-is-sum-of-any (n list-of-ns)
  "Takes in a number N and checks if any of the numbers in LIST-OF-NS equals N"
  (loop :for (a . rest) :on list-of-ns       
          :thereis (loop :for b :in rest
                           :thereis (when (= (+ a b) n)
                                      (list a b)))))

(defun aoc-9a (file)
  (find-bad-n file))

(defun find-bad-ns (file)
  (with-open-file (s file)
    (let ((numbers (read-file file))
          (bad-n (find-bad-n file)))
      (find-sums-to-n bad-n numbers))))

(defun find-sums-to-n (n list-of-ns)
  (let ((current-ns nil))
    (loop :for (a . rest) :on list-of-ns
          :do (setf current-ns (list a))
            :thereis (loop :for b :in rest
                           :do (push b current-ns)
                             :thereis (when (= n (reduce #'+ current-ns))
                                        current-ns)))))

(defun aoc-9b (file)
  (let* ((ns (find-bad-ns file))
         (top (reduce #'max ns))
         (bot (reduce #'min ns)))
    (+ top bot)))
