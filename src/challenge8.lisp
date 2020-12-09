(in-package #:aoc2020)

(defun string->instruction (string)
  (multiple-value-bind (cmd args)
      (string-split string #\Space)
    (let ((sign (intern (string (aref args 0))))
          (cmd-sym (intern (string-upcase cmd)))
          (args (parse-integer (subseq args 1))))
      (list cmd-sym sign args 'not-executed))))

(defparameter *input8*
  (read-file "src/input8"
             :line-processor #'string->instruction
             :final-result 'vector))

(defun rf ()
  (read-file "src/input8"
             :line-processor #'string->instruction
             :final-result 'vector))

(defparameter *instructions* (make-hash-table))

(define-condition successful-execution ()
  ())

(define-condition borked-execution ()
  ())

(defclass environment ()
  ((accumulator
    :initform 0
    :accessor accumulator
    :type integer)
   (instruction-array
    :initarg :instruction-array
    :accessor instruction-array
    :type vector)
   (current-pos
    :initform 0
    :accessor current-pos
    :type integer)))


(defun new-instruction (key fun)
  (setf (gethash key *instructions*) fun))

(defun use-instruction (key args environment)
  (with-accessors ((arr instruction-array)
                   (pos current-pos))
      environment
    (destructuring-bind (sym n exep)
        args
      (declare (ignore exep))
      (setf (aref arr pos) (list key sym n 'EXECUTED))
      (funcall (gethash key *instructions*) args environment))))

(new-instruction 'JMP
                 (lambda (args environment)
                   (destructuring-bind (updown n executed)
                       args
                     (declare (ignore executed))
                     (let ((current-pos (current-pos environment)))
                       (setf (current-pos environment)
                             (reduce updown (list current-pos n)))
                       environment))))

(new-instruction 'NOP
                 (lambda (args environment)
                   (declare (ignore args))
                   (incf (current-pos environment))
                   environment))

(new-instruction 'ACC
                 (lambda (args environment)
                   (destructuring-bind (updown n executed)
                       args
                     (declare (ignore executed))
                     (setf (accumulator environment)
                           (reduce updown (list (accumulator environment) n)))
                     (incf (current-pos environment)))))

(defun new-environment (instructions)
  (make-instance 'environment
                 :instruction-array (map 'vector (lambda (l) l) instructions)))

(defun execute-instruction (environment)
  (with-accessors ((current-pos current-pos)
                   (instructions instruction-array)
                   (run-instructions run-instructions))
      environment
    (if (>= current-pos (length instructions))
        (error 'successful-execution)
        (let ((instruction (aref instructions current-pos)))
          (if (equal 'EXECUTED (first (last instruction)));;pt1
              (error 'borked-execution)
              (use-instruction (first instruction)
                               (rest instruction) environment))))))

(defun execute (instructions)
  (let ((env (new-environment instructions)))
    (handler-case (loop :while (execute-instruction env))
      (borked-execution ()
        (values env "Borked"))
      (successful-execution ()
        (values env "execution completed no problemo")))))

(defun aoc-8a (input)
  (execute input))

;;;part 2

(defun positions (ele seq &key (key #'first))
  "Searches SEQ for all instances of ELE using KEY as an argument to 'position'"
  (let ((pos (position ele seq :key key))
        (res nil))
    (when pos
      (let ((f pos))
        (loop :for x := (position ele seq :start (1+ pos) :key key)
              :if x
                :do (setf pos x)
                    (push x res)
              :else :do (return (cons f res)))))))

(defun new-sequence-pos-changed (original pos change-with)
  (let ((copy (map 'vector (lambda (l) l) original)))
    (setf (first (aref copy pos)) change-with)
    copy))

(defun generate-new-sequences (input positions change-with)
  (declare (ignore input))
  (mapcar (lambda (pos)
            (new-sequence-pos-changed input pos change-with))
          positions))

(defun all-new-sequences (input)
  (let* ((jmps (positions 'JMP input))
         (nops (positions 'NOP input))
         (jmps-r-nops (generate-new-sequences input jmps 'NOP))
         (nops-r-jmps (generate-new-sequences input nops 'JMP)))
    (list jmps-r-nops nops-r-jmps)))

(defun test-new-sequences (sequences)
  (remove-if (lambda (lst)
               (let ((second (second lst)))
                 (string-equal second "Borked")))
             (reduce (lambda (l y)
                       (concatenate 'list l y)) 
                     (map 'list (lambda (ele)
                                  (map 'list (lambda (ele)
                                               (multiple-value-bind (str env)
                                                   (execute ele)
                                                 (list str env)))
                                       ele))
                          sequences))))

(defun aoc-8b ()
  (accumulator (first (first (test-new-sequences (all-new-sequences (rf)))))))
