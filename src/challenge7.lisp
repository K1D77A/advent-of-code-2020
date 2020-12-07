(in-package :aoc2020)

(defparameter *input7*
  (mapcar #'parsed-string-to-useable-rule
          (mapcar #'clean-up-list 
                  (mapcar #'parse-rulestring-to-list 
                          (read-file "src/input7"
                                     :line-processor #'string)))))

(defparameter *rules* (make-hash-table))

(defun new-rule (key list)
  (setf (gethash key *rules*) list))

(new-rule 'first-bag '(#\Space #\Space))
(new-rule 'blank '(#\Space))
(new-rule 'first-contain '(#\Space #\Space #\Space))
(new-rule 'comma-contain '(#\, #\Space))
(new-rule 'period-contain '(#\.))

(defun compose-ruleset (rules)
  (loop :for rule :in rules
        :collect (gethash rule *rules*)))

(defun parse-string (string rules)
  (let ((result ())
        (string string))
    (dolist (rule rules (values
                         (remove "" (nreverse result) :test #'string=) string))
      (multiple-value-bind (res remain)
          (string-split string rule)
        (push res result)
        (setf string remain)))))

(defun parse-with-ruleset (string ruleset)
  (let ((result ())
        (string string))
    (dolist (rule ruleset (append (nreverse result) (list (list string))))
      (multiple-value-bind (res rest)
          (parse-string string rule)
        (setf string rest)
        (push res result)))))

(defun compose-ruleset-from-rule-string (string)
  (let* ((default '(first-bag blank blank first-contain blank))
         (commas (count #\, string)))
    (compose-ruleset (append default (loop :repeat (1- commas)
                                           :collect 'comma-contain)))))

(defun parse-rulestring-to-list (string)
  (let* ((ruleset (compose-ruleset-from-rule-string string))
         (list (parse-with-ruleset string ruleset)))
    list))

(defun clean-up-list (list)
  (loop :for i :in list
        :if (or (/= 1 (length i))
                (/= 0 (count #\Space (first i))))
          :collect i))

(defun fix-bag (list)
  (concatenate 'string (first list) " " (second list)))

(defun fix-contain (list)
  (let ((res ()))
    (labels ((rec (string)
               (multiple-value-bind (r re)
                   (string-split string #\Space)
                 (if (null r)
                     nil
                     (progn (push r res)
                            (rec re))))))
      (rec (first list))
      (nreverse res))))

(defun to-rule (list)
  (destructuring-bind (n x y)
      list
    (unless (string= n "no")
      (list (parse-integer n) (concatenate 'string x " " y)))))

(defun parsed-string-to-useable-rule (list)
  (let ((bag (fix-bag (first list)))
        (contains (mapcar #'fix-contain (rest (rest list)))))
    (append (list (list bag))
            (mapcar #'to-rule  (append (list (first (rest list))) contains)))))

(defun bag-contains (string)
  (let ((list (find string *input7* :key (lambda (ele )
                                           (first (first ele)))
                                    :test #'string=)))
    (remove-if #'null (mapcar #'second list))))

(defun bag-contents-from-bag (bag)
  (mapcar (lambda (lst) (second lst)) (rest bag)))

(defun shiny-gold-p (string)
  (string= string "shiny gold"))

(defun contains-shiny-gold-p (list)
  (some #'shiny-gold-p list))

(defun bag-contains-contains (string)
  (reduce #'append (mapcar #'bag-contains (bag-contains string))))

(defun compute-bag-contains (bag)
  (let ((start (bag-contents-from-bag bag))
        (searched-for ())
        (gold 0))
    (labels ((rec (list)
               (let ((contains
                       (reduce #'append
                               (mapcar
                                (lambda (string)
                                  (if (find string searched-for
                                            :test #'string=)
                                      nil
                                      (progn (push string searched-for)
                                             (when (string= string "shiny gold")
                                               (setf gold 1))
                                             (bag-contains string))))
                                list))))
                 (if contains
                     (rec contains)
                     nil))))
      (rec start)
      gold)))

(defun aoc-7a ()
  (reduce #'+ (mapcar #'compute-bag-contains *input7*)))


;;;part 2

(defun bag-contains-count (string)
  (let ((list (find string *input7* :key (lambda (ele )
                                           (first (first ele)))
                                    :test #'string=)))
    (remove-if #'null (rest list))))

(defun total-number-of-bags-within (string)
  (labels ((rec (list acc)
             (cond ((null list)
                    acc)
                   ((numberp (car list))
                    (rec (bag-contains-count (second list))
                         (* acc (car list))))
                   ((listp list)
                    (+ (rec (car list) acc)
                       (rec (cdr list) acc))))))
    (1-(rec (bag-contains-count string) 1))))
