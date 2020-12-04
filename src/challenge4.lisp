(defpackage #:aoc2020-4)
(in-package #:aoc2020-4)

(defparameter *input4*
  (read-file "src/input4" :line-processor #'string))

(defvar *pt2validators* (make-hash-table))

(defparameter *fields*;;can accept an arbitrary number of validators.
  '((:BYR T byrvalid)
    (:IYR T iyrvalid)
    (:EYR T eyrvalid)
    (:HGT T hgtvalid)
    (:HCL T hclvalid)
    (:ECL T eclvalid)
    (:PID T pidvalid)
    (:CID nil)))

(defun new-validator (key fun)
  (setf (gethash key *pt2validators*) fun))

(defun call-validator (key arg)
  (funcall (gethash key *pt2validators*) arg))

(defun field-p (string)
  "Checks if string is a field. If it contains a space it isn't."
  (unless (find #\Space string :test #'char=)
    (= 1 (count #\: string :test #'char=))))

(defun multiple-fields-p (field-string)
  "Checks if there are multiple key-vals in string by looking for more than 1 
instance of #\:"
  (when (find #\Space field-string :test #'char=)
    (/= 1 (count #\: field-string :test #'char=))))

(defun collect-passports (list)
  (split-list-by list (lambda (ele) (string= "" ele))))

(defun key-val (field-string)
  "Given a FIELD-STRING like 'abc:def' returns a list like '(:ABC 'def'), or nil"
  (when (field-p field-string)
    (multiple-value-bind (key val)
        (string-split field-string #\:)
      (list (intern (string-upcase key) :keyword) val))))

(defun get-key-vals (multiple-fields-in-string)
  "Given a string (MULTIPLE-FIELDS-IN-STRING) with each entry
divided by #\Space, return an alist of all the key value pairs."
  (do ((string multiple-fields-in-string)
       (key-vals ()))
      ((null string)
       (return key-vals))
    (if (multiple-fields-p string)
        (multiple-value-bind (field remainder)
            (string-split string #\Space)
          (push (key-val field) key-vals)
          (setf string remainder))
        (progn (push (key-val string) key-vals)
               (setf string nil)))))

(defun passport-list-to-key-vals (passport-list)
  "Given a list of passport-field/s collects them all into an alist of their key
vals"
  (let ((key-vals nil))
    (dolist (string passport-list key-vals)
      (if (multiple-fields-p string)
          (setf key-vals (append key-vals (get-key-vals string)))
          (push (key-val string) key-vals)))))

(defun all-validate-p (ele list-of-validators)
  "Calls all of the validators listed in LIST-OF-VALIDATORS with ele. Returns t if
all validate, nil otherwise"
  (notany #'null (mapcar (lambda (validator)
                           (call-validator validator ele))
                         list-of-validators)))

(defun valid-passport-p (passport-alist &optional (validate nil))
  "Takes in a passport-alist and checks if it is valid by comparing its key names
to the key names in *fields*, only checks if the the key is designated as required"
  (loop :for (key requiredp . validators) :in *fields*        
        :if requiredp
          :do (let ((key-val (assoc key passport-alist)))
                (if key-val
                    (when validate
                      (unless (all-validate-p (second key-val) validators)
                        (return nil)))
                    (return nil)))
        :finally (return t)))

(defun aoc4-a (input)
  (count t (mapcar #'valid-passport-p
                   (mapcar #'passport-list-to-key-vals
                           (collect-passports input)))))


;;;part 2

(new-validator 'byrvalid
               (lambda (entry)
                 (let ((parsed (parse-integer entry :junk-allowed t)))
                   (when parsed 
                     (and (= (length entry) 4)
                          (<= 1920 parsed 2002))))))

(new-validator 'iyrvalid
               (lambda (entry)
                 (let ((parsed (parse-integer entry :junk-allowed t)))
                   (when parsed 
                     (and (= (length entry) 4)
                          (<= 2010 parsed 2020))))))

(new-validator 'eyrvalid
               (lambda (entry)
                 (let ((parsed (parse-integer entry :junk-allowed t)))
                   (when parsed 
                     (and (= (length entry) 4)
                          (<= 2020 parsed 2030))))))

(new-validator 'hgtvalid
               (lambda (entry)
                 (multiple-value-bind (num pos)
                     (parse-integer entry :junk-allowed t)
                   (let ((unit (subseq entry pos)))
                     (cond ((string-equal unit "cm")
                            (<= 150 num 193))
                           ((string-equal unit "in")
                            (<= 59 num 76))
                           (t nil))))))

(defun any-of (ele list-of-potential &key (test #'equal))
  "checks if ele is any of list-of-potential."
  (find t
        (map 'list (lambda (item)
                     (funcall test item ele))
             list-of-potential)))

(defun string-to-charcodes (string)
  (map 'list #'char-code string))

(defvar *hex-charcodes* (string-to-charcodes "abcdef0123456789"))

(defun valid-hex-p (string)
  (when (= (length string) 7)
    (let ((str (subseq string 1)))
      (find t (loop :for ele :across (string-downcase str)
                    :collect (any-of (char-code ele) *hex-charcodes*))))))

(new-validator 'hclvalid
               (lambda (entry)
                 (valid-hex-p entry)))

(new-validator 'eclvalid (lambda (entry)
                           (any-of entry
                                   '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")
                                   :test #'string=)))

(new-validator 'pidvalid (lambda (entry)
                           (when (= (length entry) 9)
                             (handler-case (parse-integer entry)
                               (condition () nil)))))


(defun aoc4-b (input)
  (count t (mapcar (lambda (list)
                     (valid-passport-p list t))
                   (mapcar #'passport-list-to-key-vals
                           (collect-passports input)))))







