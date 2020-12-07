;;;; aoc2020.asd

(asdf:defsystem #:aoc2020
  :description "advent of code 2020"
  :author "k1d77a"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "helpers")
               (:file "challenge1")
               (:file "challenge2")
               (:file "challenge3")
               (:file "challenge4")
               (:file "challenge5")
               (:file "challenge6")
               (:file "challenge7")))
