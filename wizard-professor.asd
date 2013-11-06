;;;; wizard-professor.asd

(asdf:defsystem #:wizard-professor
  :serial t
  :description "A spell-checker"
  :author "Tim Macdonald <tsmacdonald@gmail.com>"
  :license "MIT License"
  :depends-on (#:cl-ppcre #:fare-memoization #:alexandria)
  :components ((:file "package")
               (:file "wizard-professor")))

