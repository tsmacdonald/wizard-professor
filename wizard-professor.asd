;;;; wizard-professor.asd

(asdf:defsystem #:wizard-professor
  :serial t
  :description "A spell-checker"
  :author "Tim Macdonald <tsmacdonald@gmail.com>"
  :license "MIT License"
  :depends-on (#:cl-ppcre #:fare-memoization)
  :components ((:file "package")
               (:file "wizard-professor")
	       (:file "alexandria"))

