;;;; timeparse.asd

(asdf:defsystem #:timeparse
  :description "Parses a timestring using the spec from local-time."
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :license "MIT"
  :depends-on (#:local-time #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "timeparse")))
