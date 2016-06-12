;;;; timeparse.asd

(asdf:defsystem #:timeparse
  :description "Describe timeparse here"
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :license "MIT"
  :depends-on (#:local-time #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "timeparse")))
