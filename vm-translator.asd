;;;; vm-translator.asd

(asdf:defsystem #:vm-translator
  :description "Describe vm-translator here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:iterate #:trivia #:str #:cl-ppcre #:cl-interpol)
  :components ((:file "package")
               (:file "vm-translator")))
