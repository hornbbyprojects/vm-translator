;;;; package.lisp

(defpackage #:vm-translator
  (:use #:cl)
  (:export #:parse-line #:gen-asm #:make-context)
  (:local-nicknames (i iterate)))
