;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(asdf:defsystem #:existcl
  :author "Nazarov Arthur <funfundzwanzigsten@gmail.com>"
  :license "Public Domain"
  :description "eXist-db driver for Common Lisp."
  :version "0.0.1"
  :depends-on (#:drakma
               #:xmls
               #:babel)
  :components ((:file "package")
               (:file "driver")
               (:file "interface")
               (:file "tests")))
