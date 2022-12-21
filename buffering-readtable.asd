(asdf:defsystem #:buffering-readtable
  :description "Creates a readtable that buffers all input from a non-interactive stream passed to READ. The input can be transformed before being returned by READ."
  :author "Peter von Etter"
  :license  "LGPL-3.0"
  :version "0.0.1"
  :components ((:file "src/buffering-readtable"))
  :depends-on ())
