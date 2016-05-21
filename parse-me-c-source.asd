;;;; parse-me-c-source.asd

(asdf:defsystem #:parse-me-c-source
  :description "Parse C source files into AST; output bindings in JSON (like C2FFI)"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:iterate #:cl-itertools #:defmacro-enhance)
  :components ((:file "package")
	       (:file "preprocessor-token-streams")
               (:file "parse-me-c-source")))

