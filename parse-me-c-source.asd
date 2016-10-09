;;;; parse-me-c-source.asd

(defpackage :parse-me-c-source-system
  (:use :cl :asdf))

(in-package :parse-me-c-source-system)

(asdf:defsystem #:parse-me-c-source
  :description "Parse C source files into AST; output bindings in JSON (like C2FFI)"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :serial t
  :version "0.1"
  :depends-on (#:iterate #:cl-itertools #:defmacro-enhance #:esrap-liquid)
  :components ((:file "package")
	       (:file "preprocessor-macros")
	       (:file "preprocessor-token-streams")
               (:file "parse-me-c-source")))


(defsystem :parse-me-c-source-tests
  :description "Tests for PARSE-ME-C-SOURCE."
  :licence "MIT"
  :depends-on (#:parse-me-c-source #:fiveam #:cl-interpol)
  :serial t
  :pathname "tests/"
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :parse-me-c-source))))
  (load-system :parse-me-c-source-tests)
  (funcall (intern "RUN-TESTS" :parse-me-c-source-tests)))
