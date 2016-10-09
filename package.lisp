;;;; package.lisp

(defpackage #:parse-me-c-source
  (:use #:cl #:iterate #:cl-itertools #:defmacro-enhance #:esrap-liquid)
  (:export #:preprocessor-parse)
  )

