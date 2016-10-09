;;;; package.lisp

(defpackage #:parse-me-c-source
  (:use #:cl #:iterate #:cl-itertools #:defmacro-enhance #:esrap-liquid)
  (:shadowing-import-from #:esrap-liquid #:next-iter #:stop-iteration)
  (:export #:preprocessor-parse #:preprocessor-parse-token-iter #:mk-resolved-iterator)
  )

