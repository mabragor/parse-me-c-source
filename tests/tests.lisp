
(cl-interpol:enable-interpol-syntax)

(defpackage :parse-me-c-source-tests
  (:use #:alexandria #:cl #:parse-me-c-source #:fiveam #:iterate)
  (:export #:run-tests))

(in-package :parse-me-c-source-tests)

(defun run-tests ()
  (let ((results (run 'parse-me-c-source)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(def-suite parse-me-c-source)
(in-suite parse-me-c-source)

(test basic
  (is (equal '(:identifier "asdf") (preprocessor-parse 'identifier "asdf"))))

