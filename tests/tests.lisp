
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

(test universal-character-name
  (is (equal #\$ (preprocessor-parse 'universal-character-name "\\u0024"))))

(test integer-constant
  (is (equal '(:integer-constant 100 nil) (preprocessor-parse 'integer-constant "100")))
  (is (equal '(:integer-constant 9 nil) (preprocessor-parse 'integer-constant "011")))
  (is (equal '(:integer-constant 65535 nil) (preprocessor-parse 'integer-constant "0xFFFF")))
  (is (equal '(:integer-constant 100 (nil :long)) (preprocessor-parse 'integer-constant "100l")))
  (is (equal '(:integer-constant 9 (nil :long-long)) (preprocessor-parse 'integer-constant "011ll")))
  (is (equal '(:integer-constant 65535 (:unsigned :long)) (preprocessor-parse 'integer-constant "0xFFFFul")))
  )

(test floating-constant
  (is (equal '(:floating-constant (("1" "23") nil) :double) (preprocessor-parse 'floating-constant "1.23")))
  (is (equal '(:floating-constant (("1" "230") nil) :double) (preprocessor-parse 'floating-constant "1.230")))
  (is (equal '(:floating-constant (("123") (#\- "2")) :double) (preprocessor-parse 'floating-constant "123e-2")))
  (is (equal '(:floating-constant (("123") (#\- "02")) :double) (preprocessor-parse 'floating-constant "123e-02")))
  (is (equal '(:floating-constant (("1" "23") nil) :long-double) (preprocessor-parse 'floating-constant "1.23L")))
  )
	     
(test enumeration-constant
  (is (equal '(:enum "asdf") (preprocessor-parse 'enumeration-constant "asdf")))
  )

(test character-constant
  (is (equal '(:char-const nil (#\Nul)) (preprocessor-parse 'character-constant "'\\0'")))
  )

(test string-literal
  (is (equal '(:string-literal nil "3") (preprocessor-parse 'string-literal "\"3\"")))
  (is (equal `(:string-literal nil ,(format nil "~a" (code-char #x12)))
	     (preprocessor-parse 'string-literal "\"\\x12\"")))
  )
