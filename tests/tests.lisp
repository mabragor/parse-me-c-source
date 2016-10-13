
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

(test punctuator
  (is (equal '(:punctuator "[") (preprocessor-parse 'punctuator "[")))
  (is (equal '(:punctuator "[") (preprocessor-parse 'punctuator "<:")))
  )

(test header-name
  (is (equal '(:header-name "1/a.h") (preprocessor-parse 'header-name "<1/a.h>"))))

(test pp-number
  (is (equal '(:pp-number "123") (preprocessor-parse 'pp-number "123")))
  (is (equal '(:pp-number "12.3") (preprocessor-parse 'pp-number "12.3")))
  (is (equal '(:pp-number "12.3E-02") (preprocessor-parse 'pp-number "12.3E-02")))
  )

(test resolving-escaped-newlines
  (is (equal '(:identifier "asdf") (preprocessor-parse-token-iter 'identifier (mk-resolved-iterator "as\\
df")))))

(test preprocessor-lexer
  (let ((lex (parse-me-c-source::mk-preprocessor-lexer "#define foo")))
    (is (equal '((:PUNCTUATOR "#")
		 (:IDENTIFIER "define")
		 (:WHITESPACE)
		 (:IDENTIFIER "foo"))
	       (iter (for it next (handler-case (funcall lex)
				    (esrap-liquid::stop-iteration ()
				      (terminate))))
		     (collect it)))))
  (is (equal '((:PUNCTUATOR "#")
		 (:IDENTIFIER "define")
		 (:WHITESPACE)
	       (:IDENTIFIER "foo"))
	     (parse-me-c-source::collect-iter (parse-me-c-source::wrap-preprocessor-token-iter
			    (parse-me-c-source::mk-preprocessor-lexer "#define foo"))))))

(test mk-iterator-stack
  (is (equal '("a" "b" "c") (parse-me-c-source::collect-iter
			     (parse-me-c-source::mk-iterator-stack
			      (parse-me-c-source::simple-word-iter "a b c")))))
  )

(test naive-preprocessor
  (macrolet ((frob (x y)
	       `(is (equal ,x (parse-me-c-source::collect-iter
			       (parse-me-c-source::naive-macro-preprocessor
				(parse-me-c-source::simple-word-iter ,y)))))))
    (frob '("a" "b" "c") "a escape b c escape")
    (frob '("a" "b" "b" "b" "a" "a" "a") "a define a b stop a a a undefine a a a a")
    (frob '("a" "b" "stop" "b" "stop" "b" "stop") "a define a b escape stop stop a a a")
    ;; test active macro calls
    (frob '("a" "b" "a") "a define a b a stop a")
    (frob '("c" "d" "a" "d" "c" "b") "define a c b stop define b d a stop a b")
    ))
  
