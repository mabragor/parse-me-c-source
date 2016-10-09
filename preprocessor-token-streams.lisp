
(in-package #:parse-me-c-source)

;; OK, the first step is to get file streams, that count # of lines and position in a line

;; Iterator protocol would be Pythonic one -- with StopIteration and so on
;; I require rather fine-grained control over iterator state -- simple co-routine wouldn't do (I guess)

;; (defgeneric next-iter (iterator))
(defgeneric peek-val (iterator))

(defclass raw-char-iterator ()
  ((line-num :initform 0)
   (pos-in-line :initform 0)
   (stream :initform (error "You should specify underlying char stream")
	   :initarg :stream)
   (fname :initform nil :initarg :fname)))

(defclass feeding-iterator ()
  ((sub-iter :initform nil :initarg :sub-iter)))

(defgeneric line-num (iter))
(defgeneric pos-in-line (iter))
(defgeneric file-name (iter))

;; By default, we dispatch these functions onto sub-iter
(defmethod line-num ((iter feeding-iterator))
  (line-num (slot-value iter 'sub-iter)))
(defmethod pos-in-line ((iter feeding-iterator))
  (pos-in-line (slot-value iter 'sub-iter)))
(defmethod file-name ((iter feeding-iterator))
  (file-name (slot-value iter 'sub-iter)))

(defmethod peek-val ((iter feeding-iterator))
  (with-slots (sub-iter) iter
    (peek-val sub-iter)))

(defmethod peek-val ((iter raw-char-iterator))
  (with-slots (stream) iter
    (peek-char nil stream nil)))

(defun mk-raw-char-iter (obj)
  (cond ((typep obj 'stream) (make-instance 'raw-char-iterator :stream obj))
	((typep obj 'string) (make-instance 'raw-char-iterator :stream (make-string-input-stream obj)))
	(t (error "Unexpected OBJ type: ~a" (type-of obj)))))

(defun read-char! (stream)
  (handler-case (read-char stream)
    (end-of-file () (error 'stop-iteration))))

(defmethod next-iter ((iter raw-char-iterator))
  (with-slots (line-num pos-in-line stream) iter
    (let ((char (read-char! stream)))
      (cond ((char= #\newline char) (progn (incf line-num)
					   (setf pos-in-line 0)
					   #\newline))
	    ;; #\return #\newline counts as one linebreak
	    ((char= #\return char) (let ((next-char (peek-char nil stream nil)))
				     (if (equal #\newline next-char)
					 (read-char! stream)))
	     (progn (incf line-num)
		    (setf pos-in-line 0)
		    #\newline))
	    (t (incf pos-in-line)
	       char)))))
	     
(defclass trigraph-resolved-iterator (feeding-iterator)
  ((stashed-char :initform nil)))

(defun mk-trigraph-resolved-iterator (sub-iter)
  (make-instance 'trigraph-resolved-iterator :sub-iter sub-iter))

(defmethod next-iter ((iter trigraph-resolved-iterator))
  ;; TODO : actually resolve trigraphs -- for now this layer does nothing
  (with-slots (sub-iter) iter
    (next-iter sub-iter)))

(defclass escaped-newlines-resolved-iterator (feeding-iterator)
  ((stashed-char :initform nil)))

(defun mk-escaped-newlines-resolved-iterator (sub-iter)
  (make-instance 'escaped-newlines-resolved-iterator :sub-iter sub-iter))

(defmethod next-iter ((iter escaped-newlines-resolved-iterator))
  (with-slots (sub-iter) iter
    (let ((char (next-iter sub-iter)))
      (if (char= #\\ char)
	  (let ((it (peek-val sub-iter)))
	    (if (and it (char= #\newline it))
		(progn (next-iter sub-iter) ; we clear this newline
		       (next-iter iter)) ; we recurse onto the next line
		char))
	  char))))

(defun mk-resolved-iterator (obj)
  (mk-escaped-newlines-resolved-iterator
   (mk-trigraph-resolved-iterator
    (mk-raw-char-iter obj))))

;; (let ((trigraph-map '((#\= . #\#) (#\( . #\[) (#\/ . #\\) (#\) . #\]) (#\' . #\^)
;; 		      (#\< . #\{) (#\! . #\|) (#\> . #\}) (#\- . #\~))))
;;   (defmethod next-iter ((iter trigraph-resolved-iterator))
;;     (with-slots (sub-iter stashed-char) iter
;;       (let ((char (or stashed-char
;; 		      (next-iter sub-iter))))
;; 	(if (not (char= #\? char))
;; 	    (progn (setf stashed-char nil)
;; 		   char)
;; 	    (progn (let ((next-char (setf stashed-char (handler-case (next-iter sub-iter)
;; 							 (stop-iteration () nil)))))
;; 		     (if (not (and next-char (char= #\? char)))
;; 			 #\? ; next char is correctly stashed for the next call
;; 			 (let ((next-next-char (handler-case (next-iter sub-iter)
;; 						 (stop-iteration () nil))))
;; 			   (let ((it (and next-next-char (cdr (assoc next-next-char trigraph-map
;; 								     :test #'char=)))))
;; 			     (if (not it)
				 

;;; Now we will use ESRAP-LIQUID to quickly hack tokenizer for a preprocessor
;;; The peculiarity is that I need to leave token-iter every time in consistent state
;;; (and also do something with the cache -- probably, forget it smartly)
;;; This iterator would feed on chars -- so, standard ESRAP-LIQUID should suffice.

(define-preprocessor-rule preprocessing-token ()
  (|| header-name
      identifier
      pp-number
      character-constant
      string-literal
      punctuator
      ;; this is really the fallback option -- should be the last one
      nw-char))

(defparameter *punctuator-coercion*
  '(("<:" . "[") (":>" . "]") ("<%" . "{") ("%>" . "}") ("%:" . "#") ("%:%:" . "##")))

(defun coerce-punctuator (thing)
  (or (cdr (assoc thing *punctuator-coercion* :test #'string=))
      thing))

(define-preprocessor-rule punctuator ()
  (list :punctuator
	(coerce-punctuator (text (most-full-parse "[" "]" "(" ")" "{" "}" "." "->"
						  "++" "--" "&" "*" "+" "-" "~" "!"
						  "/" "%" "<<" ">>" "<" ">" "<=" ">=" "==" "!=" "^" "|" "&&" "||"
						  "?" ":" ";" "..."
						  "=" "*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&=" "^=" "|="
						  "," "#" "##"
						  "<:" ":>" "<%" "%>" "%:" "%:%:")))))

(define-preprocessor-rule identifier ()
  (list :identifier (text (cons (v identifier-nondigit)
				(times (|| identifier-nondigit digit))))))

(define-preprocessor-rule implementation-defined-char ()
  (fail-parse "No other chars are defined by the implementation"))

(define-preprocessor-rule identifier-nondigit ()
  (|| nondigit
      universal-character-name
      implementation-defined-char))

(defun decode-char (things radix &key guard)
  (let ((it (parse-integer (apply #'text things) :radix radix)))
    (if guard
	(funcall guard it))
    (code-char it)))

(defun universal-char-name-guard (code)
  (cond ((< code #x00A0)
	 (when (not (find code '(#x0024 #x0040 #x0060) :test #'equal))
	   (fail-parse "Chars with codes below #x00A0 (except '$', '@' and '`') cannot be hex-encoded")))
	((and (<= #xD800 code) (<= code #xDFFF))
	 (fail-parse "Chars with codes below in the range #xD800 -- #xDFFF cannot be hex-encoded"))))

(define-preprocessor-rule universal-character-name ()
  (|| (progn (v "\\u") (decode-char (v hex-quad) 16 :guard #'universal-char-name-guard))
      (progn (v "\\U") (decode-char (list-v hex-quad hex-quad) 16 :guard #'universal-char-name-guard))))

(define-preprocessor-rule hex-quad ()
  (times hexadecimal-digit :exactly 4))

(define-preprocessor-rule nondigit ()
  (|| #\_
      (character-ranges (#\a #\z) (#\A #\Z))))

(define-preprocessor-rule digit ()
  (character-ranges (#\0 #\9)))

(define-preprocessor-rule nonzero-digit ()
  (character-ranges (#\1 #\9)))

(define-preprocessor-rule octal-digit ()
  (character-ranges (#\0 #\7)))

(define-preprocessor-rule hexadecimal-digit ()
  (character-ranges (#\0 #\9) (#\a #\f) (#\A #\F)))

(define-preprocessor-rule character-constant ()
  (let* ((prefix (? (|| #\L #\u #\U)))
	 (body (progm #\' c-char-sequence #\')))
    (list :char-const prefix body)))
    
(define-preprocessor-rule c-char-sequence ()
  (postimes c-char))

(define-preprocessor-rule c-char ()
  (|| escape-sequence
      (!! (|| #\' #\\ #\newline))))

(define-preprocessor-rule escape-sequence ()
  (|| simple-escape-sequence
      octal-escape-sequence
      hexadecimal-escape-sequence
      universal-character-name))

(define-preprocessor-rule simple-escape-sequence ()
  (v #\\)
  (|| #\' #\" #\? #\\
      (progn (v "a") #\bel)
      (progn (v "b") #\backspace)
      (progn (v "f") #\page)
      (progn (v "n") #\newline)
      (progn (v "r") #\return)
      (progn (v "t") #\tab)
      (progn (v "v") #\vt)))

(define-preprocessor-rule octal-escape-sequence ()
  (v #\\)
  (decode-char (times octal-digit :from 1 :upto 3) 8))

(define-preprocessor-rule hexadecimal-escape-sequence ()
  (v "\\x")
  (decode-char (postimes hexadecimal-digit) 16))


(define-preprocessor-rule string-literal ()
  (let* ((prefix (? (most-full-parse "u8" "u" "U" "L")))
	 (body (progm #\" (? s-char-sequence) #\")))
    (list :string-literal prefix (text body))))

(define-preprocessor-rule s-char-sequence ()
  (postimes s-char))

(define-preprocessor-rule s-char ()
  (|| escape-sequence
      (!! (|| #\" #\\ #\newline))))

(define-preprocessor-rule header-name ()
  (list :header-name (text (|| (progm #\< h-char-sequence #\>)
			       (progm #\" q-char-sequence #\")))))

;; TODO : only recognize header names in the context of #include and #pragma

(define-preprocessor-rule h-char-sequence ()
  (postimes h-char))

(define-preprocessor-rule h-char ()
  (!! (|| #\newline #\>)))

(define-preprocessor-rule q-char-sequence ()
  (postimes q-char))

(define-preprocessor-rule q-char ()
  (!! (|| #\newline #\")))

(define-preprocessor-rule sign ()
  (|| #\- #\+))

(define-preprocessor-rule pp-number ()
  (let* ((first (|| digit
		    (list (v #\.) (v digit))))
	 (rest (times (|| digit
			  #\.
			  (list (v #\e) (v sign))
			  (list (v #\E) (v sign))
			  (list (v #\p) (v sign))
			  (list (v #\P) (v sign))
			  identifier-nondigit))))
    (list :pp-number (text (cons first rest)))))
	   
(define-preprocessor-rule comment ()
  (|| one-line-comment
      multi-line-comment))

;; TODO : sometimes, I need to also count the number of lines in the comment
(define-preprocessor-rule one-line-comment ()
  (v "//")
  (times (!! #\newline)))

(define-preprocessor-rule multi-line-comment ()
  (v "/*")
  (times (!! "*/"))
  (v "*/"))

(defun expand-keyword-specs (lst)
  (mapcar (lambda (elt)
	    (if (atom elt)
		(cons (string-downcase elt) (intern (string elt) "KEYWORD"))
		(cons (car elt) (cadr elt))))
	  lst))

(defparameter *known-keywords*
  (expand-keyword-specs '(auto break case char const continue default do double else enum
			  extern float for goto if inline int long register restrict return
			  short signed sizeof static struct switch typedef union unsigned
			  void volatile while
			  ("_Alignas" :align-as) ("_Alignof" :align-of) ("_Atomic" :atomic)
			  ("_Bool" :bool) ("_Complex" :complex) ("_Generic" :generic)
			  ("_Imaginary" :imaginary) ("_Noreturn" :noreturn) ("_Static_assert" :static-assert)
			  ("_Thread_local" :thread-local))))

(defun upgrade-to-keyword (identifier)
  (when (not (eq :identifier (car identifier)))
    (error "Identifier token expected when upgrading to a keyword"))
  (let ((it (cdr (assoc (cadr identifier) *known-keywords* :test #'string=))))
    (if it
	(list :keyword it))))
	

;; TODO : implicitly have identifier '__func__' inside symbol table when translating a function

(define-preprocessor-rule constant ()
  (|| integer-constant
      floating-constant
      enumeration-constant
      character-constant))

(define-preprocessor-rule integer-constant ()
  (list :integer-constant (most-full-parse decimal-constant
					   octal-constant
					   hexadecimal-constant)
	(? integer-suffix)))

(define-preprocessor-rule decimal-constant ()
  (parse-integer (text (v nonzero-digit) (times digit)) :radix 10))

(define-preprocessor-rule octal-constant ()
  (parse-integer (text (v #\0) (times octal-digit)) :radix 8))

(define-preprocessor-rule hexadecimal-constant ()
  (v hexadecimal-prefix)
  (parse-integer (text (postimes hexadecimal-digit)) :radix 16))

(define-preprocessor-rule hexadecimal-prefix ()
  (|| "0x" "0X"))

(define-preprocessor-rule long-suffix ()
  (|| #\l #\L) :long)

(define-preprocessor-rule long-long-suffix ()
  (|| "ll" "LL") :long-long)

(define-preprocessor-rule unsigned-suffix ()
  (|| #\u #\U) :unsigned)

(defun swap-order (lst)
  "Swaps order of elements of a list, assuming it has presicely two elements"
  (list (cadr lst) (car lst)))

(define-preprocessor-rule integer-suffix ()
  (most-full-parse (list-v unsigned-suffix (? long-suffix))
		   (list-v unsigned-suffix (? long-long-suffix))
		   (swap-order (list-v long-suffix (? unsigned-suffix)))
		   (swap-order (list-v long-long-suffix (? unsigned-suffix)))))


(define-preprocessor-rule floating-constant ()
  (list :floating-constant (most-full-parse decimal-floating-constant
					    hexadecimal-floating-constant)
	(or (? floating-suffix)
	    :double)))

(define-preprocessor-rule floating-suffix ()
  (|| (progn (|| #\l #\L) :long-double)
      (progn (|| #\f #\F) :float)))

(define-preprocessor-rule hexadecimal-digit-sequence ()
  (text (postimes hexadecimal-digit)))

(define-preprocessor-rule digit-sequence ()
  (text (postimes digit)))

(define-preprocessor-rule exponent-part ()
  (|| #\e #\E)
  (list-v (? sign) digit-sequence))

(define-preprocessor-rule binary-exponent-part ()
  (|| #\p #\P)
  (list-v (? sign) digit-sequence))

(define-preprocessor-rule hexadecimal-fractional-constant ()
  (|| (list (? hexadecimal-digit-sequence) (progn-v #\. hexadecimal-digit-sequence))
      (list-v hexadecimal-digit-sequence) (progn-v #\. nil)))

(define-preprocessor-rule fractional-constant ()
  (|| (list (? digit-sequence) (progn-v #\. digit-sequence))
      (list-v digit-sequence) (progn-v #\. nil)))

(define-preprocessor-rule decimal-floating-constant ()
  (|| (list-v fractional-constant (? exponent-part))
      (list-v digit-sequence exponent-part)))

(define-preprocessor-rule hexadecimal-floating-constant ()
  (v hexadecimal-prefix)
  (|| (list-v hexadecimal-fractional-constant binary-exponent-part)
      (list-v hexadecimal-digit-sequence binary-exponent-part)))

(define-preprocessor-rule enumeration-constant ()
  (list :enum (cadr (v identifier))))

;;; Expressions
;;; From here on, it may be required to actually go from ESRAP-LIQUID to
;;; another parser -- but we'll see later
;;; Yes, apparently, this part happens already after the upgrade
;;; OK, first I'll write it as a part of ESRAP-LIQUID parser, and then upgrade.

(define-preprocessor-rule primary-expression ()
  (|| ;; TODO : error on undefined identifiers
   identifier
   constant
   string-literal
   (progm '(:punctuator "(") expression '(:punctuator ")"))
   generic-selection))
      
;; (define-preprocessor-rule 
