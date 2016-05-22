
(in-package #:parse-me-c-source)

;; OK, the first step is to get file streams, that count # of lines and position in a line

;; Iterator protocol would be Pythonic one -- with StopIteration and so on
;; I require rather fine-grained control over iterator state -- simple co-routine wouldn't do (I guess)

(defgeneric next-val (iterator))
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

(defun mk-raw-char-iter (fname-or-stream)
  (if (typep fname-or-stream 'stream)
      (make-instance 'raw-char-iterator :stream fname-or-stream)
      (with-open-file (stream fname-or-stream)
	(make-instance 'raw-char-iterator :stream stream :fname fname-or-stream))))

(defun read-char! (stream)
  (handler-case (read-char stream)
    (end-of-file () (error 'stop-iteration))))

(defmethod next-val ((iter raw-char-iterator))
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

(defmethod next-val ((iter trigraph-resolved-iterator))
  ;; TODO : actually resolve trigraphs -- for now this layer does nothing
  (with-slots (sub-iter) iter
    (next-val sub-iter)))

(defclass escaped-newlines-resolved-iterator (feeding-iterator)
  ((stashed-char :initform nil)))

(defun mk-escaped-newlines-resolved-iterator (sub-iter)
  (make-instance 'escaped-newlines-resolved-iterator :sub-iter sub-iter))

(defmethod next-val ((iter escaped-newlines-resolved-iterator))
  (with-slots (sub-iter) iter
    (let ((char (next-val sub-iter)))
      (if (char= #\\ char)
	  (let ((it (peek-val sub-iter)))
	    (if (and it (char= #\newline it))
		(progn (next-val sub-iter) ; we clear this newline
		       (next-val iter)) ; we recurse onto the next line
		char))
	  char))))


;; (let ((trigraph-map '((#\= . #\#) (#\( . #\[) (#\/ . #\\) (#\) . #\]) (#\' . #\^)
;; 		      (#\< . #\{) (#\! . #\|) (#\> . #\}) (#\- . #\~))))
;;   (defmethod next-val ((iter trigraph-resolved-iterator))
;;     (with-slots (sub-iter stashed-char) iter
;;       (let ((char (or stashed-char
;; 		      (next-val sub-iter))))
;; 	(if (not (char= #\? char))
;; 	    (progn (setf stashed-char nil)
;; 		   char)
;; 	    (progn (let ((next-char (setf stashed-char (handler-case (next-val sub-iter)
;; 							 (stop-iteration () nil)))))
;; 		     (if (not (and next-char (char= #\? char)))
;; 			 #\? ; next char is correctly stashed for the next call
;; 			 (let ((next-next-char (handler-case (next-val sub-iter)
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

(define-preprocessor-rule punctuator ()
  (list :punctuator (text (most-full-parse "[" "]" "(" ")" "{" "}" "." "->"
					   "++" "--" "&" "*" "+" "-" "~" "!"
					   "/" "%" "<<" ">>" "<" ">" "<=" ">=" "==" "!=" "^" "|" "&&" "||"
					   "?" ":" ";" "..."
					   "=" "*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&=" "^=" "|="
					   "," "#" "##"
					   "<:" ":>" "<%" "%>" "%:" "%:%:"))))

(define-preprocessor-rule identifier ()
  (list :identifier (text (cons (v identifier-nondigit)
				(times (|| identifier-nondigit digit))))))

(define-preprocessor-rule implementation-defined-char ()
  (fail-parse "No other chars are defined by the implementation"))

(define-preprocessor-rule identifier-nondigit ()
  (|| nondigit
      universal-character-name
      implementation-defined-char))

(define-preprocessor-rule universal-character-name ()
  (|| (progn (v "\\u") (dehexify-char (v hex-quad)))
      (progn (v "\\U") (dehexify-char (v hex-quad) (v hex-quad)))))

(define-preprocessor-rule hex-quad ()
  (times hexadecimal-digit :exactly 4))

(define-preprocessor-rule nondigit ()
  (|| #\_
      (character-ranges (#\a #\z) (#\A #\Z))))

(define-preprocessor-rule digit ()
  (character-ranges (#\0 #\9)))

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
  (apply #'deoctify-char (times octal-digit :from 1 :upto 3)))

(define-preprocessor-rule hexadecimal-escape-sequence ()
  (v "\\x")
  (apply #'dehexify-char (postimes hexadecimal-digit)))


(define-preprocessor-rule string-literal ()
  (let* ((prefix (? (most-full-parse "u8" "u" "U" "L")))
	 (body (progm #\" (? s-char-sequence) #\")))
    (list :string-literal prefix body)))

(define-preprocessor-rule s-char-sequence ()
  (postimes s-char))

(define-preprocessor-rule s-char ()
  (|| escape-sequence
      (!! (|| #\" #\\ #\newline))))

(define-preprocessor-rule header-name ()
  (|| (progm #\< h-char-sequence #\>)
      (progm #\" q-char-sequence #\")))

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
			  identifier-nondigit
			  #\.
			  (list (v #\e) (v sign))
			  (list (v #\E) (v sign))
			  (list (v #\p) (v sign))
			  (list (v #\P) (v sign))))))
    (list :pp-number (cons first rest))))
	   
