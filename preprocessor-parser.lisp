;;;; preprocessor-parser.lisp

(in-package #:parse-me-c-source)

;; OK, here I'll write the parser for the preprocessor
;; It has a number of features (which are actually a bit entangled)
;; Yet, I can try to hack on them separately first (and see how far I can go).

;; (defparameter start-of-line nil)

;; (defparameter the-token nil)

;; (defun track-start-of-line (token)
;;   (case (car token)
;;     (:newline (setf start-of-line t))
;;     (:whitespace nil)
;;     (otherwise (setf start-of-line nil))))

;; (defun parse-directive ()
;;   (when start-of-line
;;     (skip-whitespace)
;;     (when (match (:punctuator "#"))

(defun wrap-preprocessor-token-iter (iter)
  (if (equal 'iterator (type-of iter))
      iter
      (mk-iter (let ((depleted-p nil))
		 (lambda (x)
		   (declare (ignore x))
		   (if depleted-p
		       (values)
		       (handler-case (funcall iter)
			 (esrap-liquid::stop-iteration ()
			   (setf depleted-p t)
			   (values)))))))))
					   

(defiter token-iter (preprocessor-token-iter)
  (iter (generate it in-it (wrap-preprocessor-token-iter preprocessor-token-iter))
	(yield (next it))))

;;; #if #ifdef #ifndef and friends

;;; evaluation of if-conditionals

;;; #define and #undef

;;; #error

;;; #pragma -- the esoteric feature that I won't do (since GNU C doesn't do it as well).

;;; # \n (empty directive)

;;; #line

;;; #include

;;;; Apparently special variables work in a tricky way with coroutines.
;;;; But I can't grasp, what precisely the relation is -- I have to be careful here.
;;;; (defparameter the-special-variable nil)
;;;; (defiter simple-test-of-special ()
;;;;   (iter (while t)
;;;; 	(setf the-special-variable (not the-special-variable))
;;;; 	(yield the-special-variable)))
;;;; (defiter simple-test-of-special2 ()
;;;;   (let ((the-special-variable t))
;;;;     (iter (while t)
;;;; 	  (setf the-special-variable (not the-special-variable))
;;;; 	  (yield the-special-variable))))

(defclass iterator-stack (cl-itertools::iterator)
  ((stack :initform nil)
   (on-pop-cleanups :initform nil)))

(defgeneric push-stack (new-iter obj &optional on-pop-cleanup))
(defgeneric pop-stack (obj))

(defmethod push-stack ((new-iter cl-itertools::iterator) (obj iterator-stack)
		       &optional (on-pop-cleanup (lambda () nil)))
  (with-slots (stack on-pop-cleanups) obj
    (push new-iter stack)
    (push on-pop-cleanup on-pop-cleanups)))

(defmethod pop-stack ((obj iterator-stack))
  (with-slots (stack on-pop-cleanups) obj
    (pop stack)
    (funcall (pop on-pop-cleanups))
    nil))

(defun try-next (obj)
  (with-slots (stack) obj
    (inext-or-error (car stack))))

(defun iterator-stack-empty-p (obj)
  (not (slot-value obj 'stack)))

(defmethod cl-itertools::i-coro ((obj iterator-stack))
  (labels ((rec (x)
	     (declare (ignore x))
	     (if (iterator-stack-empty-p obj)
		 (values)
		 (handler-case (try-next obj)
		   (stop-iteration ()
		     ;; (format t "I'm here")
		     (pop-stack obj)
		     (rec nil))))))
    #'rec))

(defclass caching-iterator-stack (iterator-stack)
  ((cache :initform nil)))

(defmethod cl-itertools::i-coro :around ((obj caching-iterator-stack))
  (with-slots (cache) obj
    (let ((it (call-next-method)))
      (push it cache)
      it)))

(defun last-elt (obj)
  (with-slots (cache) obj
    (car cache)))

(defun drop-cache (obj)
  (with-slots (cache) obj
    (setf cache nil)))

(defun split-cache-as-list (obj)
  (with-slots (cache) obj
    (let ((it (nreverse cache)))
      (drop-cache obj)
      it)))

(defun split-butlast-cache-as-list (obj)
  (with-slots (cache) obj
    (let ((it (nreverse (cdr cache))))
      (drop-cache obj)
      it)))
  

;; OK, let's do a very naive preprocessor with macros that do not accept parameters at all
;; Tokens -- words

;; The following words have special meaning (for the preprocessor):
;; "define" -- the next word will be the definition of the macro
;; "stop" -- stop the definition of the macro
;; "escape" -- escape the following symbol (meaning it won't mean anything special to the preprocessor
;; "undefine" -- undefine the macro that is specified by the next word

;; This preprocessor has a number of "modes" of operation:
;; Basic mode:
;;   preprocessor queries iterator stack for tokens, finding macros and expanding them (meaning it pushes
;;   the corresponding iterator on stack and starts quering it.
;;   Also, preprocessor maintains the macro "call stack", so that macros are not doubly expanded
;;   Then word "stop" in this mode is not a special word -- it just gets propagated further, as any other token
;; Macro-name reading mode:
;;   this mode is entered when the token "define" or "undefine" is met in the basic mode.
;;   The following token should be the macro name (of either defined or undefined macro)
;;   and can't be any special word.
;; Macro-body reading mode:
;;   In this mode we read the body of a macro, until we encounter the (unescaped) token "stop".

;; The input iterator we will use for tests
(defiter simple-word-iter (str)
  (iter (for it in-it (cl-ppcre:split " " str))
	(yield it)))

(defiter naive-macro-preprocessor (token-iter)
  (let ((the-stack (make-instance 'caching-iterator-stack)))
    (push-stack token-iter the-stack)
    (iter (for it in-it the-stack)
	  (yield it))))

(defun mk-iterator-stack (iter)
  (let ((it (make-instance 'iterator-stack)))
    (push-stack iter it)
    it))
  

  
