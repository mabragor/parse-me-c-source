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
  (lambda (x)
    (declare (ignore x))
    (with-slots (cache) obj
      (let ((vals (multiple-value-list (funcall (call-next-method) nil))))
	(if (not vals)
	    (values)
	    (progn (push (car vals) cache)
		   (car vals)))))))

(defun last-elt (obj)
  (with-slots (cache) obj
    (car cache)))

(defun pop-cache (obj)
  (with-slots (cache) obj
    (pop cache)))

(defun push-cache (thing obj)
  (with-slots (cache) obj
    (push thing cache)))

(defmacro with-new-cache (obj-var &body body)
  (let ((g!-old-cache (gensym "G!-OLD-CACHE"))
	(g!-cache (gensym "G!-CACHE")))
    `(with-slots ((,g!-old-cache cache)) ,obj-var
       (let ((,g!-cache ,g!-old-cache))
	 (unwind-protect (progn (setf ,g!-old-cache nil)
				,@body)
	   (setf ,g!-old-cache ,g!-cache))))))

(defun drop-cache (obj)
  (with-slots (cache) obj
    (setf cache nil)))

(defun drop-butlast-cache (obj)
  (with-slots (cache) obj
    (if cache
	(setf (cdr cache) nil))))

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
  
(defun search-for-token (obj token)
  (iter (for it in-it obj)
	(if (equal token it)
	    (terminate))))

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

(defun %mk-iterator-stack (iter class)
  (let ((it (make-instance class)))
    (push-stack iter it)
    it))

(defparameter *macros* nil)
(defparameter *iterator-stack* nil)
(defparameter *active-macro-calls* nil)

(defun mk-iterator-stack (iter)
  (%mk-iterator-stack iter 'iterator-stack))
(defun mk-cached-iterator-stack (iter)
  (%mk-iterator-stack iter 'caching-iterator-stack))

(defun base-mode-escape-handler ()
  ;; The goal here is to make this handler usable not only in base mode, but also in
  ;; the mode of parsing the macro body
  (handler-case (progn (pop-cache *iterator-stack*)
		       (inext-or-error *iterator-stack*)
		       :yield)
    (stop-iteration () :stop-iteration)))

(defun get-macro-name (token)
  (pop-cache *iterator-stack*)
  (handler-case (let ((it (inext-or-error *iterator-stack*)))
		  (drop-cache *iterator-stack*)
		  it)
    (stop-iteration () (error "'~a' token found as the last token of the stream" token))))

(defun base-mode-undefine-handler ()
  (let ((macro-name (get-macro-name "undefined")))
    (setf (gethash macro-name *macros*) nil)
    nil))

(defun macro-body-mode-stop-handler ()
  (let ((ans (split-butlast-cache-as-list *iterator-stack*)))
    (with-slots (cache) *iterator-stack*
      (push ans cache)
      :return)))

(defparameter *macro-body-mode-special-operators*
  `(("escape" . ,#'base-mode-escape-handler) ; yes, we want to reuse this handler also here
    ("stop" . ,#'macro-body-mode-stop-handler)))

(defun get-macro-body ()
  (with-new-cache *iterator-stack*
    (iter (for token in-it *iterator-stack*)
	  (let ((handler (cdr (assoc token *macro-body-mode-special-operators* :test #'equal))))
	    (if handler
		(case (funcall handler)
		  (:stop-iteration (terminate))
		  ;; Yes, we return the macro body via the stack
		  (:return (return (last-elt *iterator-stack*)))
		  (otherwise nil))))
	  (finally (error "Token stream ended while scanning macro body (missing 'stop' token?)")))))
		 
(defun base-mode-define-handler ()
  (setf (gethash (get-macro-name "defined") *macros*)
	(get-macro-body))
  nil)

(defparameter *base-mode-special-operators*
  ;; The idea is to write handling of special operators in such a way, that they are not hardcoded
  ;; (in contrast with how it's done in the standard Lisp reader
  `(("escape" . ,#'base-mode-escape-handler)
    ("undefine" . ,#'base-mode-undefine-handler)
    ("define" . ,#'base-mode-define-handler)
    ))


(defun install-macroexpansion (macro-name)
  (let ((iter (mk-iter (gethash macro-name *macros*))))
    (setf (gethash macro-name *active-macro-calls*) t)
    (push-stack iter *iterator-stack*
		;; Closure is needed here, because the cleanup will be called outside
		;; the LET, that sets the special variables -- in the iterate driver clause
		(let ((active-macro-calls *active-macro-calls*))
		  (lambda ()
		    ;; (format t "In macro cleanup: ~a ~a" macro-name active-macro-calls)
		    (setf (gethash macro-name active-macro-calls) nil))))))

(defun active-macro-p (macro-name)
  (and (gethash macro-name *macros*)
       (not (gethash macro-name *active-macro-calls*))))

(defiter naive-macro-preprocessor (token-iter)
  ;; We cannot use special variables due to current limitations of CL-COROUTINE
  ;; That's why we pass ITERATOR-STACK and MACROS around
  (let ((iterator-stack (mk-cached-iterator-stack token-iter))
	(macros (make-hash-table :test #'equal))
	(active-macro-calls (make-hash-table :test #'equal)))
    (iter (for it in-it iterator-stack)
	  (let ((*iterator-stack* iterator-stack)
		(*macros* macros)
		(*active-macro-calls* active-macro-calls))
	    ;; (format t "*iterator-stack*: ~a *macros*: ~a~%" *iterator-stack* *macros*)
	    (let ((handler (cdr (assoc it *base-mode-special-operators* :test #'equal))))
	      (if handler
		  (case (funcall handler)
		    (:stop-iteration (terminate))
		    (:yield (drop-butlast-cache *iterator-stack*)
			    (yield (last-elt *iterator-stack*)))
		    (otherwise nil))
		  (if (active-macro-p it)
		      (install-macroexpansion it)
		      (progn (drop-butlast-cache *iterator-stack*)
			     (yield it)))))))))

