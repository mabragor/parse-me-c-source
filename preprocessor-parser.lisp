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
  ())

(defmethod cl-itertools::i-coro ((obj iterator-stack))
  (lambda (x) 3))
