
(in-package #:parse-me-c-source)

;; OK, the first step is to get file streams, that count # of lines and position in a line

;; Iterator protocol would be Pythonic one -- with StopIteration and so on
;; I require rather fine-grained control over iterator state -- simple co-routine wouldn't do (I guess)

(defgeneric next-val (iterator))

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
				 

