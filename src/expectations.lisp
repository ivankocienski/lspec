(in-package :lspec)

(defstruct expectation
  message
  test-code)

(defparameter *expectation-table* (make-hash-table))

(defun build-expectation (id msg code)
  (setf (gethash id *expectation-table*)
	(make-expectation :message   msg
			  :test-code code)))

(defmacro defexpectation ((id msg var args) &body body)
  `(build-expectation ,id ,msg
		      (lambda (,var &rest ,args)
			(declare (ignorable ,args))
			,@body)))

(defun list-expectations ()
  (loop for exp-id being the hash-keys of *expectation-table*
     using (hash-value exp)
     do (format t "~a~%" exp-id)
     do (format t "  ~a~%" (expectation-message exp)))

  (let ((count (hash-table-count *expectation-table*)))
    (format t "found ~d expectations ~a~%"
	    count
	    (if (zerop count) "(You must be easy to please)" ""))
    count))


(defun find-expectation (id)
  (gethash id *expectation-table*))

(defexpectation (:to-be-zero "should be zero" val args)
  (zerop val))

(defexpectation (:to-be-true "should be true" val args)
  (and (eq (type-of val) 'boolean) val))

(defexpectation (:to-be-nil "should be nil" val args)
  (null val))
