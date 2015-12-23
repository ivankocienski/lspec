(in-package :lspec)

(defstruct expectation
  name
  description
  test-code)

(defparameter *expectation-table* (make-hash-table))

(defun build-expectation (id name desc code)
  (setf (gethash id *expectation-table*)
	(make-expectation :name name
			  :description desc
			  :test-code code)))

(defmacro defexpectation (id name desc &body body)
  `(build-expectation ,id ,name ,desc
		      (lambda (val &rest args)
			(declare (ignorable args))
			,@body)))

(defun list-expectations ()
  (loop for exp-id being the hash-keys of *expectation-table*
     using (hash-value exp)
     do (format t "~a~%" exp-id)
     do (format t "  ~a~%" (expectation-description exp)))

  (let ((count (hash-table-count *expectation-table*)))
    (format t "found ~d expectations ~a~%"
	    count
	    (if (zerop count) "(You must be easy to please)" ""))
    count))


(defun find-expectation (id)
  (gethash id *expectation-table*))

(defexpectation :to-be-zero "should be zero" "is var zero?"
  (zerop val))

(defexpectation :to-be-true "should be true" "is var true?"
  (and (eq (type-of val) 'boolean) val))
