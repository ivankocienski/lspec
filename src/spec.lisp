(in-package :lspec)

(defstruct spec
  id
  name
  code
  group
  is-empty)

(define-condition spec-failed (error)
  ((message :initarg :message
	    :initform nil
	    :accessor spec-failed-message)))

(define-condition spec-pending (error)
  ((message :initarg :message
	    :initform nil
	    :accessor spec-pending-message)))

(defun run-expectation (exid var-name var args)
  (let ((expectation (find-expectation exid)))
    (if expectation
	(if (not (funcall (expectation-test-code expectation) var args))
	    (error 'spec-failed
		   :message (format nil "(~a=~s) ~a"
				    var-name
				    var
				    (expectation-message expectation)))))))

(defmacro expect (var expectation &rest args)
  `(run-expectation ,expectation ',var ,var ,args))

(defun build-it (name group code empty)
  (setf (spec-group-entries group)
	(al-insert (spec-group-entries group) name
		   (make-spec :name name
			      :code code
			      :is-empty empty
			      :group group
			      :id (1+ (length (spec-group-entries group)))))))





(defmacro internal-it (caption group &body body)
  `(macrolet ((pending (&optional (message "This spec is pending"))
		`(error 'spec-pending
			:message ,message)))
     
     (build-it ,caption ,group (lambda () ,@body) ,(null body))))

(defun full-spec-description (spec)

  (labels ((gather-group-names (group)
	     (cons (spec-group-caption group)
		   (let ((parent (spec-group-parent group)))
		     (if parent (gather-group-names parent))))))
    
    (format nil "~{~A: ~}" (reverse
			    (cons (spec-name spec)
				  (gather-group-names (spec-group spec)))))))

(defun full-spec-id (spec)
  (labels ((gather-group-ids (group)
	     (cons (spec-group-id group)
		   (let ((parent (spec-group-parent group)))
		     (if parent (gather-group-names parent))))))
    
    (format nil "~{~A.~}" (reverse
			    (cons (spec-id spec)
				  (gather-group-names (spec-group spec)))))))
  
