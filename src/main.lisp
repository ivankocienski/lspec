(in-package :lspec)

;; what was i doing again?

(defstruct spec-group
  caption
  entries)

(defstruct spec
  name
  code)

(define-condition spec-failed (error)
  ((message :initarg :message
	    :initform nil
	    :accessor spec-failed-message)))

(defparameter *defined-spec-groups* (make-hash-table :test 'equal))


(defun run-expectation (exid var-name var args)
  (let ((expectation (find-expectation exid)))
    (if expectation
	(if (not (funcall (expectation-test-code expectation) var args))
	    (error 'spec-failed
		   :message (format nil "(~a=~s) ~a"
				    var-name
				    var
				    (expectation-message expectation)))))))

;;(defun expect-internal (varname var-value expectation &rest args)
;;  (if (not thing)
;;      (error 'spec-failed)))
  
(defmacro expect (var expectation &rest args)
  `(run-expectation ,expectation ',var ,var ,args))

(defun clear-specs ()
  (let ((count (length *defined-specs*)))
    (setf *defined-specs* nil
	  *defined-spec-groups* (make-hash-table :test 'equal))
    count))

(defun build-it (name group code)
  (push (make-spec :name name
		   :code code)
	(spec-group-entries group)))

(defun list-specs ()
  (loop for caption being the hash-keys of *defined-spec-groups*
     do (format t "~a~%" caption)))

(defun alloc-new-group (caption parent)
  (let ((new-group (make-spec-group :caption caption)))
    ;;(push new-group *defined-spec-groups*)
    (setf (gethash caption *defined-spec-groups*) new-group)
    new-group))

(defmacro specify (caption &body body)

  (let ((group-var (gensym "spec-stack")))
    
    `(let* ((,group-var nil)
	    (new-group (alloc-new-group ,caption)))
       
       (macrolet ((it (caption &body body)
		    `(build-it ,caption new-group (lambda () ,@body)))
		  (context (caption &body body)
		    `(fuuuuu))))

		    ,@body)))
  

(defun run-group (grp)
  (let ((failures 0) (count (length (spec-group-entries grp))))
    (format t "group: '~a'~%" (spec-group-caption grp))

    (dolist (s (spec-group-entries grp))

      (format t "  spec: '~a'~%" (spec-name s))
      
      (handler-case
	  (progn
	    (funcall (spec-code s))
	    (format t "      : passed~%"))
	    
	(spec-failed (failure)
	  (incf failures)
	  (format t "      : failed '~a'~%"
		  (spec-failed-message failure)))))

    (values count failures)))

(defun run-all ()
  (let ((count 0) (failures 0))

    (loop for caption being the hash-keys of *defined-spec-groups*
       using (hash-value specs)
       do (multiple-value-bind (c f) (run-group specs)
	    (incf count    c)
	    (incf failures f)))
	
    (format t "done. ~d specs, ~d failed~%" count failures)))
