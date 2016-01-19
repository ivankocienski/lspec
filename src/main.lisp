(in-package :lspec)

;; what was i doing again?

(defstruct spec-group
  caption
  ;; this should count all the sub entries?
  entries)

(defstruct spec
  name
  code)

(define-condition spec-failed (error)
  ((message :initarg :message
	    :initform nil
	    :accessor spec-failed-message)))

(defparameter *spec-group-root* nil)


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

(defun clear-specs ()
  (let ((count (length *spec-group-root*)))
    (setf *spec-group-root* nil)
    count))

(defun build-it (name group code)
  (setf (spec-group-entries group)
	(al-insert (spec-group-entries group) name
		   (make-spec :name name
			      :code code))))

;;  (push (make-spec :name name
;;		   :code code)
	;; NOTE: this is a list and it should be
	;; an a-list with (name . spec) pairs!
;;	(spec-group-entries group)))

(defun list-specs ()
  (al-each (*spec-group-root* name group)
    (declare (ignore group))
    (format t "~a~%" name)))

(defun alloc-new-group (caption parent)
  (format t "alloc-new-group~%")
  (let ((new-group (make-spec-group :caption caption)))

    (if parent
	(setf (spec-group-entries parent)
	      (al-insert (spec-group-entries parent) caption new-group))
	
	(setf *spec-group-root*
	      (al-insert *spec-group-root* caption new-group)))
	
    new-group))

(defmacro internal-group (caption parent  &body body)
    
    `(let ((new-group (alloc-new-group ,caption ,parent)))
       
       (macrolet ((it (caption &body body)
		    `(build-it ,caption new-group (lambda () ,@body)))
		  (context (caption &body body)
		    `(internal-group ,caption new-group ,@body)))

		    ,@body)))

(defmacro specify (caption &body body)
  `(internal-group ,caption nil ,@body))

  



(defun count-specs (entry-list)
  (let ((count 0))
    (format t "count-specs: (type-of entry-list) ~a~%" (type-of entry-list))
    (al-each (entry-list name ent)
      (declare (ignore name))
      (format t "  t=~a~%" (type-of ent))
      (typecase ent
	(spec-group (incf count (count-specs (spec-group-entries ent))))
	(spec (incf count))))
    count))

(defun run-group (spec-list)
  (let ((failures 0) (count 0))
    
    ;;(format t "group: '~a'~%" (spec-group-caption grp))

    (al-each (spec-list caption entry)

      (declare (ignore caption))
      
      ;;(format t "  spec: '~a'~%" (spec-name s))

      (typecase entry
	(spec-group (multiple-value-bind (c f) (run-group (spec-group-entries entry))
		      (format t "c=~a  f=~a~%" c f)
		      (incf count c)
		      (incf failures f)))
	
	(spec
	 
	 (handler-case
	     (progn
	       (incf count)
	       (funcall (spec-code entry))
	       (format t "      : passed~%"))
	   
	   (spec-failed (failure)
	     (incf failures)
	     (format t "      : failed '~a'~%"
		     (spec-failed-message failure)))))))

    (values count failures)))
  
(defun run-all ()
  (let ((count 0) (failures 0))

;;    (loop for caption being the hash-keys of *defined-spec-groups*
;;       using (hash-value specs)
    ;;       do (multiple-value-bind (c f) (run-group specs)

    (al-each (*spec-group-root* caption spec-group)
      (declare (ignore caption))
      (multiple-value-bind (c f) (run-group (spec-group-entries spec-group))
	    (incf count    c)
	    (incf failures f)))
	
    (format t "done. ~d specs, ~d failed~%" count failures)))
