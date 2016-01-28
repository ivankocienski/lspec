(in-package :lspec)

;; what was i doing again?

(defstruct spec-group
  caption
  around-callbacks
  ;; this should count all the sub entries?
  entries
  parent)

(defstruct spec
  name
  code
  is-empty)

(define-condition spec-failed (error)
  ((message :initarg :message
	    :initform nil
	    :accessor spec-failed-message)))

(define-condition spec-pending (error)
  ((message :initarg :message
	    :initform nil
	    :accessor spec-pending-message)))

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

(defun list-specs ()
  (al-each (*spec-group-root* name group)
    (declare (ignore group))
    (format t "~a~%" name)))

(defun build-it (name group code empty)
  (setf (spec-group-entries group)
	(al-insert (spec-group-entries group) name
		   (make-spec :name name
			      :code code
			      :is-empty empty))))

(defun alloc-new-group (caption parent)
  (format t "alloc-new-group~%")
  (let ((new-group (make-spec-group :caption caption :parent parent)))

    (if parent
	(setf (spec-group-entries parent)
	      (al-insert (spec-group-entries parent) caption new-group))
	
	(setf *spec-group-root*
	      (al-insert *spec-group-root* caption new-group)))
	
    new-group))

(defmacro build-around-each (group &body body)
  `(push (lambda (next-step)
	   (macrolet ((yield () `(funcall next-step)))
	     ,@body))
	 (spec-group-around-callbacks ,group))
  )

(defmacro internal-group (caption parent  &body body)
    
    `(let ((new-group (alloc-new-group ,caption ,parent)))
       
       (macrolet ((it (caption &body body)
		    `(build-it ,caption new-group (lambda () ,@body) ,(null body)))
		  
		  (context (caption &body body)
		    `(internal-group ,caption new-group ,@body))
		  
		  (around-each (&body body)
		    `(build-around-each new-group ,@body)))

		    ,@body)))

(defmacro specify (caption &body body)
  `(internal-group ,caption nil ,@body))

  



(defun count-specs (entry-list)
  (let ((count 0))
    (format t "count-specs: (type-of entry-list) ~a~%" (type-of entry-list))
    (al-each (entry-list name ent)
      (declare (ignore name))
      (typecase ent
	(spec-group (incf count (count-specs (spec-group-entries ent))))
	(spec (incf count))))
    count))

(defun invoke-spec (spec start-group)
    
  (labels ((recursive-spec-step (callbacks)
	     (let ((callback (car callbacks)))
	       (if callback
		   (funcall callback (lambda () (recursive-spec-step (cdr callbacks))))
		   (funcall (spec-code spec)))))
	   
	   (gather-callbacks (group)
	     (if (spec-group-parent group)
		 (append (spec-group-around-callbacks group)
			 (gather-callbacks (spec-group-parent group)))
		 
		 (spec-group-around-callbacks group) )))

    ;; NOTE: yeah, I know this is innefficient in the
    ;;   way it re-gathers the callbacks and reverses them for each spec.
    ;;   It could at least be moved into run-group
    
    (recursive-spec-step (reverse (gather-callbacks start-group)))))

(defun run-group (spec-grp &optional (depth 0))
  (let ((failures 0) (count 0) ( pending 0) (indent (repeat-string depth "  ")))
    
    (format t "~a~a~%" indent (spec-group-caption spec-grp))
    
    (al-each ((spec-group-entries spec-grp) caption entry)

      (declare (ignore caption))
      
      (typecase entry
	(spec-group (multiple-value-bind (c f p) (run-group entry (+ depth 2))
		      (incf count    c)
		      (incf failures f)
		      (incf pending  p)))
	
	(spec (handler-case
		  (progn
		    (format t "~a  ~a~%" indent (spec-name entry))
		    (incf count)

		    (if (spec-is-empty entry)
			(progn
			  (format t "~a  : pending~%" indent)
			  (incf pending))
			
			(progn
			  (invoke-spec entry spec-grp)
		    
			  (format t "~a  : passed~%" indent))))
		
		(spec-failed (failure)
		  (incf failures)
		  (format t "~a  : failed '~a'~%"
			  indent
			  (spec-failed-message failure)))))))

    (values count failures pending)))
  
(defun run-all ()
  (let ((count 0) (failures 0) (pending 0))

    (al-each (*spec-group-root* caption entry)
      (declare (ignore caption))
      (multiple-value-bind (c f p) (run-group entry)
	    (incf count    c)
	    (incf failures f)
	    (incf pending  p)))
	
    (format t "done. ~d specs, ~d failed, ~d pending~%" count failures pending)))
