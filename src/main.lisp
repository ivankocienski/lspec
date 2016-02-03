(in-package :lspec)

;; what was i doing again?

(defstruct spec-group
  id
  caption
  around-callbacks
  ;; this should count all the sub entries?
  entries
  parent)

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

(defparameter *spec-group-root* nil)
;;(defparameter *id-for-spec* 0)
;;(defparameter *id-for-group* 0)

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
  (labels ((iterate-entry-list (caption depth id entries)

	   (format t "~a~a~a~%"
		   (repeat-string depth "  ")
		   (if id (format nil "(~a) " id) "")
		   caption)
	   
	   (al-each (entries name entry)

	     (typecase entry
	       (spec-group (progn
			     (iterate-entry-list (spec-group-caption entry)
					       (1+ depth)
					       (spec-group-id entry)
					       (spec-group-entries entry))
			     (format t "~%")))
			   

	       (spec (format t "~a  (~a) ~a~%"
			     (repeat-string depth "  ")
			     (spec-id entry)
			     name))))))

    (iterate-entry-list "ROOT" 0 nil *spec-group-root*)))

(defun build-it (name group code empty)
  (setf (spec-group-entries group)
	(al-insert (spec-group-entries group) name
		   (make-spec :name name
			      :code code
			      :is-empty empty
			      :group group
			      :id (1+ (length (spec-group-entries group)))))))

(defun alloc-new-group (caption parent)
  (format t "alloc-new-group~%")
  (let ((new-group (make-spec-group :caption caption
				    :parent parent
				    :id (1+ (length
					     (if parent
						 (spec-group-entries parent)
						 *spec-group-root*))))))

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
	 (spec-group-around-callbacks ,group)))

(defmacro internal-it (caption group &body body)
  `(macrolet ((pending (&optional (message "This spec is pending"))
		`(error 'spec-pending
			:message ,message)))
     
     (build-it ,caption ,group (lambda () ,@body) ,(null body))))

(defmacro internal-group (caption parent &body body)
    
    `(let ((new-group (alloc-new-group ,caption ,parent)))
       
       (macrolet ((it (caption &body body)
		    `(internal-it ,caption new-group ,@body))
		  
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

(defun invoke-spec (spec)
    
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
    
    (recursive-spec-step (reverse
			  (gather-callbacks
			   (spec-group spec))))))

(defun run-group (spec-grp &optional (depth 0))
  (declare (ignore spec-grp depth)))

(defun run-group-entries (entry-list depth)
    (let ((failures 0) (count 0) (pending 0) (indent (repeat-string depth "  ")))
    
    (al-each (entry-list caption entry)

      (declare (ignore caption))
      
      (typecase entry
	
	;; run sub-group
	(spec-group (multiple-value-bind (c f p) (run-group entry (+ depth 2))
		      (incf count    c)
		      (incf failures f)
		      (incf pending  p)))

	;; run spec
	(spec (handler-case
		  (progn
		    (format t "~a  ~a~%" indent (spec-name entry))
		    (incf count)

		    (if (spec-is-empty entry)
			(progn
			  (format t "~a  : pending~%" indent)
			  (incf pending))
			
			(progn
			  (invoke-spec entry)
		    
			  (format t "~a  : passed~%" indent))))
		
		(spec-failed (failure)
		  (incf failures)
		  (format t "~a  : failed '~a'~%"
			  indent
			  (spec-failed-message failure)))

		(spec-pending (pending-condition)
		  (incf pending)
		  (format t "~a  : pending '~a'~%"
			  indent
			  (spec-pending-message pending-condition)))))))

    (values count failures pending)))

(defun run-group (spec-grp &optional (depth 0))
  (let ((indent (repeat-string depth "  ")))
    
    (format t "~a~a~%" indent (spec-group-caption spec-grp))

    (multiple-value-bind (c f p) (run-group-entries (spec-group-entries spec-grp) depth)
      (values c f p))))
						    

;;(defun run-group (spec-grp &optional (depth 0))
;;  (let ((failures 0) (count 0) (pending 0) (indent (repeat-string depth "  ")))
    
;;    (format t "~a~a~%" indent (spec-group-caption spec-grp))

;;    (al-each ((spec-group-entries spec-grp) caption entry)

;;      (declare (ignore caption))
      
;;      (typecase entry
;;	(spec-group (multiple-value-bind (c f p) (run-group entry (+ depth 2))
;;		      (incf count    c)
;;		      (incf failures f)
;;		      (incf pending  p)))
	
;;	(spec (handler-case
;;		  (progn
;;		    (format t "~a  ~a~%" indent (spec-name entry))
;;		    (incf count)

;;		    (if (spec-is-empty entry)
;;			(progn
;;			  (format t "~a  : pending~%" indent)
;;			  (incf pending))
			
;;			(progn
;;			  (invoke-spec entry)
;;		    
;;			  (format t "~a  : passed~%" indent))))
		
;;		(spec-failed (failure)
;;		  (incf failures)
;;		  (format t "~a  : failed '~a'~%"
;;			  indent
;;			  (spec-failed-message failure)))

;;		(spec-pending (pending-condition)
;;		  (incf pending)
;;		  (format t "~a  : pending '~a'~%"
;;			  indent
;;			  (spec-pending-message pending-condition)))))))

;;    (values count failures pending)))
  
(defun run-all ()
  (let ((count 0) (failures 0) (pending 0))

    (al-each (*spec-group-root* caption entry)
      (declare (ignore caption))
      (multiple-value-bind (c f p) (run-group entry)
	    (incf count    c)
	    (incf failures f)
	    (incf pending  p)))
	
    (format t "done. ~d specs, ~d failed, ~d pending~%" count failures pending)))

(defun run-select (filter-string)
  (let ((path  (mapcar (lambda (s) (parse-integer s)) (split-by-char filter-string #\.))))

    (labels ((find-entry (entry-list id)
	     (al-each (entry-list name entry)
	       (declare (ignore name))
	       (let ((entry-id (typecase entry
				 (spec (spec-id entry))
				 (spec-group (spec-group-id entry)))))
		 (if (eq entry-id id)
		     (return-from find-entry entry))))
	       nil)

	     (find-by-descent (id-list entries)
	       (if id-list
		   
		   (if entries
		       (let ((found (find-entry entries (car id-list))))
			 (if found
			     (if (spec-group-p found)
				 (find-by-descent (cdr id-list)
						  (spec-group-entries found))
				 found))))
		   entries))
	     )

      (let ((found (find-by-descent path
				    *spec-group-root*)))

	(typecase found
	  (spec
	   (invoke-spec found)) 
	  
	  (list
	   (run-group-entries found 0))
	  
	  (t (format t "didn't find a thing~%")))))))
