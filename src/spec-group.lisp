(in-package :lspec)

(defparameter *spec-group-root* nil)

(defstruct spec-group
  id
  caption
  around-callbacks
  ;; this should count all the sub entries?
  entries
  parent)

(defmethod print-object ((this spec-group) out)
  (print-unreadable-object (this out)
    (format out "SPEC-GROUP ")
    (format out "id=~d " (spec-group-id this))
    (format out "caption=~s " (spec-group-caption this))
    (format out "around-callbacks=") (print-object (spec-group-around-callbacks this) out)
    (format out " entries=") (print-object (spec-group-entries this) out)))

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
	   (package-macrolet ((yield () `(funcall next-step)))
	     ,@body))
	 (spec-group-around-callbacks ,group)))

(defmacro internal-group (caption parent &body body)
    
    `(let ((new-group (alloc-new-group ,caption ,parent)))
       
       (package-macrolet ((it (caption &body body)
		    `(internal-it ,caption new-group ,@body))
		  
		  (context (caption &body body)
		    `(internal-group ,caption new-group ,@body))
		  
		  (around-each (&body body)
		    `(build-around-each new-group ,@body)))

	 ,@body)))

(defun run-group-entries (formatter entry-list)
    
    (al-each-value (entry-list entry)

      (typecase entry
	
	;; run sub-group
	(spec-group
	 (with-formatter-group-run (formatter entry)
	   (run-group-entries formatter
			      (spec-group-entries entry))))

	;; run spec
	(spec (invoke-spec formatter entry)))))
