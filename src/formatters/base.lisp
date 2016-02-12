(in-package :lspec)

(defconstant +SPEC-RUN-UNDEFINED+ -1)
(defconstant +SPEC-RUN-PASS+ 0)
(defconstant +SPEC-RUN-FAIL+ 1)
(defconstant +SPEC-RUN-PENDING+ 2)

(defclass output-formatter ()
  ((name :accessor output-formatter-name
	 :initform ""
	 :initarg name)
   
   (description :accessor output-formatter-description
		:initform ""
		:initarg :desc)
   
   (start-time :accessor output-formatter-start-time
	       :initform nil)
   
   (end-time :accessor output-formatter-end-time
	       :initform nil)

   (spec-count :initform 0)
   
   (spec-fail-count :initform 0)
   
   (spec-pending-count :initform 0)))

;;
;; generics
;;

(defgeneric formatter-start-run (this))
(defgeneric formatter-end-run (this))

(defgeneric formatter-start-group (this group))
(defgeneric formatter-end-group (this))

(defgeneric formatter-start-spec (this spec))
(defgeneric formatter-end-spec (this result message))

(defgeneric formatter-report (this))

;;
;; methods
;;

(defmethod formatter-start-run (this)
  (with-slots (start-time
	       spec-count
	       spec-fail-count
	       spec-pending-count
	       end-time) this

    (let ((now (get-real-time)))
      (setf start-time         now
	    spec-count         0
	    spec-fail-count    0
	    spec-pending-count 0
	    end-time           now))))

(defmethod formatter-end-run (this)
  (with-slots (end-time) this
    (setf end-time (get-real-time))))

(defmethod formatter-start-group (this group))
(defmethod formatter-end-group (this))

(defmethod formatter-start-spec (this spec)
  (with-slots (spec-count) this
    (incf spec-count))
  )

(defmethod formatter-end-spec (this result message)
  (with-slots (spec-fail-count
	       spec-pending-count) this

    (if (eq result +SPEC-RUN-FAIL+)
	(incf spec-fail-count))

    (if (eq result +SPEC-RUN-PENDING+)
	(incf spec-pending-count)))
  )

(defmethod formatter-report (this)
  (with-slots (start-time
	       end-time
	       spec-count
	       spec-fail-count
	       spec-pending-count) this
    
    (format t "~%done. ~fs~%"
	    (- end-time start-time))
    
    (format t "results: ~d specs, ~d failures, ~d pending~%"
	    spec-count
	    spec-fail-count
	    spec-pending-count)))
	    

;;
;; support
;;

(defmacro with-formatter-spec-run ((formatter spec) &body body)
  (let ((result-var (gensym "result"))
	(result-msg-var (gensym "result-var-message")))
    
    `(let ((,result-var +SPEC-RUN-UNDEFINED+)
	   (,result-msg-var ""))
       
       (macrolet ((set-spec-result (r &optional (msg "")) `(setf ,',result-var ,r
								 ,',result-msg-var ,msg)))
	 (unwind-protect
	      (progn
		(formatter-start-spec ,formatter ,spec)
		,@body)
	   
	   (formatter-end-spec ,formatter ,result-var ,result-msg-var))))))

(defmacro with-formatter-group-run ((formatter group) &body body)
  `(unwind-protect
	(progn
	  (formatter-start-group ,formatter ,group)
	  ,@body)
     (formatter-end-group ,formatter)))

(defun make-formatter (name)
  (cond
    ((eq name :tree) (make-instance 'tree-formatter))
    (t (error (format nil "Could not find '~s' as formatter" name)))))

(defmacro with-formatter-run ((formatter-var) &body body)
  `(unwind-protect
	(progn
	  (formatter-start-run ,formatter-var)
	  ,@body)
     (formatter-end-run ,formatter-var)))
  
(defmacro with-named-formatter ((formatter-var formatter-name) &body body)
  `(let ((,formatter-var (make-formatter ,formatter-name)))
     ,@body))
