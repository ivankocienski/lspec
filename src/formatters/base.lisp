(in-package :lspec)

(defclass output-formatter ()
  ((name :accessor output-formatter-name
	 :initform ""
	 :initarg name)
   
   (description :accessor output-formatter-description
		:initform ""
		:initarg :desc)
   
   (start-time :accessor output-formatter-start-time
	       :initform nil)

   (spec-count :initform 0)
   
   (spec-pass-count :initform 0)
   
   (spec-pending-count :initform 0)))


(defgeneric formatter-start-run (this))
(defgeneric formatter-end-run (this))

(defgeneric formatter-start-group (this group))
(defgeneric formatter-end-group (this))

(defgeneric formatter-start-spec (this spec))
(defgeneric formatter-end-spec (this result message))

(defgeneric formatter-report (this))

;; methods

(defmethod formatter-start-run (this))
(defmethod formatter-end-run (this))

(defmethod formatter-start-group (this group))
(defmethod formatter-end-group (this))

(defmethod formatter-start-spec (this spec))
(defmethod formatter-end-spec (this result message))

(defmethod formatter-report (this))

(defconstant +SPEC-RUN-UNDEFINED+ -1)
(defconstant +SPEC-RUN-PASS+ 0)
(defconstant +SPEC-RUN-FAIL+ 1)
(defconstant +SPEC-RUN-PENDING+ 2)

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

(defmacro with-formatter-run ((formatter) &body body)
  `(unwind-protect
	(progn
	  (formatter-start-run ,formatter)
	  ,@body)
     (formatter-end-run ,formatter)))
  
