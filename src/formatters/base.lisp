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

   (spec-count         :initform 0)
   ;;(spec-fail-count    :initform 0)
   ;;(spec-pending-count :initform 0)

   (pending-specs :initform nil)
   (failed-specs  :initform nil)))

;;
;; generics
;;

(defgeneric formatter-start-run (this))
(defgeneric formatter-end-run (this))

(defgeneric formatter-start-group (this group))
(defgeneric formatter-end-group (this))

(defgeneric formatter-start-spec (this spec))
(defgeneric formatter-end-spec (this spec result message))

(defgeneric formatter-report (this))

;;
;; methods
;;

(defmethod formatter-start-run (this)
  (with-slots (start-time
	       end-time
	       spec-count
	       pending-specs
	       failed-specs) this

    (let ((now (get-real-time)))
      (setf start-time         now
	    end-time           now
	    spec-count         0
	    pending-specs      nil
	    failed-specs       nil))))

(defmethod formatter-end-run (this)
  (with-slots (end-time) this
    (setf end-time (get-real-time))))

(defmethod formatter-start-group (this group))
(defmethod formatter-end-group (this))

(defmethod formatter-start-spec (this spec)
  (with-slots (spec-count) this
    (incf spec-count))
  )

(defmethod formatter-end-spec (this spec result message)
  (with-slots (pending-specs
	       failed-specs) this

    (if (eq result +SPEC-RUN-FAIL+)
	(setf failed-specs (cons (cons spec
				       message)
				 failed-specs)))

    (if (eq result +SPEC-RUN-PENDING+)
	(setf pending-specs (cons (cons spec
					message)
				  pending-specs)))))

(defmethod formatter-report (this)
  (with-slots (start-time
	       end-time
	       spec-count
	       pending-specs
	       failed-specs) this

    (format t "~%---------------------------------------------~%~%")

    ;; print pending specs
    (if pending-specs
	(progn
	  (format t "~%pending:~%")
	  (dolist (ps pending-specs)
	    (let ((spec    (car ps))
		  (message (cdr ps)))
	      (format t "  (~a) ~a~%"
		      (full-spec-id spec)
		      (full-spec-description spec))
	      
	      (format t "    ~s~%"
		      (if (empty? message)
			  "[is pending]"
			  message))	      
	      ))))

    ;; print failed specs
    (if failed-specs
	(progn
	  (format t "~%failed:~%")
	  (dolist (fs failed-specs)
	    (let ((spec    (car fs))
		  (message (cdr fs)))
	      
	      (format t "  (~a) ~a~%"
		      (full-spec-id spec)
		      (full-spec-description spec))
	      
	      (format t "    ~s~%"
		      (if (empty? message)
			  "[NO FAILURE MESSAGE!]"
			  message))
	      ))))
	  

    ;; summary
    (format t "~%results: ~d specs, ~d failures, ~d pending~%"
	    spec-count
	    (length failed-specs)
	    (length pending-specs))

    (format t "~%done. ~fs~%"
	    (- end-time start-time))))

	    

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
	   
	   (formatter-end-spec ,formatter ,spec ,result-var ,result-msg-var))))))

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
