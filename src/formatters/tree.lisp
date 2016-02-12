(in-package :lspec)

(defclass tree-formatter (output-formatter)
  ((indent :accessor tree-formatter-indent
	   :initform 0)
   
   (indent-string :initform "")
   
   (indent-delta :initform 2
		 :initarg delta)))

(defmethod formatter-start-run ((this tree-formatter))
  (call-next-method))

(defmethod formatter-end-run ((this tree-formatter))
  (call-next-method))

(defmethod formatter-start-group ((this tree-formatter) group)
  (call-next-method)
  (with-slots (indent
	       indent-string
	       indent-delta) this

    (format t "~a~a~%" indent-string (spec-group-caption group))
    (incf indent indent-delta)
    (setf indent-string (repeat-string indent " ")))
  )

(defmethod formatter-stop-group ((this tree-formatter))
  (call-next-method)
  (with-slots (indent
	       indent-string
	       indent-delta) this

    (decf indent indent-delta)
    (setf indent-string (repeat-string indent " "))))

(defmethod formatter-start-spec ((this tree-formatter) spec)
  (call-next-method)
  (with-slots (indent-string) this
    (format t "~a>  ~a~%" indent-string (spec-name spec))))

(defmethod formatter-end-spec ((this tree-formatter) result message)
  (call-next-method)
  (with-slots (indent-string) this
    (format t "~a>    : ~a ~a~%"
	    indent-string
	    (cond ((eq result +SPEC-RUN-FAIL+) "FAIL")
		  ((eq result +SPEC-RUN-PENDING+) "pending")
		  ((eq result +SPEC-RUN-PASS+) "okay")
		  (t "maiyunnow?"))
	    (if (or (eq result +SPEC-RUN-PENDING+)
		    (eq result +SPEC-RUN-FAIL+))
		message
		""))))

(defmethod formatter-report ((this tree-formatter))
  (call-next-method))
