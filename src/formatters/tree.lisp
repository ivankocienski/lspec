(in-package :lspec)

(defclass tree-formatter (output-formatter)
  ((indent :accessor tree-formatter-indent
	   :initform 0)
   
   (indent-string :initform "")
   
   (indent-delta :initform 2
		 :initarg delta)))

(defmethod formatter-start-run ((this tree-formatter))
  (call-next-method)
  )

(defmethod formatter-end-run ((this tree-formatter))
  (call-next-method)
  )

(defmethod formatter-start-group ((this tree-formatter) group)
  (call-next-method)
  )

(defmethod formatter-stop-group ((this tree-formatter))
  (call-next-method)
  )

(defmethod formatter-start-spec ((this tree-formatter) group)
  (call-next-method)
  )

(defmethod formatter-end-spec ((this tree-formatter) result message)
  (call-next-method)
  )

(defmethod formatter-report ((this tree-formatter))
  (call-next-method))
