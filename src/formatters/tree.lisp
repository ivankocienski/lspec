(in-package :lspec)

(defclass tree-formatter (output-formatter)
  ((indent :accessor tree-formatter-indent
	   :initform 0)
   
   (indent-string :initform "")
   
   (indent-delta :initform 2
		 :initarg delta)))

(defmethod formatter-start-run ((this tree-formatter))
  )

(defmethod formatter-end-run ((this tree-formatter))
  )

(defmethod formatter-start-group ((this tree-formatter) group)
  )

(defmethod formatter-stop-group ((this tree-formatter))
  )

(defmethod formatter-start-spec ((this tree-formatter) group)
  )

(defmethod formatter-end-spec ((this tree-formatter) result message)
  )

(defmethod formatter-report ((this tree-formatter))
  )
