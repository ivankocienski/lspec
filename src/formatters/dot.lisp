(in-package :lspec)

(defclass dot-formatter (output-formatter)
  ())

(defmethod formatter-start-run ((this dot-formatter))
  (call-next-method))

(defmethod formatter-end-run ((this dot-formatter))
  (call-next-method))

(defmethod formatter-start-group ((this dot-formatter) group)
  (call-next-method))

(defmethod formatter-stop-group ((this dot-formatter))
  (call-next-method))

(defmethod formatter-start-spec ((this dot-formatter) spec)
  (call-next-method))

(defmethod formatter-end-spec ((this dot-formatter) spec result message)
  (call-next-method)
  (print (cond ((eq result +SPEC-RUN-FAIL+) "F")
	       ((eq result +SPEC-RUN-PENDING+) "P")
	       ((eq result +SPEC-RUN-PASS+) ".")
	       (t "?")))
  (finish-output))

(defmethod formatter-report ((this dot-formatter))
  (call-next-method))
