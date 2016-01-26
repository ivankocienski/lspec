(defpackage :lspec
  (:use :cl)
  (:export :expect
	   :it))


(declaim (optimize (debug 3) (safety 3)))
