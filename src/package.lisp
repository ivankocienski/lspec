(defpackage :lspec
  (:use :cl)
  (:export :clear-specs
	   :list-specs
	   :count-specs
	   
	   :run-all
	   :run-select
	   
	   :specify

	   :expect
	   :list-expectations
	   :defexpactation


	   ;; these are also defined in the context of specify macro
	   ;;:it
	   ;;:context
	   ;;:around-each
	   ;;:yield
	   ))


(declaim (optimize (debug 3) (safety 3)))
