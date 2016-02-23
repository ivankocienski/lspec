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

	   :current-formatter
	   :set-formatter
	   :list-formatters

	   ;; these are also defined in the context of specify macro
	   ;;:it
	   ;;:context
	   ;;:around-each
	   ;;:pending
	   ;;:yield
	   ))

;; leave this in here?
(declaim (optimize (debug 3) (safety 3)))
