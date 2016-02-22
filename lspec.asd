
(asdf:defsystem #:lspec
  :description "spec type thing for lisp"
  :author "Me"
  :license "MIT"
  :serial t
  :components ((:module "src"
			:components ((:file "package")
				     (:module "formatters"
					      :components ((:file "base")
							   (:file "tree")))
				     (:file "expectations")
				     (:file "spec-group")
				     (:file "spec")
				     (:file "lisp")
				     (:file "main")))))

(asdf:defsystem #:lspec-example
  :description "Example code for lspec"
  :author "Me"
  :license "MIT"
  :serial t
  :depends-on (:lspec)
  :components ((:module "examples"
			:components ((:file "00-basic-examples")))))
