
(asdf:defsystem #:lspec
  :description "spec type thing for lisp"
  :author "Me"
  :license "MIT"
  :serial t
  :components ((:module "src"
			:components ((:file "package")
				     (:file "lisp")
				     (:module "formatters"
					      :components ((:file "base")
							   (:file "dot")
							   (:file "tree")))
				     (:file "expectations")
				     (:file "spec-group")
				     (:file "spec")
				     (:file "main")))))

(asdf:defsystem #:lspec-example
  :description "Example code for lspec"
  :author "Me"
  :license "MIT"
  :serial t
  :depends-on (:lspec)
  :components ((:module "examples"
			:components ((:file "00-basic-examples")))))
